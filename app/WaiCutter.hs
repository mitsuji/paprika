{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.String (fromString)
import System.Environment (getArgs)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as H
import qualified Network.Wai.Application.Static as Static
import Data.Maybe (fromJust)
import Data.FileEmbed (embedDir)
import WaiAppStatic.Types (toPieces)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS

import Network.Transport.InMemory (createTransport)
import Control.Distributed.Process.Node (LocalNode,newLocalNode,initRemoteTable,forkProcess,runProcess)
import Control.Distributed.Process (Process,ProcessId,send,receiveWait,match,getSelfPid,terminate)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import qualified Data.Set as Set

import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Data.Word8 (_question)

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Binary (Binary)

import Data.Aeson.Types (ToJSON,toJSON,object,(.=))
import qualified Data.Aeson as AE

import Paprikax(leftOn,leftOff,rightOn,rightOff)




main :: IO ()
main = do
  node <- createTransport >>= (\t -> newLocalNode t initRemoteTable)
  vmpid <- forkProcess node $ viewerManagerProcess Set.empty
  rmpid <- forkProcess node $ reporterManagerProcess (RMS ([],[]) (Set.empty,Set.empty) 10) vmpid
  _ <- forkProcess node $ timerProcess rmpid
  host:port:_ <- getArgs
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (wsRouterApp node vmpid rmpid) staticApp


staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
--    settings = Static.defaultWebAppSettings "static/cutter"
    settings = Static.embeddedSettings $(embedDir "static/cutter")
    indices = fromJust $ toPieces ["viewer.htm"] -- default content


wsRouterApp :: LocalNode -> ProcessId -> ProcessId -> WS.ServerApp
wsRouterApp node vmpid rmpid pconn
  | ("/viewer"   == path) = viewerApp node vmpid rmpid pconn
  | ("/reporter" == path) = reporterApp node rmpid pconn
  | otherwise = WS.rejectRequest pconn "endpoint not found"
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    path = BS.takeWhile (/=_question) requestPath


viewerApp :: LocalNode -> ProcessId -> ProcessId -> WS.ServerApp
viewerApp node vmpid rmpid pconn = do
  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30
  
  vpid <- forkProcess node $ viewerProcess conn
  runProcess node $ do
    send vmpid (VMMRegistViewer vpid)
    send rmpid (RMMQueryMembers vpid)
    send rmpid (RMMQueryThreshold vpid)
  loop conn vpid `catch` onError vpid
  where
    loop :: WS.Connection -> ProcessId -> IO ()
    loop conn vpid = do
      msg <- WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> onClose vpid

        WS.DataMessage (WS.Text lbs) -> do
          let thr = read $ T.unpack $ WS.fromLazyByteString lbs
          runProcess node $ send rmpid $ RMMSetThreshold thr
          loop conn vpid
        _ -> loop conn vpid
        
    onError :: ProcessId -> WS.ConnectionException -> IO ()
    onError vpid _ = onClose vpid

    onClose vpid =
      runProcess node $ do
        send vmpid (VMMUnregistViewer vpid)
        send vpid VMClose
      
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = BS.drop 1 $ BS.dropWhile (/=_question) requestPath
    name = T.unpack $ decodeUtf8 $ H.urlDecode True query
    


reporterApp :: LocalNode -> ProcessId -> WS.ServerApp
reporterApp node rmpid pconn = do
  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30
  
  rpid <- forkProcess node $ reporterProcess conn
  runProcess node $ send rmpid (RMMRegistReporter rpid)
  loop conn rpid `catch` onError rpid
  where
    loop :: WS.Connection -> ProcessId -> IO ()
    loop conn rpid = do
      msg <- WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> onClose rpid
            
        WS.DataMessage (WS.Text _) -> do
          runProcess node $ send rmpid $ RMMCountUp rpid
          loop conn rpid
        _ -> loop conn rpid
        
    onError :: ProcessId -> WS.ConnectionException -> IO ()
    onError rpid _ = onClose rpid
      
    onClose rpid = 
      runProcess node $ do
        send rmpid (RMMUnregistReporter rpid)
        send rpid RMClose
      
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = BS.drop 1 $ BS.dropWhile (/=_question) requestPath
    name = T.unpack $ decodeUtf8 $ H.urlDecode True query  




type ViewerManagerState = Set.Set ProcessId

data ViewerManagerMsg = VMMRegistViewer ProcessId
                      | VMMUnregistViewer ProcessId
                      | VMMFreq Int Int
                      | VMMMembers Int Int
                      | VMMThreshold Int
                      deriving (Show,Generic,Typeable)
                        
instance Binary ViewerManagerMsg

viewerManagerProcess :: ViewerManagerState -> Process ()
viewerManagerProcess state = do
  state' <- receiveWait [match (p state)]
  viewerManagerProcess state'
    where
      p :: ViewerManagerState -> ViewerManagerMsg -> Process ViewerManagerState
      
      p state (VMMRegistViewer pid)   = return $ Set.insert pid state
      
      p state (VMMUnregistViewer pid) = return $ Set.delete pid state
      
      p state (VMMFreq lf rf) = do
        mapM_ (\pid -> send pid $ VMFreq lf rf) $ Set.toList state
        return state
        
      p state (VMMMembers lm rm) = do
        mapM_ (\pid -> send pid $ VMMembers lm rm) $ Set.toList state
        return state

      p state (VMMThreshold thr) = do
        mapM_ (\pid -> send pid $ VMThreshold thr) $ Set.toList state
        return state



data ViewerMsg = VMFreq Int Int
               | VMMembers Int Int
               | VMThreshold Int
               | VMClose
               deriving (Show,Generic,Typeable)
                        
instance Binary ViewerMsg

instance ToJSON ViewerMsg where
  toJSON (VMFreq lf rf) =
    object ["type" .= ("freq" :: String)
           ,"content" .= object ["left"  .= lf
                                ,"right" .= rf
                                ]
           ]
    
  toJSON (VMMembers lm rm) =
    object ["type" .= ("members" :: String)
           ,"content" .= object ["left"  .= lm
                                ,"right" .= rm
                                ]
           ]

  toJSON (VMThreshold thr) =
    object ["type" .= ("threshold" :: String)
           ,"content" .= thr
           ]

viewerProcess :: WS.Connection -> Process ()    
viewerProcess conn = forever $ receiveWait [match (p conn)]
  where
    p :: WS.Connection -> ViewerMsg -> Process ()
    
    p conn msg@(VMFreq _ _) = liftIO $ WS.sendTextData conn $ AE.encode msg
       
    p conn msg@(VMMembers _ _) = liftIO $ WS.sendTextData conn $ AE.encode msg

    p conn msg@(VMThreshold _ ) = liftIO $ WS.sendTextData conn $ AE.encode msg

    p conn VMClose = terminate 



data ReporterManagerState = RMS ([Int],[Int]) (Set.Set ProcessId, Set.Set ProcessId) Int

data ReporterManagerMsg = RMMRegistReporter ProcessId
                        | RMMUnregistReporter ProcessId
                        | RMMCountUp ProcessId
                        | RMMQueryMembers ProcessId
                        | RMMSetThreshold Int
                        | RMMQueryThreshold ProcessId
                        | RMMRequestAggregate
                        | RMMRequestFreq
                        deriving (Show,Generic,Typeable)
                                 
instance Binary ReporterManagerMsg
         
reporterManagerProcess :: ReporterManagerState -> ProcessId -> Process ()
reporterManagerProcess state vmpid = do
  state' <- receiveWait [match (p state)]
  reporterManagerProcess state' vmpid
    where
      p :: ReporterManagerState -> ReporterManagerMsg -> Process ReporterManagerState
      
      p (RMS cs (lms,rms) thr) (RMMRegistReporter rpid) =
        if Set.size lms < Set.size rms
        then do
          let lms' = Set.insert rpid lms
          send rpid $ RMIsLeft True
          send vmpid $ VMMMembers (Set.size lms') (Set.size rms)
          return $ RMS cs (lms',rms) thr
        else do
          let rms' = Set.insert rpid rms
          send rpid $ RMIsLeft False
          send vmpid $ VMMMembers (Set.size lms) (Set.size rms')
          return $ RMS cs (lms,rms') thr
                     
      p (RMS cs (lms,rms) thr) (RMMUnregistReporter rpid) =
        if Set.member rpid lms
        then do
          let lms' = Set.delete rpid lms
          send vmpid $ VMMMembers (Set.size lms') (Set.size rms)
          return $ RMS cs (lms',rms) thr
        else do
          let rms' = Set.delete rpid rms
          send vmpid $ VMMMembers (Set.size lms) (Set.size rms')
          return $ RMS cs (lms,rms') thr
                     
      p (RMS (lcs,rcs) ms@(lms,_) thr) (RMMCountUp rpid) =
        if Set.member rpid lms
        then return $ RMS (head lcs +1 : tail lcs, rcs) ms thr
        else return $ RMS (lcs, head rcs +1 : tail rcs) ms thr
             
      p state@(RMS _ (lms,rms) _) (RMMQueryMembers vpid) = do
        send vpid $ VMMembers (Set.size lms) (Set.size rms)
        return state
        
      p (RMS cs ms _) (RMMSetThreshold thr) = do
        send vmpid $ VMMThreshold thr
        return $ RMS cs ms thr

      p state@(RMS _ _ thr) (RMMQueryThreshold vpid) = do
        send vpid $ VMThreshold thr
        return state
        
      p (RMS (lcs,rcs) ms thr) RMMRequestAggregate = do
        -- [TODO] calc correct interbal
        return $ RMS (0 : take 10 lcs,0: take 10 rcs) ms thr
        
      p state@(RMS (lcs,rcs) _ thr) RMMRequestFreq = do
        -- [TODO] calc correct interbal
        let lf = sum lcs
        let rf = sum rcs
        if thr <= lf
          then liftIO leftOn
          else liftIO leftOff
        if thr <= rf
          then liftIO rightOn
          else liftIO rightOff
        send vmpid $ VMMFreq lf rf
        return state

        



data ReporterMsg = RMIsLeft Bool
                 | RMClose
                 deriving (Show,Generic,Typeable)
                        
instance Binary ReporterMsg

instance ToJSON ReporterMsg where
  toJSON (RMIsLeft True) =
    object ["type" .= ("side" :: String), "content" .= ("left"  :: String)]
    
  toJSON (RMIsLeft False) =
    object ["type" .= ("side" :: String), "content" .= ("right" :: String)]

reporterProcess :: WS.Connection -> Process ()    
reporterProcess conn = forever $ receiveWait [match (p conn)]
  where
    p :: WS.Connection -> ReporterMsg -> Process ()
    
    p conn msg@(RMIsLeft _) = liftIO $ WS.sendTextData conn $ AE.encode msg

    p conn RMClose = terminate -- [TODO] RMMUnregistReporter



timerProcess :: ProcessId -> Process ()
timerProcess rmpid = loop 0
  where
    loop :: Int -> Process ()
    loop i = do
      send rmpid $ RMMRequestAggregate
      if i `mod` 5 == 0
        then send rmpid $ RMMRequestFreq
        else return ()
      liftIO $ threadDelay $ 100 * 1000
      loop (i+1)


