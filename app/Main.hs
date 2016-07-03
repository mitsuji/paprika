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
import Data.Set as Set

import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Data.Word8 (_question)

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Binary (Binary)

import System.Huckleberry.Linux.Gpio
import System.Huckleberry.Linux.Pwm



data ViewerManagerMsg = VMMRegistViewer ProcessId
                      | VMMUnregistViewer ProcessId
                      | VMMFreq Int Int Int
                      | VMMMembers Int Int
                      deriving (Show,Generic,Typeable)
                        
instance Binary ViewerManagerMsg


data ViewerMsg = VMFreq Int Int Int
               | VMMembers Int Int
               | VMClose
               deriving (Show,Generic,Typeable)
                        
instance Binary ViewerMsg



data ReporterManagerMsg = RMMRegistReporter ProcessId
                        | RMMUnregistReporter ProcessId
                        | RMMCountUp ProcessId
                        | RMMRequestFreq Int
                        deriving (Show,Generic,Typeable)
                                 
instance Binary ReporterManagerMsg
         
         
data ReporterMsg = RMCountUp
                 | RMIsLeft Bool
                 | RMClose
                 deriving (Show,Generic,Typeable)
                        
instance Binary ReporterMsg




main :: IO ()
main = do
  node <- createTransport >>= (\t -> newLocalNode t initRemoteTable)
  vmpid <- forkProcess node $ viewerManagerProcess Set.empty
  rmpid <- forkProcess node $ reporterManagerProcess (RMS ([],[]) (Set.empty,Set.empty)) vmpid
  tpid  <- forkProcess node $ timerProcess rmpid
  host:port:_ <- getArgs
  return ()
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (wsRouterApp node vmpid rmpid) staticApp



staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.defaultWebAppSettings "static"
--    settings = Static.embeddedSettings $(embedDir "static")
    indices = fromJust $ toPieces ["viewer.htm"] -- default content


wsRouterApp :: LocalNode -> ProcessId -> ProcessId -> WS.ServerApp
wsRouterApp node vmpid rmpid pconn
  | ("/viewer"   == path) = viewerApp node vmpid pconn
  | ("/reporter" == path) = reporterApp node rmpid pconn
  | otherwise = WS.rejectRequest pconn "endpoint not found"
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    path = BS.takeWhile (/=_question) requestPath


viewerApp :: LocalNode -> ProcessId -> WS.ServerApp
viewerApp node vmpid pconn = do
  conn <- WS.acceptRequest pconn
  vpid <- forkProcess node $ viewerProcess conn vmpid
  liftIO $ runProcess node $ send vmpid (VMMRegistViewer vpid)
  loop conn vpid
  where
    loop :: WS.Connection -> ProcessId -> IO ()
    loop conn vpid = do
      msg <- liftIO $ WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> do
          liftIO $ runProcess node $ send vmpid (VMMUnregistViewer vpid)
          liftIO $ runProcess node $ send vpid VMClose
          liftIO $ putStrLn "viewer close complete!!"
        _ -> loop conn vpid
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = BS.drop 1 $ BS.dropWhile (/=_question) requestPath
    name = T.unpack $ decodeUtf8 $ H.urlDecode True query  


reporterApp :: LocalNode -> ProcessId -> WS.ServerApp
reporterApp node rmpid pconn = do
  conn <- WS.acceptRequest pconn
  rpid <- forkProcess node $ reporterProcess conn rmpid
  liftIO $ runProcess node $ send rmpid (RMMRegistReporter rpid)
  loop conn rpid
  where
    loop :: WS.Connection -> ProcessId -> IO ()
    loop conn rpid = do
      msg <- liftIO $ WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> do
          liftIO $ runProcess node $ send rmpid (RMMUnregistReporter rpid)
          liftIO $ runProcess node $ send rpid RMClose
          liftIO $ putStrLn "reporter close complete!!"
        WS.DataMessage (WS.Text lbs) -> do
--          tellString cpid $ T.unpack $ WS.fromLazyByteString lbs
          liftIO $ runProcess node $ send rpid RMCountUp
          loop conn rpid
        _ -> loop conn rpid
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = BS.drop 1 $ BS.dropWhile (/=_question) requestPath
    name = T.unpack $ decodeUtf8 $ H.urlDecode True query  




type ViewerManagerState = Set.Set ProcessId

viewerManagerProcess :: ViewerManagerState -> Process ()
viewerManagerProcess state = do
  state' <- receiveWait [match (p state)]
  viewerManagerProcess state'
    where
      p :: ViewerManagerState -> ViewerManagerMsg -> Process ViewerManagerState
      p state (VMMRegistViewer pid)   = return $ Set.insert pid state
      p state (VMMUnregistViewer pid) = return $ Set.delete pid state
      p state (VMMFreq lf rf ib) = do
        mapM_ (\pid -> send pid $ VMFreq lf rf ib) $ Set.toList state
        -- [TODO] do IOT things
        return state
      p state (VMMMembers lm rm) = do
        mapM_ (\pid -> send pid $ VMMembers lm rm) $ Set.toList state
        return state


viewerProcess :: WS.Connection -> ProcessId -> Process ()    
viewerProcess conn vmpid = forever $ receiveWait [match (p conn vmpid)]
  where
    p :: WS.Connection -> ProcessId -> ViewerMsg -> Process ()
    p conn _ msg@(VMFreq _ _ _) =
      let
        txt = T.pack $ show msg -- [TODO] jsonize
      in
       liftIO $ WS.sendTextData conn txt
    p conn _ msg@(VMMembers _ _) =
      let
        txt = T.pack $ show msg -- [TODO] jsonize
      in
       liftIO $ WS.sendTextData conn txt

    p conn _ VMClose = terminate



data ReporterManagerState = RMS ([Int],[Int]) (Set.Set ProcessId, Set.Set ProcessId)

reporterManagerProcess :: ReporterManagerState -> ProcessId -> Process ()
reporterManagerProcess state vmpid = do
  state' <- receiveWait [match (p state vmpid)]
  reporterManagerProcess state' vmpid
    where
      p :: ReporterManagerState -> ProcessId -> ReporterManagerMsg -> Process ReporterManagerState
      p (RMS cs (lms,rms)) _ (RMMRegistReporter rpid) =
        if Set.size lms < Set.size rms
        then do
          let lms' = Set.insert rpid lms
          send rpid $ RMIsLeft True
          send vmpid $ VMMMembers (Set.size lms') (Set.size rms)
          return $ RMS cs (lms',rms)
        else do
          let rms' = Set.insert rpid rms
          send rpid $ RMIsLeft False
          send vmpid $ VMMMembers (Set.size lms) (Set.size rms')
          return $ RMS cs (lms,rms')
                     
      p (RMS cs (lms,rms)) _ (RMMUnregistReporter rpid) =
        if Set.member rpid lms
        then do
          let lms' = Set.delete rpid lms
          send vmpid $ VMMMembers (Set.size lms') (Set.size rms)
          return $ RMS cs (lms',rms)
        else do
          let rms' = Set.delete rpid rms
          send vmpid $ VMMMembers (Set.size lms) (Set.size rms')
          return $ RMS cs (lms,rms')
                     
      p (RMS (lcs,rcs) ms@(lms,_)) _ (RMMCountUp rpid) =
        if Set.member rpid lms
        then return $ RMS (head lcs +1 : tail lcs, rcs) ms
        else return $ RMS (lcs, head rcs +1 : tail rcs) ms
             
      p (RMS (lcs,rcs) ms) vmpid (RMMRequestFreq ib) = do
        send vmpid $ VMMFreq (sum lcs) (sum rcs) ib
        return $ RMS (0 : take 4 lcs,0: take 4 rcs) ms


reporterProcess :: WS.Connection -> ProcessId -> Process ()    
reporterProcess conn rmpid = forever $ receiveWait [match (p conn rmpid)]
  where
    p :: WS.Connection -> ProcessId -> ReporterMsg -> Process ()
    p conn rmpid RMCountUp = do
      pid <- getSelfPid
      send rmpid $ RMMCountUp pid

    p conn _ msg@(RMIsLeft _) =
      let
        txt = T.pack $ show msg -- [TODO] jsonize
      in
       liftIO $ WS.sendTextData conn txt

    p conn _ RMClose = terminate -- [TODO] RMMUnregistReporter


timerProcess :: ProcessId -> Process ()
timerProcess rmpid = loop
  where
    loop = do
      let ib = 250 * 1000
      send rmpid $ RMMRequestFreq ib
      liftIO $ threadDelay ib
      loop
