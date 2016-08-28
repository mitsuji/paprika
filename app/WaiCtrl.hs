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
import Control.Monad (forever,mzero)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import qualified Data.Set as Set

import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Data.Word8 (_question)

import Data.Maybe(mapMaybe)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Binary (Binary)

import Data.Aeson.Types (ToJSON,toJSON,object,(.=),FromJSON,(.:))
import qualified Data.Aeson as AE

import qualified Paprikax as PX




main :: IO ()
main = do
  PX.stop
-- [TODO] reset left arm
-- [TODO] reset right arm
  node <- createTransport >>= (\t -> newLocalNode t initRemoteTable)
  cmpid <- forkProcess node $ ctrlManagerProcess (Set.empty,CtrlCommandStop,(0,0))
  host:port:_ <- getArgs
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (wsRouterApp node cmpid) staticApp


staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
--    settings = Static.defaultWebAppSettings "static/ctrl"
    settings = Static.embeddedSettings $(embedDir "static/ctrl")
    indices = fromJust $ toPieces ["default.htm"] -- default content


wsRouterApp :: LocalNode -> ProcessId -> WS.ServerApp
wsRouterApp node cmpid pconn
  | ("/default" == path) = ctrlApp node cmpid pconn
  | otherwise = WS.rejectRequest pconn "endpoint not found"
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    path = BS.takeWhile (/=_question) requestPath


ctrlApp :: LocalNode -> ProcessId -> WS.ServerApp
ctrlApp node cmpid pconn = do
  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30
  
  cpid <- forkProcess node $ ctrlProcess conn
  runProcess node $ do
    send cmpid (CMMRegistCtrl cpid)
    send cmpid (CMMQueryCommand cpid)
  loop conn cpid `catch` onError cpid
  where
    loop :: WS.Connection -> ProcessId -> IO ()
    loop conn cpid = do
      msg <- WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close _ _) -> onClose cpid

        WS.DataMessage (WS.Text lbs) -> do
          case AE.decode lbs :: Maybe CtrlManagerMsg of
            Nothing -> return ()
            Just cmd -> runProcess node $ send cmpid cmd
          loop conn cpid
        _ -> loop conn cpid
        
    onError :: ProcessId -> WS.ConnectionException -> IO ()
    onError cpid _ = onClose cpid

    onClose cpid =
      runProcess node $ do
        send cmpid (CMMUnregistCtrl cpid)
        send cpid CMClose
      
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = BS.drop 1 $ BS.dropWhile (/=_question) requestPath
    name = T.unpack $ decodeUtf8 $ H.urlDecode True query
    



data CtrlCommand = CtrlCommandStop
                 | CtrlCommandForward
                 | CtrlCommandBackward
                 | CtrlCommandForwardLeft
                 | CtrlCommandForwardRight
                 | CtrlCommandBackwardLeft
                 | CtrlCommandBackwardRight
                 | CtrlCommandTurnLeft
                 | CtrlCommandTurnRight
                 | CtrlCommandLeftArm PX.ArmLevel
                 | CtrlCommandRightArm PX.ArmLevel
                 deriving (Generic,Typeable)

instance Show CtrlCommand where
  show CtrlCommandStop          = "s"
  show CtrlCommandForward       = "f"
  show CtrlCommandBackward      = "b"
  show CtrlCommandForwardLeft   = "fl"
  show CtrlCommandForwardRight  = "fr"
  show CtrlCommandBackwardLeft  = "bl"
  show CtrlCommandBackwardRight = "br"
  show CtrlCommandTurnLeft      = "tl"
  show CtrlCommandTurnRight     = "tr"
  show (CtrlCommandLeftArm _)   = "al"
  show (CtrlCommandRightArm _)  = "ar"

instance Read CtrlCommand where
  readsPrec _ =  mapMaybe t' <$> lex
    where
      t' :: (String,String) -> Maybe (CtrlCommand,String)
      t' (p,r) = t p >>= (\p' -> return (p',r))
        
      t :: String -> Maybe CtrlCommand
      t s = case s of
        "s"  -> Just CtrlCommandStop
        "f"  -> Just CtrlCommandForward
        "b"  -> Just CtrlCommandBackward 
        "fl" -> Just CtrlCommandForwardLeft
        "fr" -> Just CtrlCommandForwardRight
        "bl" -> Just CtrlCommandBackwardLeft
        "br" -> Just CtrlCommandBackwardRight
        "tl" -> Just CtrlCommandTurnLeft
        "tr" -> Just CtrlCommandTurnRight
        _    -> Nothing

instance Binary CtrlCommand

instance FromJSON CtrlCommand where
  parseJSON (AE.String s) = return $ read $ T.unpack s
  parseJSON (AE.Object o) = do
    cmd <- T.unpack <$> o .: "name"
    case cmd of
      "al" -> CtrlCommandLeftArm  <$> o .: "level"
      "ar" -> CtrlCommandRightArm <$> o .: "level"
                            
  parseJSON _ = mzero


type CtrlManagerState = (Set.Set ProcessId,CtrlCommand,(PX.ArmLevel,PX.ArmLevel))

data CtrlManagerMsg = CMMRegistCtrl ProcessId
                    | CMMUnregistCtrl ProcessId
                    | CMMSetCommand CtrlCommand
                    | CMMQueryCommand ProcessId
                    deriving (Show,Generic,Typeable)
                        
instance Binary CtrlManagerMsg

instance FromJSON CtrlManagerMsg where
  parseJSON v@(AE.Object o) = do
    cmd <- T.unpack <$> o .: "name"
    case cmd of
      "al" -> CMMSetCommand <$> AE.parseJSON v
      "ar" -> CMMSetCommand <$> AE.parseJSON v
      otherwise -> CMMSetCommand <$> o .: "name"
        
  parseJSON _ = mzero
  

ctrlManagerProcess :: CtrlManagerState -> Process ()
ctrlManagerProcess state = do
  state' <- receiveWait [match (p state)]
  ctrlManagerProcess state'
    where
      p :: CtrlManagerState -> CtrlManagerMsg -> Process CtrlManagerState
      
      p (cs,cmd,arm) (CMMRegistCtrl cpid) = return $ (Set.insert cpid cs,cmd,arm)
      
      p (cs,cmd,arm) (CMMUnregistCtrl cpid) = return $ (Set.delete cpid cs,cmd,arm)
                     
      p (cs,cmd,(_,ar)) (CMMSetCommand cmd'@(CtrlCommandLeftArm lev)) = do
        -- [TODO] left arm
        mapM_ (\pid -> send pid $ CMCommand cmd') $ Set.toList cs
        return $ (cs,cmd,(lev,ar))
  
      p (cs,cmd,(al,_)) (CMMSetCommand cmd'@(CtrlCommandRightArm lev)) = do
        -- [TODO] right arm
        mapM_ (\pid -> send pid $ CMCommand cmd') $ Set.toList cs
        return $ (cs,cmd,(al,lev))
  
      p (cs,_,arm) (CMMSetCommand cmd) = do
        case cmd of
          CtrlCommandStop          -> liftIO PX.stop
          CtrlCommandForward       -> liftIO PX.forward
          CtrlCommandBackward      -> liftIO PX.backward
          CtrlCommandForwardLeft   -> liftIO PX.forwardLeft
          CtrlCommandForwardRight  -> liftIO PX.forwardRight
          CtrlCommandBackwardLeft  -> liftIO PX.backwardLeft
          CtrlCommandBackwardRight -> liftIO PX.backwardRight
          CtrlCommandTurnLeft      -> liftIO PX.turnLeft
          CtrlCommandTurnRight     -> liftIO PX.turnRight
        mapM_ (\pid -> send pid $ CMCommand cmd) $ Set.toList cs
        return $ (cs,cmd,arm)
  
      p state@(cs,cmd,(al,ar)) (CMMQueryCommand cpid) = do
        send cpid $ CMCommand cmd
        send cpid $ CMCommand $ CtrlCommandLeftArm al
        send cpid $ CMCommand $ CtrlCommandRightArm ar
        return state
          
  
  
data CtrlMsg = CMCommand CtrlCommand
             | CMClose
             deriving (Show,Generic,Typeable)
                      
instance Binary CtrlMsg

instance ToJSON CtrlMsg where
  toJSON (CMCommand cmd@(CtrlCommandLeftArm lev)) =
    object ["type" .= ("command" :: String), "name" .= show cmd, "level" .= lev]
    
  toJSON (CMCommand cmd@(CtrlCommandRightArm lev)) =
    object ["type" .= ("command" :: String), "name" .= show cmd, "level" .= lev ]
    
  toJSON (CMCommand cmd) =
    object ["type" .= ("command" :: String), "name" .= show cmd ]

ctrlProcess :: WS.Connection -> Process ()    
ctrlProcess conn = forever $ receiveWait [match (p conn)]
  where
    p :: WS.Connection -> CtrlMsg -> Process ()
    
    p conn msg@(CMCommand _ ) = liftIO $ WS.sendTextData conn $ AE.encode msg

    p conn CMClose = terminate 


