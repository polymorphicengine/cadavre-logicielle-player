{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Editor.Backend where

{-
    Backend.hs - Implements the interaction between the compiler-interpreter and the editor
    Copyright (C) 2023, Martin Gius

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import Control.Concurrent.MVar
import Control.Monad.State hiding (State)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Editor.Parse
import Editor.UI
import Foreign.JavaScript (JSObject)
import Graphics.UI.Threepenny.Core as C hiding (get, text)
import qualified Network.Socket as N
import Sound.Osc.Fd as O

data EvalMode
  = EvalBlock
  | EvalLine
  deriving (Eq, Show)

type Range = MVar (Int, Int)

type TableAddress = N.SockAddr

data Definition
  = Def
  { dName :: String,
    dType :: String
  }

type Definitions = [Definition]

data State
  = State
  { sLocal :: Udp,
    sRemote :: MVar TableAddress,
    sRange :: Range
  }

-- TODO : maybe replace with ReaderT?
type Game = StateT State UI

instance MonadUI Game where
  liftUI = lift . liftUI

runGame :: Game a -> State -> UI (a, State)
runGame = runStateT

evalContentAtCursor :: EvalMode -> JSObject -> Game ()
evalContentAtCursor mode cm = parseBlocks mode cm >>= actOnCommand

parseBlocks :: EvalMode -> JSObject -> Game Command
parseBlocks mode cm = do
  line <- liftUI $ getCursorLine cm
  contents <- liftUI $ getValue cm
  let blockMaybe = case mode of
        EvalLine -> getLineContent line (linesNum contents)
        EvalBlock -> getBlock line $ getBlocks contents
  case blockMaybe of
    Nothing -> return NoCommand
    Just b -> do
      case runParser (bContent b) of
        Left err -> liftUI (addMessage $ show err) >> return NoCommand
        Right c -> checkTimeout (bStart b, bEnd b) >> return c

sendMessageRemote :: Packet -> Game ()
sendMessageRemote p = do
  st <- get
  remote <- liftIO $ readMVar $ sRemote st
  liftIO $ O.sendTo (sLocal st) p remote

actOnCommand :: Command -> Game ()
actOnCommand (Statement str) = sendMessageRemote (O.p_message "/eval" [O.string str])
actOnCommand (Type str) = sendMessageRemote (O.p_message "/type" [O.string str])
actOnCommand Ping = sendMessageRemote (O.p_message "/ping" [])
actOnCommand (Definition n t c) = defAction n t c
actOnCommand NoCommand = liftUI $ addMessage "Could not parse."
actOnCommand (RemoteAddress add p) = newRemoteAddress add p

defAction :: String -> String -> String -> Game ()
defAction name t c = sendMessageRemote (O.p_message "/define" [O.string name, O.string t, O.string code, O.string def])
  where
    def = "let " ++ name ++ " = _define" ++ t ++ "\"" ++ name ++ "\""
    code = "streamSet" ++ t ++ " tidal " ++ show name ++ " $ " ++ c

newRemoteAddress :: String -> Int -> Game ()
newRemoteAddress addr port = do
  rMV <- gets sRemote
  as <- liftIO $ N.getAddrInfo Nothing (Just addr) Nothing
  let (N.SockAddrInet _ a) = N.addrAddress (head as)
      remote = N.SockAddrInet (fromIntegral port) a
  liftIO $ modifyMVar_ rMV (const $ return remote)
  liftUI $ addMessage $ "Changed address of the table to " ++ addr ++ ":" ++ show port
  successAction

checkTimeout :: (Int, Int) -> Game ()
checkTimeout x = liftIO getCurrentTime >>= timeout x

timeout :: (Int, Int) -> UTCTime -> Game ()
timeout x t = do
    rMV <- gets sRange
    success <- liftIO $ tryPutMVar rMV x
    unless
        success
        ( do
            cur <- liftIO getCurrentTime
            ( if diffUTCTime cur t >= 10
                then do
                cm <- liftUI getCodeMirror
                liftUI $ uncurry (flashError cm) x
                liftUI $ addMessage "No response from table.."
                void $ liftIO $ takeMVar rMV
                else timeout x t
            )
        )



--------------------------------------------------------
--------- acting on responses from the table -----------
--------------------------------------------------------

playingHand :: Game ()
playingHand = do
  udp <- gets sLocal
  m <- liftIO $ recvMessage udp
  act m
  playingHand

act :: Maybe O.Message -> Game ()
act (Just (Message "/ok" [])) = successAction
act (Just (Message "/ok" [AsciiString x])) = successAction >> liftUI $ addMessage (ascii_to_string x)
act (Just (Message "/error" [AsciiString e])) = errorAction >> liftUI $ addMessage (ascii_to_string e)
act (Just m) = liftUI $ addMessage ("Unhandeled message: " ++ show m)
act Nothing = liftUI $ addMessage "Not a message?"


successAction :: Game ()
successAction = do
    rMV <- gets sRange
    may <- liftIO $ tryTakeMVar rMV
    case may of
       Nothing -> return ()
       Just (st,end) -> do
                cm <- liftUI getCodeMirror
                liftUI $ flashSuccess cm st end

errorAction :: Game ()
errorAction = do
    rMV <- gets sRange
    may <- liftIO $ tryTakeMVar rMV
    case may of
       Nothing -> return ()
       Just (st,end) -> do
                cm <- liftUI getCodeMirror
                liftUI $ flashError cm st end
