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
  rMV <- gets sRange
  line <- liftUI $ getCursorLine cm
  contents <- liftUI $ getValue cm
  let blockMaybe = case mode of
        EvalLine -> getLineContent line (linesNum contents)
        EvalBlock -> getBlock line $ getBlocks contents
  case blockMaybe of
    Nothing -> return NoCommand
    Just b -> do
      liftIO $ putMVar rMV (bStart b, bEnd b)
      case runParser (bContent b) of
        Left err -> error $ show err
        Right c -> return c

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
  rangeMV <- gets sRange
  (st, end) <- liftIO $ takeMVar rangeMV
  cm <- liftUI getCodeMirror
  liftUI $ flashSuccess cm st end

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
act (Just (Message "/ok" [])) = do
  rMV <- gets sRange
  (st, end) <- liftIO $ takeMVar rMV
  cm <- liftUI getCodeMirror
  liftUI $ flashSuccess cm st end
act (Just (Message "/ok" [AsciiString x])) = do
  rMV <- gets sRange
  (st, end) <- liftIO $ takeMVar rMV
  liftUI $ addMessage (ascii_to_string x)
  cm <- liftUI getCodeMirror
  liftUI $ flashSuccess cm st end
act (Just (Message "/error" [AsciiString e])) = do
  rMV <- gets sRange
  (st, end) <- liftIO $ takeMVar rMV
  liftUI $ addMessage (ascii_to_string e)
  cm <- liftUI getCodeMirror
  liftUI $ flashError cm st end
act (Just m) = liftUI $ addMessage ("Unhandeled message: " ++ show m)
act Nothing = liftUI $ addMessage "Not a message?"
