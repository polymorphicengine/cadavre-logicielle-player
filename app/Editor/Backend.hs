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
import Editor.Types
import Editor.UI
import Foreign.JavaScript (JSObject)
import Graphics.UI.Threepenny.Core as C hiding (get, text)
import qualified Network.Socket as N
import Sound.Osc.Fd as O
import qualified Sound.Osc.Transport.Fd.Udp as O

actOnCommand :: Command -> Env -> UI ()
actOnCommand (Statement str) env = sendMessageRemote (O.p_message "/eval" [utf8String str]) env
actOnCommand (Type str) env = sendMessageRemote (O.p_message "/type" [utf8String str]) env
actOnCommand Ping env = sendMessageRemote (O.p_message "/ping" []) env
actOnCommand (Definition name code) env = sendMessageRemote (O.p_message "/define" [utf8String name, utf8String code]) env
actOnCommand NoCommand _ = addMessage "Could not parse."
actOnCommand (Say x) env = sendMessageRemote (O.p_message "/say" [utf8String x]) env

evalContentAtCursor :: EvalMode -> Env -> JSObject -> UI ()
evalContentAtCursor mode env cm = parseBlocks mode cm env >>= \c -> actOnCommand c env

parseBlocks :: EvalMode -> JSObject -> Env -> UI Command
parseBlocks mode cm env = do
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
        Right c -> do
          let rMV = sRange env
          _ <- liftIO $ tryPutMVar rMV (bStart b, bEnd b)
          return c

connect :: Env -> UI ()
connect env = do
  mayname <- liftUI getName
  case mayname of
    Nothing -> return ()
    Just name -> do
      mayorb <- liftUI getOrbit
      case mayorb of
        Nothing -> return ()
        Just orb -> do
          mayadd <- liftUI getAddress
          case mayadd of
            Nothing -> return ()
            Just add -> do
              mayport <- liftUI getPort
              case mayport of
                Nothing -> return ()
                Just port -> do
                  newRemoteAddress add port env
                  sendMessageRemote (O.p_message "/sit" [utf8String name, O.int32 orb]) env

newRemoteAddress :: String -> Int -> Env -> UI ()
newRemoteAddress addr port env = do
  let rMV = sRemote env
  as <- liftIO $ N.getAddrInfo Nothing (Just addr) Nothing
  let (N.SockAddrInet _ a) = N.addrAddress (head as)
      remote = N.SockAddrInet (fromIntegral port) a
  liftIO $ modifyMVar_ rMV (const $ return remote)
  liftUI $ addMessage $ "Changed address of the table to " ++ addr ++ ":" ++ show port

sendMessageRemote :: Packet -> Env -> UI ()
sendMessageRemote p env = do
  remote <- liftIO $ readMVar $ sRemote env
  liftIO $ O.sendTo (sLocal env) p remote
