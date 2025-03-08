{-# LANGUAGE OverloadedStrings #-}

module Editor.Setup (setup) where

{-
    Setup.hs - setup of the various components of the backend
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

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, newMVar)
import Control.Monad (void)
import Editor.Act
import Editor.Backend
import Editor.Frontend
import Editor.Types
import Editor.UI
import Graphics.UI.Threepenny.Core as C hiding (defaultConfig, text)
import qualified Network.Socket as N
import Sound.Osc.Transport.Fd.Udp (udp_server)

setup :: Window -> UI ()
setup win = void $ do
  frontend win

  setupBackend
  addFileInputAndSettings
  makeEditor "editor"

setupBackend :: UI ()
setupBackend = do
  win <- askWindow
  local <- liftIO $ udp_server 2324
  remote <- defaultTableAddress

  rMV <- liftIO newEmptyMVar
  remMV <- liftIO $ newMVar remote
  let env = Env local remMV rMV
      st = ActState [] []
  _ <- liftIO $ forkIO $ runUI win $ void $ runAct (playingHand env) st

  createHaskellFunction "evalBlockAtCursor" (runUI win . evalContentAtCursor EvalBlock env)
  createHaskellFunction "evalLineAtCursor" (runUI win . evalContentAtCursor EvalLine env)
  createHaskellFunction "connect" (runUI win $ connect env)

addFileInputAndSettings :: UI ()
addFileInputAndSettings = do
  win <- askWindow
  body <- getBody win
  void $
    element body
      #+ [ fileInput,
           settings
         ]

defaultTableAddress :: UI TableAddress
defaultTableAddress = do
  addr <- liftIO $ N.getAddrInfo Nothing (Just "127.0.0.1") Nothing
  let (N.SockAddrInet _ a) = N.addrAddress (head addr)

  return $ N.SockAddrInet 2323 a
