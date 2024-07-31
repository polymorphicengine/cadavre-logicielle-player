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
import Control.Monad (void)
import Editor.Backend
import Editor.Frontend
import Editor.UI
import Graphics.UI.Threepenny.Core as C hiding (defaultConfig, text)
import Sound.Osc.Fd as O

setup :: Window -> UI ()
setup win = void $ do
  frontend win

  setupBackend
  addFileInputAndSettings
  makeEditor "editor"

setupBackend :: UI ()
setupBackend = do
  win <- askWindow
  local <- liftIO $ udpServer "127.0.0.1" 2324
  out <- getOutputEl
  _ <- liftIO $ forkIO $ runUI win $ serve local out

  createHaskellFunction "evalBlockAtCursor" (runUI win . evalContentAtCursor local EvalBlock)
  createHaskellFunction "evalLineAtCursor" (runUI win . evalContentAtCursor local EvalLine)
  createHaskellFunction "evalWhole" (runUI win . evalContentAtCursor local EvalWhole)

addFileInputAndSettings :: UI ()
addFileInputAndSettings = do
  win <- askWindow
  body <- getBody win
  void $
    (element body)
      #+ [ fileInput,
           tidalSettings
         ]