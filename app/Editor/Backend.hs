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

import Control.Monad (void)
import Editor.UI
import Foreign.JavaScript (JSObject)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)
import qualified Network.Socket as N
import Sound.Osc.Fd as O

data EvalMode
  = EvalBlock
  | EvalLine
  | EvalWhole
  deriving (Eq, Show)

evalContentAtCursor :: Udp -> EvalMode -> JSObject -> UI ()
evalContentAtCursor local mode cm = do
  line <- getCursorLine cm
  evalContentAtLine local mode cm line

evalContentAtLine :: Udp -> EvalMode -> JSObject -> Int -> UI ()
evalContentAtLine local mode cm line = do
  editorContent <- getValue cm
  out <- getOutputEl
  addr <- liftIO $ N.getAddrInfo Nothing (Just "127.0.0.1") Nothing
  let (N.SockAddrInet _ a) = N.addrAddress (head addr)
      remote = N.SockAddrInet 2323 a
  liftIO $ O.sendTo local (O.p_message "/ping" []) remote

serve :: Udp -> Element -> UI ()
serve udp out = do
  m <- liftIO $ recvMessage udp
  act out m
  serve udp out

act :: Element -> Maybe O.Message -> UI ()
act out Nothing = element out # set UI.text "Not a message?" >> return ()
act out (Just m) = element out # set UI.text ("Unhandeled message: " ++ show m) >> return ()
