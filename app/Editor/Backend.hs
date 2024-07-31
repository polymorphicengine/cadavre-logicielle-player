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

data EvalMode
  = EvalBlock
  | EvalLine
  | EvalWhole
  deriving (Eq, Show)

evalContentAtCursor :: EvalMode -> JSObject -> UI ()
evalContentAtCursor mode cm = do
  line <- getCursorLine cm
  evalContentAtLine mode cm line

evalContentAtLine :: EvalMode -> JSObject -> Int -> UI ()
evalContentAtLine mode cm line = do
  editorContent <- getValue cm
  out <- getOutputEl
  void $ element out # set UI.text editorContent
