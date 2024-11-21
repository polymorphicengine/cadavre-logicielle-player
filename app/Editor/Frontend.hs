module Editor.Frontend where

{-
    Frontend.hs - defines the html dom for the editor interface
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
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)
import System.Environment (getExecutablePath)
import System.FilePath (dropFileName)

frontend :: Window -> UI ()
frontend win = do
  void $ return win # set title "cadavre-logicielle"

  UI.addStyleSheet win "cadavre-logicielle.css"
  UI.addStyleSheet win "theme.css"

  setCallBufferMode NoBuffering

  body <- UI.getBody win

  void $ element body #+ [inputContainer, container]

settings :: UI Element
settings = do
  execPath <- liftIO $ dropFileName <$> getExecutablePath
  keys <- liftIO $ readFile $ execPath ++ "static/config.js"
  mkElement "script" # set UI.text keys

fileInput :: UI Element
fileInput =
  UI.input
    #@ "fileInput"
    # set UI.type_ "file"
    # set style [("display", "none")]

container :: UI Element
container = UI.div #. "container" #+ [rightContainer, editor, messageContainer]

editor :: UI Element
editor = UI.div #+ [UI.textarea #@ "editor"]

messageContainer :: UI Element
messageContainer = UI.div #. "message-container" #@ "message-container"

rightContainer :: UI Element
rightContainer = UI.div #. "right-container" #+ [playerContainer, definitionContainer]

playerContainer :: UI Element
playerContainer = UI.div #. "player-container" #@ "player-container"

definitionContainer :: UI Element
definitionContainer = UI.div #. "definition-container" #@ "definition-container"

inputContainer :: UI Element
inputContainer = UI.div #. "input-container" #@ "input-container" #+ [playerInput, addressInput, connectButton]

playerInput :: UI Element
playerInput = UI.div #+ [nameInput, orbitInput] #. "player-input"

nameInput :: UI Element
nameInput = UI.div #+ [UI.label # set UI.text "name:" #. "label", input "name-input"] #. "input"

orbitInput :: UI Element
orbitInput = UI.div #+ [UI.label # set UI.text "orbit:" #. "label", input "orbit-input"] #. "input"

addressInput :: UI Element
addressInput =
  UI.div
    #. "add-input"
    #+ [ UI.div #+ [UI.label # set UI.text "address:" #. "label", input "address-input" # set UI.text "localhost"] #. "input",
         UI.div #+ [UI.label # set UI.text "port:" #. "label", input "port-input" # set UI.text "2323"] #. "input"
       ]

connectButton :: UI Element
connectButton = UI.button #. "button" #@ "connect-button" # set UI.text "connect" # set (attr "onclick") "connect()"

input :: String -> UI Element
input name = UI.div #. "inputarea" #@ name # set (attr "contenteditable") "true"
