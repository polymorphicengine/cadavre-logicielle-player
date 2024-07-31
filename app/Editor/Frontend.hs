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
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)
import System.Environment (getExecutablePath)
import System.FilePath (dropFileName)

frontend :: Window -> UI Element
frontend win = do
  void $ return win # set title "cadavre-logicielle"

  UI.addStyleSheet win "tidal.css"
  UI.addStyleSheet win "theme.css"

  setCallBufferMode NoBuffering

  editor <-
    UI.div
      #. "main"
      #+ [UI.textarea # set UI.id_ "editor"]
      # set UI.style [("flex-grow", "8")]

  container <-
    UI.div
      # set UI.id_ "container"
      #. "flex-container CodeMirror cm-s-tomorrow-night-eighties"

  body <- UI.getBody win # set UI.style [("background-color", "black")]

  void $
    element body
      #+ [ element container
             #+ [ element editor,
                  outputWrapper
                ]
         ]
  return editor

tidalSettings :: UI Element
tidalSettings = do
  execPath <- liftIO $ dropFileName <$> getExecutablePath
  tidalKeys <- liftIO $ readFile $ execPath ++ "static/tidalConfig.js"
  mkElement "script" # set UI.text tidalKeys

outputWrapper :: UI Element
outputWrapper =
  UI.div
    #+ [ UI.pre
           # set UI.id_ "output"
           #. "outputBox"
           # set style [("font-size", "3vh")]
       ]

fileInput :: UI Element
fileInput =
  UI.input
    # set UI.id_ "fileInput"
    # set UI.type_ "file"
    # set style [("display", "none")]