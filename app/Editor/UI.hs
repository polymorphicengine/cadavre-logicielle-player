{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Editor.UI where

{-
    UI.hs - miscellanious functions for the user interface
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

-- (Stream, sPMapMV, Pattern, queryArc, Arc(..))

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Monad (void)
import Control.Monad.Catch (catch)
import Data.Time
import Foreign.JavaScript (JSObject)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (get, text, value)

getCursorLine :: (ToJS a) => a -> UI Int
getCursorLine cm = catchHaskellError 0 $ callFunction $ ffi "getCursorLine(%1)" cm

getValue :: (ToJS a) => a -> UI String
getValue cm = catchHaskellError "" $ callFunction $ ffi "%1.getValue()" cm

createHaskellFunction name fn = do
  handler <- ffiExport fn
  runFunction $ ffi ("window." ++ name ++ " = %1") handler

-- adding and removing editors

makeEditor :: String -> UI ()
makeEditor i = runFunction $ ffi $ i ++ "cm = CodeMirror.fromTextArea(document.getElementById('" ++ i ++ "'), fullSettings.editor);"

-- flashing

highlightBlock :: JSObject -> Int -> Int -> String -> UI (Maybe JSObject)
highlightBlock cm lineStart lineEnd color = catchHaskellErrorMaybe $ callFunction $ ffi "((%1).markText({line: %2, ch: 0}, {line: %3, ch: 0}, {css: %4}))" cm lineStart lineEnd color

unHighlight :: JSObject -> UI ()
unHighlight mark = runFunction $ ffi "if (typeof %1 !== 'undefined'){%1.clear()};" mark

flashSuccess :: JSObject -> Int -> Int -> UI ()
flashSuccess cm lineStart lineEnd = do
  maymark <- highlightBlock cm lineStart (lineEnd + 1) "background-color: green"
  case maymark of
    Just mark -> do
      liftIO $ threadDelay 100000
      unHighlight mark
      flushCallBuffer
    Nothing -> return ()

flashError :: JSObject -> Int -> Int -> UI ()
flashError cm lineStart lineEnd = do
  maymark <- highlightBlock cm lineStart (lineEnd + 1) "background-color: red"
  case maymark of
    Just mark -> do
      liftIO $ threadDelay 100000
      unHighlight mark
      flushCallBuffer
    Nothing -> return ()

-- setting, getting and clearing the config

setConfig :: Window -> String -> String -> IO ()
setConfig win key v = runUI win $ runFunction $ ffi ("window.electronAPI.putInStore(%1," ++ v ++ ")") key

clearConfig :: Window -> IO ()
clearConfig win = runUI win $ runFunction $ ffi "window.electronAPI.clearStore()"

mkMessage :: String -> String -> UI Element
mkMessage t m = UI.pre # set UI.text (t ++ " - " ++ m) #. "message"

addElement :: String -> String -> Element -> UI ()
addElement className containerId el = do
  win <- askWindow
  els <- getElementsByClassName win className
  mayContainer <- getElementById win containerId
  case mayContainer of
    Nothing -> return ()
    Just cont -> void $ element cont # set UI.children (el : els)

addMessage :: String -> UI ()
addMessage m = do
  t <- liftIO getZonedTime
  el <- mkMessage (showTime t) m
  addElement "message" "message-container" el

infixl 8 #@

(#@) :: UI Element -> String -> UI Element
(#@) mx s = mx # set (attr "id") s

getCodeMirror :: UI (Maybe JSObject)
getCodeMirror = catchHaskellErrorMaybe $ callFunction $ ffi "document.querySelector(\"#editor + .CodeMirror\").CodeMirror"

showTime :: ZonedTime -> String
showTime = take 8 . show . localTimeOfDay . zonedTimeToLocalTime

-- to catch javascript errors when using callFunction
catchHaskellError :: a -> UI a -> UI a
catchHaskellError x action = catch action (\(e :: SomeException) -> addMessage (show e) >> return x)

catchHaskellErrorMaybe :: UI a -> UI (Maybe a)
catchHaskellErrorMaybe action = catch (fmap Just action) (\(e :: SomeException) -> addMessage (show e) >> return Nothing)
