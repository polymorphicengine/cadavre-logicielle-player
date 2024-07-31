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
import Foreign.JavaScript (JSObject)
import Graphics.UI.Threepenny.Core as C hiding (get, text, value)

getOutputEl :: UI Element
getOutputEl = do
  win <- askWindow
  elMay <- getElementById win "output"
  case elMay of
    Nothing -> error "can't happen"
    Just el -> return el

getDisplayElV :: UI Element
getDisplayElV = do
  win <- askWindow
  elMay <- getElementById win "displayV"
  case elMay of
    Nothing -> error "can't happen"
    Just el -> return el

getCursorLine :: (ToJS a) => a -> UI Int
getCursorLine cm = callFunction $ ffi (wrapCatchErr "getCursorLine(%1)") cm

getValue :: (ToJS a) => a -> UI String
getValue cm = callFunction $ ffi "getV(%1)" cm

createHaskellFunction name fn = do
  handler <- ffiExport fn
  runFunction $ ffi ("window." ++ name ++ " = %1") handler

-- adding and removing editors

catchJSErrors :: UI ()
catchJSErrors = runFunction $ ffi "window.onerror = function(msg, url, linenumber) { alert(msg);return true;}"

makeEditor :: String -> UI ()
makeEditor i = runFunction $ ffi $ i ++ "cm = CodeMirror.fromTextArea(document.getElementById('" ++ i ++ "'), fullSettings.editor);"

-- flashing

checkUndefined :: (ToJS a) => a -> UI String
checkUndefined cm = callFunction $ ffi "(function (a) { if (typeof a === 'undefined' || a === null) {return \"yes\";} else { return \"no\"; } })(%1)" cm

highlightBlock :: JSObject -> Int -> Int -> String -> UI JSObject
highlightBlock cm lineStart lineEnd color = do
  undef <- checkUndefined cm
  case undef of
    "no" -> callFunction $ ffi "((%1).markText({line: %2, ch: 0}, {line: %3, ch: 0}, {css: %4}))" cm lineStart lineEnd color
    _ -> callFunction $ ffi "return {}"

unHighlight :: JSObject -> UI ()
unHighlight mark = runFunction $ ffi "if (typeof %1 !== 'undefined'){%1.clear()};" mark

flashSuccess :: JSObject -> Int -> Int -> UI ()
flashSuccess cm lineStart lineEnd = do
  mark <- highlightBlock cm lineStart (lineEnd + 1) "background-color: green"
  liftIO $ threadDelay 100000
  unHighlight mark
  flushCallBuffer

flashError :: JSObject -> Int -> Int -> UI ()
flashError cm lineStart lineEnd = do
  mark <- highlightBlock cm lineStart (lineEnd + 1) "background-color: red"
  liftIO $ threadDelay 100000
  unHighlight mark
  flushCallBuffer

-- setting, getting and clearing the config

setConfig :: Window -> String -> String -> IO ()
setConfig win key v = runUI win $ runFunction $ ffi ("window.electronAPI.putInStore(%1," ++ v ++ ")") key

clearConfig :: Window -> IO ()
clearConfig win = runUI win $ runFunction $ ffi "window.electronAPI.clearStore()"

wrapCatchErr :: String -> String
wrapCatchErr st = "try {" ++ st ++ "} catch (err) {}"
