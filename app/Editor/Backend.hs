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
import Editor.Parse
import Editor.UI
import Foreign.JavaScript (JSObject)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)
import qualified Network.Socket as N
import Sound.Osc.Fd as O

data EvalMode
  = EvalBlock
  | EvalLine
  deriving (Eq, Show)

type LastBlock = MVar (JSObject, Int, Int)

evalContentAtCursor :: Udp -> N.SockAddr -> EvalMode -> LastBlock -> JSObject -> UI ()
evalContentAtCursor local remote mode bMV cm = parseBlocks mode cm bMV >>= actOnCommand local remote

parseBlocks :: EvalMode -> JSObject -> LastBlock -> UI Command
parseBlocks mode cm bMV = do
  line <- getCursorLine cm
  contents <- getValue cm
  let blockMaybe = case mode of
        EvalLine -> getLineContent line (linesNum contents)
        EvalBlock -> getBlock line $ getBlocks contents
  case blockMaybe of
    Nothing -> getOutputEl >>= \out -> element out # set UI.text "Failed to get Block" >> return Ping
    Just b -> do
      liftIO $ putMVar bMV (cm, bStart b, bEnd b)
      case runParser (bContent b) of
        Left err -> error $ show err
        Right c -> return c

actOnCommand :: Udp -> N.SockAddr -> Command -> UI ()
actOnCommand local addr (Statement str) = liftIO $ O.sendTo local (O.p_message "/eval" [O.string str]) addr
actOnCommand local addr (Type str) = liftIO $ O.sendTo local (O.p_message "/type" [O.string str]) addr
actOnCommand local addr Ping = liftIO $ O.sendTo local (O.p_message "/ping" []) addr
actOnCommand local addr (Definition n t c) = defAction n t c local addr

serve :: Udp -> Element -> LastBlock -> UI ()
serve udp out bMV = do
  m <- liftIO $ recvMessage udp
  act out bMV m
  serve udp out bMV

act :: Element -> LastBlock -> Maybe O.Message -> UI ()
act _ bMV (Just (Message "/ok" [])) = do
  (cm, st, end) <- liftIO $ takeMVar bMV
  flashSuccess cm st end
act out bMV (Just (Message "/ok" [AsciiString x])) = do
  (cm, st, end) <- liftIO $ takeMVar bMV
  _ <- element out # set UI.text (ascii_to_string x)
  flashSuccess cm st end
act out bMV (Just (Message "/error" [AsciiString e])) = do
  (cm, st, end) <- liftIO $ takeMVar bMV
  _ <- element out # set UI.text ("Error:" ++ ascii_to_string e)
  flashError cm st end
act out _ (Just m) = element out # set UI.text ("Unhandeled message: " ++ show m) >> return ()
act out _ Nothing = element out # set UI.text "Not a message?" >> return ()

defAction :: String -> String -> String -> Udp -> N.SockAddr -> UI ()
defAction name t c local remote = liftIO $ O.sendTo local (O.p_message "/define" [O.string name, O.string t, O.string code, O.string def]) remote
  where
    def = "let " ++ name ++ " = _define" ++ t ++ "\"" ++ name ++ "\""
    code = "streamSetF tidal " ++ show name ++ " " ++ c
