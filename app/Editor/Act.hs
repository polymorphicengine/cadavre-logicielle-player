module Editor.Act where

import Control.Concurrent (tryTakeMVar)
import Control.Monad.State (liftIO, modify)
import Editor.Types
import Editor.UI
import Graphics.UI.Threepenny (liftUI)
import Sound.Osc.Fd as O

--------------------------------------------------------
--------- acting on responses from the table -----------
--------------------------------------------------------

playingHand :: Env -> Act ()
playingHand env = do
  let udp = sLocal env
  m <- liftIO $ recvMessage udp
  act m env
  playingHand env

act :: Maybe O.Message -> Env -> Act ()
act (Just (Message "/ok" [])) env = successAction env
act (Just (Message "/ok" [AsciiString x])) env = successAction env >> liftUI (addMessage (toUTF8 x))
act (Just (Message "/error" [AsciiString e])) env = errorAction env >> liftUI (addMessage (toUTF8 e))
act (Just (Message "/joined" [AsciiString name, Int32 i])) _ = joinedAction (toUTF8 name) (fromIntegral i)
act (Just (Message "/define" [AsciiString name, AsciiString defName, AsciiString typ])) _ = addDefinition (toUTF8 name) (Def (toUTF8 defName) (toUTF8 typ))
act (Just (Message "/change" [AsciiString name, AsciiString defName])) _ = changeAction (toUTF8 name) (toUTF8 defName)
act (Just (Message "/message" [AsciiString msg])) _ = messageAction (toUTF8 msg)
act (Just m) _ = liftUI $ addMessage ("Unhandeled message: " ++ show m)
act Nothing _ = liftUI $ addMessage "Not a message?"

successAction :: Env -> Act ()
successAction env = do
  let rMV = sRange env
  may <- liftIO $ tryTakeMVar rMV
  case may of
    Nothing -> return ()
    Just (st, end) -> do
      maycm <- liftUI getCodeMirror
      case maycm of
        Just cm -> liftUI $ flashSuccess cm st end
        Nothing -> return ()

errorAction :: Env -> Act ()
errorAction env = do
  let rMV = sRange env
  may <- liftIO $ tryTakeMVar rMV
  case may of
    Nothing -> return ()
    Just (st, end) -> do
      maycm <- liftUI getCodeMirror
      case maycm of
        Just cm -> liftUI $ flashError cm st end
        Nothing -> return ()

joinedAction :: String -> Int -> Act ()
joinedAction name orb = addPlayer $ Player name orb

changeAction :: String -> String -> Act ()
changeAction name def = liftUI $ addMessage (name ++ " changed the definition of " ++ def)

messageAction :: String -> Act ()
messageAction msg = liftUI $ addMessage msg

addDefinition :: String -> Definition -> Act ()
addDefinition name d = do
  el <- liftUI $ mkDefinition d
  liftUI $ addMessage (name ++ " folded the document and revealed " ++ show d)
  liftUI $ addElement "definition" "definition-container" el
  modify $ \st -> st {aDefs = d : aDefs st}

addPlayer :: Player -> Act ()
addPlayer p = do
  el <- liftUI $ mkPlayer p
  liftUI $ addMessage (pName p ++ " joined the game!")
  liftUI $ addElement "player" "player-container" el
  modify $ \st -> st {aPlayers = p : aPlayers st}
