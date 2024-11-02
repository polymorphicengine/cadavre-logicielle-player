{-# LANGUAGE FlexibleInstances #-}

module Editor.Types where

import Control.Concurrent (MVar)
import Control.Monad.State (StateT, lift, runStateT)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Graphics.UI.Threepenny (MonadUI, UI, liftUI)
import qualified Network.Socket as N
import Sound.Osc (Udp)
import qualified Sound.Osc as O

data EvalMode
  = EvalBlock
  | EvalLine
  deriving (Eq, Show)

-- TODO : maybe replace with ReaderT?
type Game = StateT Env UI

instance MonadUI Game where
  liftUI = lift . liftUI

runGame :: Game a -> Env -> UI (a, Env)
runGame = runStateT

type Range = MVar (Int, Int)

type TableAddress = N.SockAddr

data Env
  = Env
  { sLocal :: Udp,
    sRemote :: MVar TableAddress,
    sRange :: Range
  }

type Act = StateT ActState UI

instance MonadUI Act where
  liftUI = lift . liftUI

runAct :: Act a -> ActState -> UI (a, ActState)
runAct = runStateT

data ActState
  = ActState
  { aDefs :: Definitions,
    aPlayers :: Players
  }

data Player
  = Player {pName :: String, pOrbit :: Int}

type Players = [Player]

instance Show Player where
  show (Player name orb) = name ++ " on orbit " ++ show orb

data Definition
  = Def
  { dName :: String,
    dType :: String
  }
  deriving (Eq)

instance Show Definition where
  show (Def name typ) = name ++ " :: " ++ typ

type Definitions = [Definition]

utf8String :: String -> O.Datum
utf8String s = O.AsciiString $ encodeUtf8 $ T.pack s

toUTF8 :: O.Ascii -> String
toUTF8 x = T.unpack $ decodeUtf8 x
