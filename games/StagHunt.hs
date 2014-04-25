module StagHunt where
import Botworld
import Botworld.Display
import Control.Applicative
import Control.Monad (join)
import Data.Map (Map, fromList)

cpu :: Processor
cpu = P 1048576

hare :: Robot
hare = Robot (F White 1) [Cargo 1 1] (P 0) []

stag :: Robot
stag = Robot (F Yellow 1) [Cargo 2 1, Shield] (P 0) []

robot :: Color -> Memory -> Robot
robot c = Robot (F c 1) [] cpu

mintWorld :: Memory -> Memory -> Botworld
mintWorld r b = join <$> fillGrid (5, 5) squares where
  xxx = Nothing
  ___ = Just $ Square [] []
  _h_ = Just $ Square [hare] []
  _s_ = Just $ Square [stag] []
  _R_ = Just $ Square [robot Red r] []
  _B_ = Just $ Square [robot Blue b] []
  squares =
    [ _h_, ___, ___, xxx, xxx
	, ___, xxx, ___, xxx, xxx
	, _R_, ___, _s_, ___, ___
	, xxx, xxx, ___, xxx, ___
	, xxx, xxx, _B_, ___, _h_
	]

players :: Map String Player
players = fromList
  [ ("Red player", Player standardValuer (2, 0))
  , ("Blue player", Player standardValuer (4, 2))
  ]

display :: Botworld -> IO ()
display = displayBotworld players
