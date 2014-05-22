module Precommit where
import Botworld
import Botworld.Display
import Data.Maybe
import Data.Map (Map, empty, insert)
import qualified Precommit.Omega as Omega
import qualified Precommit.Ideal as Ideal
import Text.Printf (printf)

-- Processor you get to use.

-- NOTE: This value should be increased if you plan to implement an AIXI.
cpu :: Processor
cpu = P 8192

-- Game description

value :: Item -> Int
value (Cargo price _) = price
value _ = 0

you :: Player
you = Player standardValuer (0, 0)

players :: Map String Player
players = insert "You" you empty

-- World description

worthless :: Item
worthless = Cargo 1 5

valuable :: Item
valuable = Cargo 100 5

rob :: Memory -> Robot
rob = Robot (F Red 10) [worthless] cpu

omega :: Robot
omega = Robot (F Black 10) inventory (P 4096) Omega.machine where
  inventory = valuable : replicate 5 DestroyShield

mintWorld :: Memory -> Botworld
mintWorld mem = Grid (2, 1)
  [ Just (Square [rob mem] [])
  , Just (Square [omega] [])
  ]

-- One game: the initial state plus five updated states.
game :: Memory -> [Botworld]
game = take 6 . iterate update . mintWorld

-- Extracts the final state of rob's memory (if rob survives).
extract :: Botworld -> Memory
extract (Grid _ cs) = get $ filter isRed allRobots where
  allRobots = concatMap robotsIn $ catMaybes cs
  isRed r = color (frame r) == Red
  get (r:_) = memory r
  get [] = []

-- An eternity of games (with the robot memory carried across each boundary).
eternity :: Memory -> [[Botworld]]
eternity m = let games = game m : map (game . extract . last) games in games

-- Display one game
displayGame :: [Botworld] -> IO ()
displayGame states = do
  displayBotworld players $ head states
  mapM_ (displayEventGrid players . runEnvironment) (init states)
  displayScoreboard players $ last states

-- Display many games
playN :: Memory -> Int -> IO ()
playN m n = do
  let games = take n $ eternity m
  mapM_ displayGame games
  let reward = sum $ map (flip score you . last) games
  putStrLn "\n═══════════════════════"
  printf " Total: %d\n" reward

main :: IO ()
main = playN Ideal.machine 2
