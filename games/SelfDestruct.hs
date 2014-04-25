module SelfDestruct where
import Botworld
import Botworld.Display
import Data.Maybe
import Data.Map (Map, empty, insert)
import qualified SelfDestruct.Omega as Omega
import qualified SelfDestruct.Ideal as Ideal
import Text.Printf (printf)

-- Processor you get to use.

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
valuable = Cargo 99 5

rex :: Memory -> Robot
rex = Robot (F Red 10) [worthless] cpu

omega :: Robot
omega = Robot (F Black 10) [valuable, Shield, Shield] (P 4096) Omega.machine

mintWorld :: Memory -> Botworld
mintWorld mem = Grid (2, 1)
  [ Just (Square [rex mem] [])
  , Just (Square [omega] [])
  ]

-- One game: the initial state plus five updated states.
game :: Memory -> [Botworld]
game = take 6 . iterate update . mintWorld

-- Display one game
displayGame :: [Botworld] -> IO ()
displayGame states = do
  displayBotworld players $ head states
  mapM_ (displayEventGrid players . runEnvironment) (init states)
  displayScoreboard players $ last states

main :: IO ()
main = displayGame $ game Ideal.machine
