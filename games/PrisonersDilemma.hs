module PrisonersDilemma where
import Botworld
import Botworld.Display
import Data.Map (Map, fromList)

cpu :: Processor
cpu = P 1048576

leftItem :: Item
leftItem = Cargo 0 1

rightItem :: Item
rightItem = Cargo 1 1

leftsValues :: Item -> Int
leftsValues item
  | item == leftItem = 1
  | item == rightItem = 3
  | otherwise = 0

rightsValues :: Item -> Int
rightsValues item
  | item == leftItem = 3
  | item == rightItem = 1
  | otherwise = 0

leftRobot :: Memory -> Robot
leftRobot = Robot (F Red 1) [leftItem] cpu

rightRobot :: Memory -> Robot
rightRobot = Robot (F Blue 1) [rightItem] cpu

leftPlayer :: Player
leftPlayer = Player leftsValues (0, 0)

rightPlayer :: Player
rightPlayer = Player rightsValues (1, 0)

players :: Map String Player
players = fromList [("Red", leftPlayer), ("Blue", rightPlayer)]

mintWorld :: Memory -> Memory -> EventGrid
mintWorld l r = runCreation $ fillGrid (2, 1) [leftCell, rightCell] where
  leftCell = Square [leftRobot l] []
  rightCell = Square [rightRobot r] []

display :: EventGrid -> IO ()
display = displayEventGrid players
