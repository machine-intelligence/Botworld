module Rudimentary where
import Botworld
import Control.Monad.Reader (runReader)
import Data.Maybe (catMaybes)

-- Simple programs
-- Below is a function that allows us to create robots that execute a hardcoded
-- list of actions using seven registers. (These robots must be run on
-- a processor that has speed 3 or greater.)

-- Our simple programs use seven registers. These include:
rPRG = 0  -- Program register (builtin)
rINP = 1  -- Input register (builtin)
rOUT = 2  -- Output register (builtin)
rNIL = 3  -- Nil register (will always be nil)
rSPN = 4  -- Spin register (holds a program that spins until output is read)
rCTR = 5  -- Control register (holds the main program while spinning)
rQUE = 6  -- Queue register (holds the upcoming commands)

-- This helper function that creates a register of exactly the right size for
-- its contents.
reg :: Encodable i => i -> Register
reg i = let t = encode i in R (size t) t

-- This function builds a 7-register machine that hardcodes its commands.
-- Basically, it holds a list of commands it wants to execute in the QUE
-- register. The main program deconstructs the QUE register putting the head in
-- OUT and the rest back in QUE, then loads the SPN program. The SPN program
-- spins until OUT becomes Nil, then loads the main program back up. Rinse,
-- repeat.
hardcode :: [Command] -> Memory
hardcode [] = []
hardcode commands = registers where
  registers = program : input : output : nil : spinner : queuer : queue : []
  psize = max (size $ contents queuer) (size $ contents spinner)
  program = R psize (contents spinner)
  spinner = reg [CopyIfNil rOUT rCTR rPRG, CopyIfNil rNIL rSPN rPRG]
  input = R 0 Nil
  outputs = map encode commands
  output = R (maximum $ map size outputs) (head outputs)
  nil = R 0 Nil
  queuer = reg [Deconstruct rQUE rOUT rQUE, CopyIfNil rNIL rSPN rPRG]
  queue = reg $ tail outputs

-- This function is useful when you're debugging a program and want to simulate
-- that OUT has been read. (Remember that OUT is zeroed after it is read.)
zeroOUT :: Memory -> Memory
zeroOUT = alter 2 (forceR Nil)

-- This function runs a memory and assumes that the run succeeds.
-- (If it fails, the function crashes.)
unsafeRun :: Int -> Memory -> Memory
unsafeRun n m = let Right m' = runFor n m in m'

-- This function dumps the contents of a memory IF the memory was created with
-- the hardcode function above. Otherwise, it crashes.
unsafeContents :: Memory -> String
unsafeContents (p:i:o:n:s:c:q:[]) = result where
  result = unlines $
    ( ("PRG: " ++ show prg)
	: ("INP: " ++ show inp)
	: ("OUT: " ++ out)
	: ("NIL: " ++ show nil)
	: ("SPN: " ++ show spn)
	: ("CTR: " ++ show ctr)
	: ("QUE: " ++ show que)
	: [])
  Just prg = decode (contents p) :: Maybe [Instruction]
  inp = contents i
  out = maybe (show $ contents o) show (decode (contents o) :: Maybe Command)
  nil = contents n
  Just spn = decode (contents s) :: Maybe [Instruction]
  Just ctr = decode (contents c) :: Maybe [Instruction]
  Just que = decode (contents q) :: Maybe [Command]

-- This function is like the above function, but it prints the contents of the
-- memory to IO (instead of just turning it into a string).
unsafeCheck :: Memory -> IO ()
unsafeCheck = putStr . unsafeContents

-- World building

-- Adds a robot to a square.
addRobot :: Robot -> Maybe Square -> Maybe Square
addRobot r = fmap (\(Square rs is) -> Square (r:rs) is)

-- Lets you see the state of a world.
see :: GameConfig -> Botworld -> String
see = flip $ runReader . visualize


-- Example game:

-- Only cargo is valued.
simpleValuer :: Item -> Int
simpleValuer (Cargo v _) = v
simpleValuer _ = 0

-- There is only one player, Player 1. Their home square is top-left.
sampleGame :: GameConfig
sampleGame = GameConfig [((0, 0), "Player 1")] simpleValuer

-- These are the cargos in the initial world.
sampleCargoes :: [[Item]]
sampleCargoes =
  [ [Cargo 2 3, Cargo  1 2, Cargo 1 2, Cargo 9 1, Cargo 1 1]
  , [Cargo 3 3, Cargo  3 3, Cargo 9 3, Cargo 6 1, Cargo 5 2]
  , [Cargo 2 2, Cargo 10 1, Cargo 3 3, Cargo 9 3, Cargo 4 2]
  , [Cargo 1 2, Cargo  6 3, Cargo 2 3, Cargo 1 2, Cargo 3 2]
  , [Cargo 5 2, Cargo  6 1, Cargo 6 1, Cargo 8 2, Cargo 1 3]
  ]

uninhabitedWorld :: Botworld
uninhabitedWorld = generate (5, 5) gen where
  gen (x, y) = Just $ Square [] [sampleCargoes !! y !! x]

-- The lifter robot tries to lift a few boxes and then return to the home
-- square.
lifter :: Robot
lifter = Robot (F Red 10) [] (P 10) $ hardcode
  [Lift 0, Move S, Lift 0, Move E, Move S, Lift 0 , Move N, Move N, Move W]

-- The aggressor tries to destroy the lifter.
aggressor :: Robot
aggressor = Robot (F Green 200) [] (P 10) $ hardcode
  [Pass, Pass, Destroy 1, Build [0..12] $ hardcode [Move W]]

-- The overwriter tries to rebuild the aggressor into a nicer robot (that walks
-- away).
overwriter :: Robot
overwriter = Robot (F Blue 0) [] (P 10) $ hardcode
  [Move N, Destroy 0, Build [1..9] $ hardcode [Move S, Move S]]

-- Here is a world with all three robots.
populatedWorld :: Botworld
populatedWorld =
  change (addRobot lifter) (0, 0) $
  change (addRobot aggressor) (0, 1) $
  change (addRobot overwriter) (0, 2)
  uninhabitedWorld

-- This infinite list contains all updates of the initial world. (We'll look at
-- the first ten or so.)
evolution :: [Botworld]
evolution = iterate update populatedWorld

-- This function displays the world (given our game config).
display :: Botworld -> String
display = see sampleGame

-- When run, this file prints out the initial state, the final state, and
-- a scoreboard.
main :: IO ()
main = do
  let initialWorld = evolution !! 0
  let finalWorld = evolution !! 10
  putStr $ display initialWorld
  putStr $ display finalWorld
  putStr $ runReader (scoreboard finalWorld) sampleGame
