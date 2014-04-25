module Botworld.Debug where
import Botworld
import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.Monoid
import Text.Printf (printf)

type Registers = [String]

data RobotView = Visible Frame [Item] deriving Show
instance Decodable RobotView where
  decode = fmap (uncurry Visible) . decode

data ActionView
  = Indeterminate
  | Precisely Action
  | DidInspect Int
  | DidBuild [Int]
  deriving Show

instance Decodable ActionView where
  decode t = case decode t :: Maybe (Int, Constree) of
    Just (0, Nil) -> Just Indeterminate
    Just (1, Nil) -> Just $ Precisely Created
    Just (2, d)   -> (Precisely . MoveBlocked) <$> decode d
    Just (3, d)   -> (Precisely . MovedOut) <$> decode d
    Just (4, d)   -> (Precisely . MovedIn) <$> decode d
    Just (5, i)   -> (Precisely . CannotLift) <$> decode i
    Just (6, i)   -> (Precisely . GrappledOver) <$> decode i
    Just (7, i)   -> (Precisely . Lifted) <$> decode i
    Just (8, i)   -> (Precisely . Dropped) <$> decode i
    Just (9, i)   -> (Precisely . InspectTargetFled) <$> decode i
    Just (10, i)  -> (Precisely . InspectBlocked) <$> decode i
    Just (11, i)  -> DidInspect <$> decode i
    Just (12, i)  -> (Precisely . DestroyTargetFled) <$> decode i
    Just (13, i)  -> (Precisely . DestroyBlocked) <$> decode i
    Just (14, i)  -> (Precisely . Destroyed) <$> decode i
    Just (15, is) -> DidBuild <$> decode is
    Just (16, is) -> (Precisely . BuildInterrupted) <$> decode is
    _             -> Nothing

data PrivateInput = None | WasInvalid | InspectSuccess Int Int Memory
  deriving Show

instance Decodable PrivateInput where
  decode Nil = Just None
  decode (Cons (Cons Nil Nil) Nil) = Just WasInvalid
  decode t = uncurry3 InspectSuccess <$> decode t
    where uncurry3 f (a, b, c) = f a b c

type Input = (Int, Event, Constree)
type InputView = (Int, ([RobotView], [ActionView], ([Item], [Item], [Item])), PrivateInput)


inspectPRG :: Register -> String
inspectPRG (R _ t) = maybe "???" show (decode t :: Maybe [Instruction])

inspectINP :: Register -> String
inspectINP (R _ Nil) = "Nil"
inspectINP (R _ t) = maybe "???" show (decode t :: Maybe InputView)

inspectOUT :: Register -> String
inspectOUT (R _ Nil) = "Nil"
inspectOUT (R _ t) = maybe "???" show (decode t :: Maybe Command)

inspectREG :: Register -> String
inspectREG (R _ t) = fromMaybe "???" . listToMaybe $ catMaybes decodings where
  showR (R x y) = "<R " ++ show x ++ " ...>"
  decodings =
    [ if t == Nil then Just "Nil" else Nothing
    , show  <$> (decode t :: Maybe Int)
    , show  <$> (decode t :: Maybe Command)
    , show  <$> (decode t :: Maybe (Command, Command))
    , show  <$> (decode t :: Maybe (Instruction, Instruction))
    , show  <$> (decode t :: Maybe [Instruction])
    , show  <$> (decode t :: Maybe [Command])
    , show  <$> (decode t :: Maybe Instruction)
    , show  <$> (decode t :: Maybe (Int, Int))
    , show  <$> (decode t :: Maybe [Int])
    , showR <$> (decode t :: Maybe Register)
    ]

inspectMemory :: [String] -> Memory -> [String]
inspectMemory names regs = zipWith3 prepare [(0::Int)..] names stringified where
  prepare index name content = padI index <> padN name <> content
  padN = printf ("%" <> show widthN <> "s:\t")
  padI = printf ("%0" <> show widthI <> "d:")
  widthN = maximum $ map (length . show) names
  widthI = length $ show $ pred (length names)
  stringified = case regs of
    (p:i:o:rs) -> inspectPRG p : inspectINP i : inspectOUT o : map inspectREG rs
    regs -> map inspectREG regs

-- | Discards and clears a robot's output register.
clearOutput :: Robot -> Robot
clearOutput = fst . (takeOutput :: Robot -> (Robot, Maybe Command))

-- | Discards and clears a robot's output register, then sets its input register.
prep :: Robot -> Input -> Robot
prep = setInput . clearOutput

-- | Clears OUT, sets INP, then runs the register machine according to the robot's speed.
stepRobot :: Robot -> Input -> Robot
stepRobot r i = tickRobot (prep r i) (speed $ processor r)

-- | Runs the register machine for N steps.
tickRobot :: Robot -> Int -> Robot
tickRobot r n = case runFor n (memory r) of
  Right m -> r{memory=m}
  Left e -> error $ show e

-- | Runs the register machine until a predicate becomes true.
tickRobotUntil :: Robot -> (Memory -> Bool) -> (Int, Robot)
tickRobotUntil r p = if p (memory r) then (0, r)
  else first succ $ tickRobotUntil (tickRobot r 1) p
