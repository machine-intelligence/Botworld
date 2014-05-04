module Botworld.Display where
import Botworld hiding (Cell)
import Botworld.TextBlock
import Control.Applicative
import Control.Monad (join)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Text.Printf (printf)

colorChar :: Color -> Char
colorChar Red     = 'R'
colorChar Orange  = 'O'
colorChar Yellow  = 'Y'
colorChar Green   = 'G'
colorChar Blue    = 'B'
colorChar Violet  = 'V'
colorChar Black   = 'Ω'
colorChar White   = 'W'

showPos :: Position -> String
showPos = uncurry (printf "(%d, %d)")

squareStatus :: Square -> TextBlock
squareStatus = pure . robotStatus . robotsIn

changeStatus :: Event -> TextBlock
changeStatus res = [robotStatus presentRobots, actionStatus acts] where
  acts = snd <$> robotActions res
  presentRobots = [r | (r, a) <- robotActions res, not $ isExit a]

robotStatus :: [Robot] -> String
robotStatus rs = case colorChar . color . frame <$> rs of
  [c] -> printf "  %c  " c
  [c1, c2] -> printf " %c %c " c1 c2
  (c1:c2:c3:c4:_:_) -> printf "%c%c%c%c…" c1 c2 c3 c4
  cs -> printf "%-5s" cs

actionStatus :: [Action] -> String
actionStatus as = liftDropFlag : (uncurry bit <$> flags) where
  bit p f = if any p as then f else ' '
  flags = [(isMake, '+'), (isKill, '×'), (isInspect, '?'), (isInvalid, '!')]
  liftDropFlag = case (any isLift as, any isDrop as) of
    (False, False)  -> ' '
    (True, False)   -> '↑'
    (False, True)   -> '↓'
    (True, True)    -> '↕'
  isLift a = case a of { Lifted _ -> True; _ -> False }
  isDrop a = case a of { Dropped _ -> True; _ -> False }
  isMake a = case a of { Created -> True; _ -> False }
  isKill a = case a of { Destroyed _ -> True; _ -> False }
  isInspect a = case a of { Inspected _ _ -> True; _ -> False }
  isInvalid a = case a of { Invalid -> True; _ -> False }

robotSummary :: Int -> Robot -> TextBlock
robotSummary i r =
  [ show i
  , show $ color $ frame r
  , printf "%d spd, %d str" (speed $ processor r) (strength $ frame r)
  ]

actionSummary :: Action -> String
actionSummary a = case a of
  Created               -> "created"
  Passed               -> "passed"
  MoveBlocked d        -> printf "%s %s" "blocked" (show d)
  MovedOut d           -> printf "%s %s" "exited" (show d)
  MovedIn d            -> printf "%s %s" "entered" (show d)
  CannotLift i         -> printf "%s %2d" "cantlift" i
  GrappledOver i       -> printf "%s %2d" "grappled" i
  Lifted i             -> printf "%s %2d" "lift" i
  Dropped i            -> printf "%s %2d" "drop" i
  InspectTargetFled i  -> printf "%s %2d" "evadedby" i
  InspectBlocked i     -> printf "%s %2d" "assaulted" i
  Inspected i _        -> printf "%s %2d" "inspected" i
  DestroyTargetFled i  -> printf "%s %2d" "missed" i
  DestroyBlocked i     -> printf "%s %2d" "battered" i
  Destroyed i          -> printf "%s %2d" "destroyed" i
  BuildInterrupted is  -> printf "%s %2d" "interrupted" $ ilist is
  Built is _           -> printf "%s %s" "built" $ ilist is
  Invalid              -> "invalid"
  where ilist is = unwords $ printf "%2d" <$> is

itemDetail :: Item -> TextBlock
itemDetail DestroyShield = ["[†]"]
itemDetail InspectShield = ["[°]"]
itemDetail (FramePart f) = [printf "[%c]" $ colorChar $ color f, show $ strength f]
itemDetail (ProcessorPart p) = ["[#]", show $ speed p]
itemDetail (RegisterPart r) = ["[|]", show $ limit r]
itemDetail (Cargo t w) = [printf "$%d" t, printf "%dg" w]

cellDetail :: Position -> Event -> TextBlock
cellDetail pos res = labeled (showPos pos) [summary, robotSection, itemSection] where
  summary = labeled "Summary" [summaryGrid res]
  robotSection = labeled "Robots" $ uncurry robotDetail <$> robotActions res
  itemSection = labeled "Items" [untouchedSection, droppedSection, fallenSection]
  untouchedSection = labeled "Untouched" [itemGrid $ untouchedItems res]
  droppedSection = labeled "Dropped" [itemGrid $ droppedItems res]
  fallenSection = labeled "Fallen" $ fallenGrid <$> fallenItems res

  summaryGrid :: Event -> TextBlock
  summaryGrid = renderGrid' . vGrid 4 . zipWith summarize [0..] . robotActions

  summarize :: Int -> (Robot, Action) -> TextBlock
  summarize i (r, a) = robotSummary i r ++ [actionSummary a]

  robotDetail :: Robot -> Action -> TextBlock
  robotDetail r a =
    [ show $ color $ frame r
    , printf "Action: %s" $ actionSummary a
    , printf "Strength: %d" $ strength $ frame r
    , printf "Speed: %d" $ speed $ processor r
    , printf "Registers: %d" $ length $ memory r
    ] ++ labeled "Items" [itemGrid $ inventory r]

  itemGrid :: [Item] -> TextBlock
  itemGrid [] = ["None."]
  itemGrid items = renderGrid' . vGrid 6 $ itemDetail <$> items

  fallenGrid :: ItemCache -> TextBlock
  fallenGrid (ItemCache xs ys) = renderGrid' $ join <$> vGrid 6 items where
    items = (Just . itemDetail <$> xs) ++ [Nothing] ++ (Just . itemDetail <$> ys)

renderBotworld :: Map String Player -> Botworld -> TextBlock
renderBotworld players g = renderWorld (fmap squareStatus <$> g) homes Map.empty where
  homes = Map.fold (Set.insert . home) Set.empty players

displayBotworld :: Map String Player -> Botworld -> IO ()
displayBotworld ps = display . renderBotworld ps

renderEventGrid :: Map String Player -> EventGrid -> TextBlock
renderEventGrid ps g = renderWorld (fmap changeStatus <$> g) homes moves where
  homes = Map.fold (Set.insert . home) Set.empty ps
  moves = Map.fromList $ makeViolation <$> indices g
  makeViolation pos = (pos, Set.fromList $ allViolations $ at g pos)
  allViolations (Just res)
    =  [Right d | MovedIn d <- snd <$> robotActions res]
    ++ [Left d | MovedOut d <- snd <$> robotActions res]
  allViolations Nothing = []

displayEventGrid :: Map String Player -> EventGrid -> IO ()
displayEventGrid ps = display . renderEventGrid ps

renderScoreboard :: Map String Player -> Botworld -> TextBlock
renderScoreboard ps w = blankSeparated $ uncurry scoreDetail <$> sortedPlayers where
  sortedPlayers = sortBy (comparing $ score w . snd) (Map.assocs ps)
  scoreDetail name p = labeled label [scores] where
    scores = robotScore <$> maybe [] robotsIn (at w $ home p)
    len = max (length label1) $ if null scores then 8 else 2 + maximum (length <$> scores)
    label = label1 ++ label2
    label1 = printf " %s: %d$\n" name (score w p)
    label2 = replicate len '─'
    robotScore r = printf "%s robot: %d points" (show $ color $ frame r) (points p r)

displayScoreboard :: Map String Player -> Botworld -> IO ()
displayScoreboard ps = display . renderScoreboard ps

renderChangesAt :: EventGrid -> Position -> TextBlock
renderChangesAt w pos = maybe wall (cellDetail pos) (at w pos) where
  wall = labeled (showPos pos) [["Wall."]]

displayChangesAt :: EventGrid -> Position -> IO ()
displayChangesAt w = display . renderChangesAt w
