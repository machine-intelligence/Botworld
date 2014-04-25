module Botworld.TextBlock
  ( TextBlock
  , HomeSquares
  , Movements
  , display
  , labeled
  , blankSeparated
  , squareUp
  , squareUp'
  , renderMatrix
  , renderGrid
  , renderGrid'
  , renderWorld
  ) where
import Botworld hiding (Cell)
import Control.Applicative
import Control.Arrow ((&&&))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

-- |General note: in this module, primed things are the Maybe-versions.

type TextBlock = [String]
type HomeSquares = Set Position
type Movements = Map Position (Set (Either Direction Direction))

display :: TextBlock -> IO ()
display = putStr . unlines

labeled :: String -> [TextBlock] -> TextBlock
labeled header [] = header : ["  None."]
labeled header tbs = header : map indent (blankSeparated tbs) where
  indent "" = ""
  indent x = "  " ++ x

blankSeparated :: [TextBlock] -> TextBlock
blankSeparated = intercalate [""]

-- |Squares up a matrix of text blocks so that, when rendered, they will line
-- up in a grid pattern.
squareUp :: [[TextBlock]] -> [[TextBlock]]
squareUp = squareUp' . map (map Just)

-- |Squares up a matrix of maybe text blocks, filling the missing ones with
-- appropriately many ╳ characters.
squareUp' :: [[Maybe TextBlock]] -> [[TextBlock]]
squareUp' rows = map normalizeRow paddedRows where
  paddedRows = map (pad Nothing $ maxlen rows) rows
  pad x n xs = xs ++ replicate (n - length xs) x
  cellheight = foldr max 0 . mapMaybe (fmap length)
  maxlen = foldr (max . length) 0

  normalizeRow row = zipWith normalizeCell [0..] row where
    normalizeCell :: Int -> Maybe [String] -> [String]
    normalizeCell i Nothing = padStrings '╳' i $ replicate (cellheight row) ""
    normalizeCell i (Just strs) = padStrings ' ' i $ pad "" (cellheight row) strs

  padStrings c i = map $ pad c (colwidth i)
  colwidth i = foldr max 0 $ catMaybes [maxlen <$> row !! i | row <- paddedRows]

-- |Renders a matrix of textblocks into a single textblock.
-- [[ ["A", "B"], ["C", "D"] ],[ ["E"], ["F"] ]]
-- renders as: AC
--             BD
--             EF
renderMatrix :: [[TextBlock]] -> TextBlock
renderMatrix = concatMap renderRow where
  renderRow [] = []
  renderRow r@(tb:_) = foldr conjoin (replicate (length tb) "") r
  conjoin (x:xs) (y:ys) = (x ++ y) : conjoin xs ys
  conjoin _ _ = []

-- |Renders a grid of text blocks. (Adds ASCII box borders.)
renderGrid :: Grid TextBlock -> TextBlock
renderGrid = renderGrid' . fmap Just

-- |Renders a grid of maybe textblocks. (Adds ASCII box borders and shades
-- missing squares with ╳ characters.)
renderGrid' :: Grid (Maybe TextBlock) -> TextBlock
renderGrid' g = renderWorld g Set.empty Map.empty

-- |Renders a grid Botworld style. This is like rendering a normal grid with
-- two special options:
-- 1. Specific cells (home squares) may be highlighted (given thick borders).
-- 2. Specific borders may be breached with arrow characters (indicating
--    movement).
renderWorld :: Grid (Maybe TextBlock) -> HomeSquares -> Movements -> TextBlock
renderWorld g homes moves = visualization where
  visualization = renderMatrix rows
  rows = chunksOf width $ map borderedBlock $ indices normalized
  normalized = g{cells=concat $ squareUp' $ chunksOf width $ cells g}
  (width, height) = dimensions g

  chunksOf _ [] = []
  chunksOf r xs = take r xs : chunksOf r (drop r xs)

  breachesOf pos = fromMaybe Set.empty $ Map.lookup pos moves
  lbreachChar pos dir x y =
    if Left dir `Set.member` breachesOf pos then y else x
  rbreachChar pos dir x y =
    if Right dir `Set.member` breachesOf pos then y else x

  borderedBlock :: Position -> [String]
  borderedBlock pos = addTop $ addBottom $ zipWith addSide [0 :: Int ..] tb where
    tb = at normalized pos
    addTop = (top:)
    addBottom xs = if onBottom then xs ++ [bottom] else xs
    h = length tb
    w = if null tb then 0 else length $ head tb

    breaches = breachesOf pos
    breachings d = (has $ Left d, has $ Right d) where
      has x = x `Set.member` breaches
    breach dir x ys = case breachings dir of
      (False, False) -> x
      (True, False) -> head ys
      (False, True) -> ys !! 1
      (True, True) -> ys !! 2
    lpad = (w - 5) `div` 2
    rpad = w - 5 - lpad

    top = tl : horiz ++ end where
      horiz = if w >= 5 then breached else replicate w t
      end = [tr | onRightEdge]
      breached = wside ++ [breach N t "⇑⇓⇕"] ++ eside
      wside = [exitNW, enterNW] ++ replicate lpad t
      eside = replicate rpad t ++ [enterNE, exitNE]
      exitNW = lbreachChar pos NW t '⇖'
      enterNW = rbreachChar pos NW t '⇘'
      enterNE = rbreachChar pos NE t '⇙'
      exitNE = lbreachChar pos NE t '⇗'

    addSide n x = vl : x ++ end where
      end = [vr | onRightEdge]
      vl = if h >= 2 && n == 0 then breach W l "⇐⇒⇔" else l
      vr = if h >= 2 && n == 0 then breach E r "⇒⇐⇔" else r

    bottom = bl : horiz ++ end where
      horiz = if w >= 5 then breached else replicate w b
      end = [br | onRightEdge]
      breached = wside ++ [breach S b "⇓⇑⇕"] ++ eside
      wside = [sExitNW, sEnterNW] ++ replicate lpad b
      eside = replicate rpad b ++ [sEnterNE, sExitNE]
      sPos = towardsIn S pos
      sExitNW = lbreachChar sPos NW b '⇖'
      sEnterNW = rbreachChar sPos NW b '⇘'
      sEnterNE = rbreachChar sPos NE b '⇙'
      sExitNE = lbreachChar sPos NE b '⇗'

    onRightEdge = fst pos == pred width
    onBottom = snd pos == pred height
    isThick = pos `Set.member` homes
    [t, tr, r, br, b, bl, l, tl] = borders isThick (thickMap pos)

  thickMap pos = Map.fromList $ mapMaybe (thickVal pos) [N ..]
  thickVal p d = (const d &&& (`Set.member` homes)) <$> towardsIn' d p

  towardsIn d p = (x `mod` width, y `mod` height) where
    (x, y) = towards d p

  towardsIn' d p = if inGrid then Just (x, y) else Nothing where
    (x, y) = towards d p
    inGrid = x >= 0 && x < width && y >= 0 && y < height


-- |Finds the eight border characters for a square given its border thickness
-- and the thickness of its neighbors' borders.
borders :: Bool -> Map Direction Bool -> String
borders thickness neighbors = [t, tr, r, br, b, bl, l, tl] where
  n    = max (Just thickness) (Map.lookup N neighbors)
  nne  = max (Map.lookup N neighbors) (Map.lookup NE neighbors)
  nee  = max (Map.lookup NE neighbors) (Map.lookup E neighbors)
  e    = max (Just thickness) (Map.lookup E neighbors)
  ese  = max (Map.lookup E neighbors) (Map.lookup SE neighbors)
  ses  = max (Map.lookup SE neighbors) (Map.lookup S neighbors)
  s    = max (Just thickness) (Map.lookup S neighbors)
  ssw  = max (Map.lookup S neighbors) (Map.lookup SW neighbors)
  sww  = max (Map.lookup SW neighbors) (Map.lookup W neighbors)
  w    = max (Just thickness) (Map.lookup W neighbors)
  wnw  = max (Map.lookup W neighbors) (Map.lookup NW neighbors)
  nwn  = max (Map.lookup NW neighbors) (Map.lookup N neighbors)
  get k = fromMaybe '?' (Map.lookup k borderChars)
  t   = get (Nothing, n, Nothing, n)
  tr  = get (nne, nee, e, n)
  r   = get (e, Nothing, e, Nothing)
  br  = get (e, ese, ses, s)
  b   = get (Nothing, s, Nothing, s)
  bl  = get (w, s, ssw, sww)
  l   = get (w, Nothing, w, Nothing)
  tl  = get (nwn, n, w, wnw)

-- A map of ASCII border drawing characters, by stroke.
-- Each glyph has four strokes (N, E, S, W) in one of three states (empty,
-- thin, or thick).
borderChars :: Map (Maybe Bool, Maybe Bool, Maybe Bool, Maybe Bool) Char
borderChars = let
  none = Nothing
  thin = Just False
  thick = Just True
  in Map.fromList
    [ (( thin,  none,  thin,  none), '│')
    , (( none,  thin,  none,  thin), '─')
    , ((thick,  none, thick,  none), '┃')
    , (( none, thick,  none, thick), '━')
    , (( none,  none,  thin,  thin), '┐')
    , (( thin,  none,  none,  thin), '┘')
    , (( none,  thin,  thin,  none), '┌')
    , (( thin,  thin,  none,  none), '└')
    , (( none,  none, thick, thick), '┓')
    , ((thick,  none,  none, thick), '┛')
    , (( none, thick, thick,  none), '┏')
    , ((thick, thick,  none,  none), '┗')
    , (( thin,  thin,  thin,  thin), '┼')
    , (( none,  thin,  thin,  thin), '┬')
    , (( thin,  none,  thin,  thin), '┤')
    , (( thin,  thin,  thin,  none), '├')
    , (( thin,  thin,  none,  thin), '┴')
    , ((thick, thick, thick, thick), '╋')
    , (( none, thick, thick, thick), '┳')
    , ((thick,  none, thick, thick), '┫')
    , ((thick, thick, thick,  none), '┣')
    , ((thick, thick,  none, thick), '┻')
    , (( thin, thick, thick,  thin), '╆')
    , ((thick, thick,  thin,  thin), '╄')
    , (( thin,  thin, thick, thick), '╅')
    , ((thick,  thin,  thin, thick), '╃')
    , (( none, thick, thick,  thin), '┲')
    , ((thick, thick,  none,  thin), '┺')
    , (( none,  thin, thick, thick), '┱')
    , ((thick,  thin,  none, thick), '┹')
    , (( thin, thick, thick,  none), '┢')
    , ((thick, thick,  thin,  none), '┡')
    , (( thin,  none, thick, thick), '┪')
    , ((thick,  none,  thin, thick), '┩')
    ]
