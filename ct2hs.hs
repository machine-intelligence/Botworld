module Main where
import Prelude hiding (lines)
import Botworld hiding (limit, Error(..), memory, contents)
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List (elemIndex)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec hiding ((<|>), many, spaces)
import Text.Parsec.String (Parser)
import Text.Printf (printf)

type Name = String
type Size = Maybe Int
type Declaration = (Name, Size, Value)
data Value = Code [INSTRUCTION] | Data Constree deriving Show

-- These are the instructions recognized in ct files.
-- COPY, EXEC, and WRITE require that you have a register named NIL.
-- If you want the instructions to act like they sound, NIL should always
-- contain exactly Nil.
data INSTRUCTION
  = NILIFY    Name
  | CONS      Name Name Name
  | DEST      Name Name Name
  | COPY      Name Name
  | CONDCOPY  Name Name Name
  | EXEC      Name
  | CONDEXEC  Name Name
  | PUSH      Name Name
  | POP       Name Name
  | WRITE     Name
  | CONDWRITE Name Name
  deriving Show

data Error = NoSuchRegister Name | DuplicateDeclaration Name deriving Show
type File = [Declaration]

names :: File -> [Name]
names f = [n | (n, _, _) <- f]

validate :: [Declaration] -> Either Error File
validate decls = case safeSet $ names decls of
  Left dup -> Left $ DuplicateDeclaration dup
  Right _ -> Right decls

safeSet :: Ord k => [k] -> Either k (Set k)
safeSet = foldM safeInsert Set.empty where
  safeInsert s x = if Set.member x s then Left x else Right $ Set.insert x s

convert :: File -> Either Error [Register]
convert f = mapM compile f where
  compile (_, s, Code l) = (make s . encode) <$> mapM instruct l
  compile (_, s, Data t) = Right $ make s t
  make Nothing t = R (size t) t
  make (Just s) t = R s (trim s t)
  instruct (NILIFY tgt)           = Nilify      <$> r tgt
  instruct (CONS fnt bck tgt)     = Construct   <$> r fnt   <*> r bck <*> r tgt
  instruct (DEST src fnt bck)     = Deconstruct <$> r src   <*> r fnt <*> r bck
  instruct (COPY src dst)         = CopyIfNil   <$> r "NIL" <*> r src <*> r dst
  instruct (CONDCOPY tst src dst) = CopyIfNil   <$> r tst   <*> r src <*> r dst
  instruct (EXEC prg)             = CopyIfNil   <$> r "NIL" <*> r prg <*> return 0
  instruct (CONDEXEC tst prg)     = CopyIfNil   <$> r tst   <*> r prg <*> return 0
  instruct (PUSH src stk)         = Construct   <$> r src   <*> r stk <*> r stk
  instruct (POP stk tgt)          = Deconstruct <$> r stk   <*> r tgt <*> r stk
  instruct (WRITE src)            = CopyIfNil   <$> r "NIL" <*> r src <*> return 2
  instruct (CONDWRITE tst src)    = CopyIfNil   <$> r tst   <*> r src <*> return 2
  r x = maybe (Left $ NoSuchRegister x) Right (elemIndex x $ names f)

file :: Parser [Declaration]
file = many declaration

spaces :: Parser ()
spaces = void $ many $ satisfy isSpace

spaces1 :: Parser ()
spaces1 = void $ many1 $ satisfy isSpace

declaration :: Parser Declaration
declaration = (,,) <$> h <*> s <*> v where
    h = bullshit *> name
    s = spaces *> limit <* spaces <* char ':' <* spaces <* bullshit
    v = try (Data <$> (tdata <* bullshit)) <|> (Code <$> many (instruction <* bullshit)) <?> "a value"

bullshit :: Parser ()
bullshit = void $ many ignored where
    ignored = try spaces1 <|> try comment <?> "ignored text"

comment :: Parser ()
comment = char ';' *> skipMany (noneOf "\r\n")

limit :: Parser Size
limit = optionMaybe (read <$> many1 digit)

name :: Parser Name
name = (:) <$> satisfy isAlpha_ <*> many (satisfy isAlphaNum_) where
  isAlpha_ = (||) <$> isAlpha <*> (== '_')
  isAlphaNum_ = (||) <$> isAlphaNum <*> (== '_')

instruction :: Parser INSTRUCTION
instruction
  =   try (string "CONS"      *> (CONS      <$> ref <*> ref <*> ref))
  <|> try (string "DEST"      *> (DEST      <$> ref <*> ref <*> ref))
  <|> try (string "COPY"      *> (COPY      <$> ref <*> ref))
  <|> try (string "CONDCOPY"  *> (CONDCOPY  <$> ref <*> ref <*> ref))
  <|> try (string "EXEC"      *> (EXEC      <$> ref))
  <|> try (string "CONDEXEC"  *> (CONDEXEC  <$> ref <*> ref))
  <|> try (string "PUSH"      *> (PUSH      <$> ref <*> ref))
  <|> try (string "POP"       *> (POP       <$> ref <*> ref))
  <|> try (string "WRITE"     *> (WRITE     <$> ref))
  <|> try (string "CONDWRITE" *> (CONDWRITE <$> ref <*> ref))
  <?> "an instruction"
  where ref = spaces *> name <* spaces

encreg :: Parser Register
encreg = R <$> (string "R" *> spaces1 *> int <* spaces1) <*> tdata

memory :: Parser Memory
memory = listOf encreg

int :: Parser Int
int = read <$> many1 digit

direction :: Parser Direction
direction
  =   try (string "N" *> pure N)
  <|> try (string "NE" *> pure NE)
  <|> try (string "E" *> pure E)
  <|> try (string "SE" *> pure SE)
  <|> try (string "S" *> pure S)
  <|> try (string "SW" *> pure SW)
  <|> try (string "W" *> pure W)
  <|> try (string "NW" *> pure NW)
  <?> "a direction"

command :: Parser Command
command
  =   try (Move    <$> (string "Move" *> spaces1 *> direction))
  <|> try (Lift    <$> (string "Lift" *> spaces1 *> int))
  <|> try (Drop    <$> (string "Drop" *> spaces1 *> int))
  <|> try (Inspect <$> (string "Inspect" *> spaces1 *> int))
  <|> try (Destroy <$> (string "Destroy" *> spaces1 *> int))
  <|> try (Build   <$> (string "Build" *> spaces1 *> listOf int <* spaces1) <*> memory)
  <|> try (string "Pass" *> pure Pass)
  <?> "a command"

instr :: Parser Instruction
instr
  =   try (Nilify <$> (string "Nilify" *> spaces1 *> int))
  <|> try (Construct <$> (string "Construct" *> spaces1 *> int <* spaces1) <*> (int <* spaces1) <*> int)
  <|> try (Deconstruct <$> (string "Deconstruct" *> spaces1 *> int <* spaces1) <*> (int <* spaces1) <*> int)
  <|> try (CopyIfNil <$> (string "CopyIfNil" *> spaces1 *> int <* spaces1) <*> (int <* spaces1) <*> int)
  <?> "a literal instruction"

constree :: Parser Constree
constree
  =   try (string "Nil" *> pure Nil)
  <|> try (Cons <$> (string "Cons" *> spaces1 *> constree <* spaces1) <*> constree)
  <?> "some constree"

listOf :: Parser a -> Parser [a]
listOf p = bracketed (sepBy p (spaces *> char ',' <* spaces)) where
  bracketed = between (char '[' *> spaces) (spaces <* char ']')

treeTuple :: Parser Constree
treeTuple = treeify <$> bracketed (sepBy1 tdata (spaces *> char ',' <* spaces)) where
  bracketed = between (char '(' *> spaces) (spaces <* char ')')
  treeify [] = error "sepBy1 failure"
  treeify [x] = x
  treeify (x:xs) = Cons x (treeify xs)

tdata :: Parser Constree
tdata
  = try (encode <$> int)
  <|> try (encode <$> command)
  <|> try (encode <$> instr)
  <|> try constree
  <|> try treeTuple
  <|> try (encode <$> listOf tdata)
  <?> "valid data"

load :: String -> String -> Either String File
load fname txt = do
  decls <- left show $ parse file fname txt
  left show $ validate decls

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ do
    hPutStrLn stderr "Not enough arguments. (Try ct2hs --help.)"
    exitFailure
  let basename = head args
  when (basename == "-h" || basename == "--help") $ do
    putStrLn "ct2hs NAME [MODULE]"
    putStrLn ""
    putStrLn "NAME should specify a .ct file WITHOUT the .ct extension."
    putStrLn "MODULE determines the haskell module name; it defaults to NAME."
    putStrLn "Output will be written to stdout; redirect it as neccessary."
    putStrLn ""
    putStrLn "example:"
    putStrLn " > ct2hs Omega Precommit.Omega > Omega.hs"
    exitSuccess
  when (length args > 2) $ do
    hPutStrLn stderr "Too many arguments. (Try ct2hs --help.)"
    exitFailure
  let modname = if length args > 1 then args !! 1 else basename
  let filename = basename <> ".ct"
  contents <- readFile filename
  case load filename contents of
    Left e -> putStrLn e
    Right f -> either print (printModule modname $ names f) (convert f)

template :: String
template = unlines
  [ "module %s (machine, registerNames, inspect, view) where"
  , "import Botworld"
  , "import Botworld.Debug"
  , ""
  , "machine :: Memory"
  , "machine = %s"
  , ""
  , "registerNames :: [String]"
  , "registerNames = %s"
  , ""
  , "inspect :: Memory -> [String]"
  , "inspect = inspectMemory registerNames"
  , ""
  , "view :: Robot -> IO ()"
  , "view = putStrLn . unlines . inspect . memory"
  ]

moduleFor :: String -> [String] -> Memory -> String
moduleFor modname rs m = printf template modname (show m) (show rs)

printModule :: String -> [String] -> Memory -> IO ()
printModule modname rnames m = putStr (moduleFor modname rnames m)
