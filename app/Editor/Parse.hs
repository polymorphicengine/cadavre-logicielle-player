module Editor.Parse where

import Control.Monad (void)
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.String

type Position = (Int, Int)

data Command
  = Type String
  | Statement String
  | Definition String String String
  | Ping
  | NoCommand
  | Sit String
  | Say String
  | RemoteAddress String Int
  deriving (Show)

data Block = Block
  { bStart :: Int,
    bEnd :: Int,
    bContent :: String
  }
  deriving (Eq, Show)

-- parsing commands

runParser :: String -> Either ParseError Command
runParser = parse parseCommand ""

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

parseType :: Parser Command
parseType = do
  whitespace
  _ <- string ":t"
  s <- many anyChar
  return (Type s)

parseDefType :: Parser String
parseDefType = string "Int" <|> string "Double" <|> string "Bool" <|> string "VM" <|> string "String" <|> string "Time" <|> string "Note"

parseDef :: Parser Command
parseDef = do
  whitespace
  _ <- string ":define"
  whitespace
  l <- letter
  name <- many (letter <|> digit)
  whitespace
  t <- parseDefType
  whitespace
  c <- many anyChar
  return (Definition (l : name) t c)

parsePing :: Parser Command
parsePing = do
  whitespace
  _ <- string ":ping"
  return Ping

parseSit :: Parser Command
parseSit = do
  whitespace
  _ <- string ":sit"
  whitespace
  s <- many1 (letter <|> digit <|> char '_' <|> char '-')
  return (Sit s)

parseSay :: Parser Command
parseSay = do
  whitespace
  xs <- sepBy (many (try parser <|> pure <$> noneOf "-")) (string "--")
  return (Say $ concat $ concat xs)
  where
    parser = do
      x <- string "-"
      c <- noneOf "-"
      return $ x ++ [c]

parseRemoteAddress :: Parser Command
parseRemoteAddress = do
  whitespace
  _ <- string ":remote"
  whitespace
  add <- many (digit <|> char '.')
  _ <- string ":"
  d <- many digit
  return $ RemoteAddress add (read d)

parseStatement :: Parser Command
parseStatement = Statement <$> many1 anyChar

parseCommand :: Parser Command
parseCommand = try parseDef <|> try parseType <|> try parsePing <|> try parseSit <|> try parseSay <|> try parseRemoteAddress <|> parseStatement <|> return NoCommand

-- parsing blocks

whiteString :: String -> Bool
whiteString "" = True
whiteString (x : xs) = elem x " \t\n" && whiteString xs

linesNum :: String -> [(Int, String)]
linesNum s = zip [0 ..] (addNewLine . lines $ s)

blocks' :: [(Int, String)] -> [[(Int, String)]]
blocks' ss = case break (whiteString . snd) ss of
  ([], _ : ys) -> blocks' ys
  (xs, _ : ys) -> xs : blocks' ys
  (xs, []) -> [xs]

blocks :: [[(Int, String)]] -> [Block]
blocks [] = []
blocks ([] : _) = []
blocks (b : bs) = (Block {bStart = (fst . head) b, bEnd = (fst . last) b, bContent = concatMap snd b}) : blocks bs

getBlock :: Int -> [Block] -> Maybe Block
getBlock _ [] = Nothing
getBlock num (block@(Block n1 n2 _) : bs) = if n1 <= num && num <= n2 then Just block else getBlock num bs

getBlocks :: String -> [Block]
getBlocks = blocks . blocks' . linesNum

addNewLine :: [String] -> [String]
addNewLine [] = []
addNewLine [x] = [x]
addNewLine (x : xs) = (x ++ "\n") : addNewLine xs

getLineContent :: Int -> [(Int, String)] -> Maybe Block
getLineContent _ [] = Nothing
getLineContent num ((n, s) : ls)
  | n == num = Just $ Block n n s
  | otherwise = getLineContent num ls

-------

-- replaceTabs :: String -> String
-- replaceTabs "" = ""
-- replaceTabs ('\t' : xs) = "    " ++ replaceTabs xs
-- replaceTabs (x : xs) = x : replaceTabs xs

isValidAddress :: String -> Bool
isValidAddress x = case parse parseAddress "" x of
  Left _ -> False
  Right _ -> True

parseAddress :: Parser ()
parseAddress = do
  count 3 digit
  char '.'
  count 3 digit
  char '.'
  digit
  many1 digit
  return ()
