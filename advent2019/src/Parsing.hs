{-# LANGUAGE OverloadedStrings #-} 
module Parsing where
import Data.Char
import Data.Void  
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Debug.Trace
type Parser = Parsec Void T.Text

parse = runParser

asteroid :: Parser Bool
asteroid = do
  item <- (char '#') <|> (char '.')
  traceM [item]
  let result = case item of
        '#' -> True
        '.' -> False
  return result

asteroids :: Parser [Bool]
asteroids = traceShow "ast" $ some asteroid

sector :: Parser [[Bool]]
sector = traceShow "hi" $ asteroids `sepEndBy` sc

digs :: Parser [Char]
digs = many digitChar

integer :: Parser Integer
integer = do 
    i <- L.signed sc (lexeme L.decimal)
    pure i

int :: Parser Int
int = do
    i <- L.signed sc (lexeme L.decimal)
    pure i
    
lModules :: Parser [Integer]
lModules = integer `sepBy` sc

csvInt :: Parser [Int]
csvInt = int `sepBy` (char ',')

csvInteger :: Parser [Integer]
csvInteger = integer `sepBy` (char ',')

data Direction = Up | Down | Lft | Rght deriving Show
data Motion = M Direction Int deriving Show

motion :: Parser Motion
motion = do
  dirGlyph <- (char 'D') <|> char 'U' <|> char 'L' <|> char 'R'
  mag <- int
--  let dir = if dirGlyph == 'D' then Down else Up
  let dir = case dirGlyph of
        'D' -> Down
        'U' -> Up
        'L' -> Lft
        'R' -> Rght
        _   -> error "Unexpected direction"
  return (M dir mag)

turtle :: Parser [Motion]
turtle =  motion `sepBy` (char ',')

turtles :: Parser ([Motion], [Motion])
turtles = do
  a <- turtle
  b <- turtle
  return (a,b)

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Day 6 parsing
orbit :: Parser (T.Text, T.Text)
orbit = do
  a <- T.pack <$> some alphaNumChar
  char ')'
  b <- T.pack <$> some alphaNumChar
  sc
  return (b,a)

orbits :: Parser [(T.Text, T.Text)]
orbits = do
  a <- some orbit
  return a
