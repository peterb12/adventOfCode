{-# LANGUAGE OverloadedStrings #-} 
module Parsing where
import Data.Void  
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

parse = runParser

aModule :: Parser Integer
aModule = do 
    i <- lexeme L.decimal
    pure i
lModules :: Parser [Integer]
lModules = aModule `sepBy` sc

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
