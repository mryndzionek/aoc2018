module Days.Day8
  ( day8
  ) where

import Control.Monad
import Data.Either (fromRight)
import Data.Maybe (mapMaybe)
import Data.Void
import qualified Data.List.Safe as S

import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lx
import Text.Megaparsec hiding (State, parse, count)

import Util

lexeme :: Parser a -> Parser a
lexeme = Lx.lexeme space

integer :: Parser Int
integer = lexeme Lx.decimal

sum1 :: Parser Int
sum1 = do
  nChild <- integer
  nMeta <- integer
  childr <- sum <$> replicateM nChild sum1
  meta <- sum <$> replicateM nMeta integer
  pure $ childr + meta

sum2 :: Parser Int
sum2 = do
  nChild <- integer
  nMeta <- integer
  childr <- replicateM nChild sum2
  meta <- replicateM nMeta integer
  pure $
    if null childr
      then sum meta
      else sum . mapMaybe (\i -> childr S.!! (i - 1)) $ meta

parse :: Parser Int -> String -> Either (ParseErrorBundle String Void) Int
parse p = runParser p ""

day8 :: Str -> (Int, Int)
day8 (Str input) =
  let resp = (,) <$> parse sum1 input <*> parse sum2 input
   in fromRight (0, 0) resp
