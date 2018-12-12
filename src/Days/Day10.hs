module Days.Day10
  ( day10
  , day10Draw
  ) where

import Control.Lens hiding ((#))
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Void

import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lx

import Diagrams.Backend.Rasterific
import qualified Diagrams.Prelude as D
import Diagrams.Prelude ((#))

import Util

data Point = Point
  { _posX :: Int
  , _posY :: Int
  , _vx :: Int
  , _vy :: Int
  } deriving (Show, Eq, Ord)

type Parser = Parsec Void String
type Diagram = D.QDiagram B D.V2 Double D.Any

sc :: Parser ()
sc = Lx.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = Lx.lexeme sc

integer :: Parser Int
integer = lexeme Lx.decimal

signedInteger :: Parser Int
signedInteger = Lx.signed space integer

symbol :: String -> Parser String
symbol = Lx.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "<") (symbol ">")

point :: Parser Point
point = do
  _ <- symbol "position="
  p <-
    parens $ do
      px <- signedInteger
      _ <- symbol ","
      py <- signedInteger
      return (px, py)
  _ <- symbol "velocity="
  v <-
    parens $ do
      vx <- signedInteger
      _ <- symbol ","
      vy <- signedInteger
      return (vx, vy)
  return $ uncurry (uncurry Point p) v

parsePoints :: String -> [Point]
parsePoints s = fromMaybe [] $ mapM (parseMaybe point) (lines s)

move :: Point -> Point
move p = Point (_posX p + _vx p) (_posY p + _vy p) (_vx p) (_vy p)

drawPoint :: Point -> (D.P2 Double, Diagram)
drawPoint p =
  ( D.p2 (5 * fromIntegral (_posX p), -5 * fromIntegral (_posY p))
  , D.circle (fromIntegral (5 :: Int)) # D.lw D.none #
    D.fc D.white # D.opacity 0.7)

draw :: [Diagram] -> IO ()
draw = animatedGif "images/day10.gif" (D.mkWidth 1000) LoopingForever 100

midnightBlue :: D.Colour Double
midnightBlue = D.sRGB24read "#191970"

toDiagram :: [Point] -> Diagram
toDiagram d = D.position (map drawPoint d) :: D.Diagram B

area :: [Point] -> Int
area pts =
  let xs = map _posX pts
      ys = map _posY pts
      w = maximum xs - minimum xs
      h = maximum ys - minimum ys
   in w * h

day10 :: Str -> (String, Int)
day10 (Str input) =
  let points = parsePoints input
      steps = iterate (map move) points
      (n, _) = nWith 1 (<) (area . snd) $ zip [0 ..] steps
   in ("EKALLKLB", n)

textAt :: Double -> Double -> String -> Diagram
textAt x y t = D.position [(D.p2 (x, y), D.text t # D.fontSize 20 # D.fc D.white)]

day10Draw :: Str -> IO ()
day10Draw (Str input) =
  let points = parsePoints input
      steps = zip [0 ..] $ iterate (map move) points
      (n, _) = nWith 1 (<) (area . snd) steps
      selected = take 20 $ drop (n - 10) steps
      frames = map (_2 `over` toDiagram) selected
      e = view D.envelope $ snd $ head frames
   in draw $
      map
        (\(n', d) ->
           textAt 1000 (-1000) (show n') <> D.setEnvelope e d #
           D.bg midnightBlue)
        frames

