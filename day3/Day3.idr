import Data.List
import Data.String
import Data.String.Views

import Debug.Error
import System

%flag C "-Wl,-stack_size -Wl,8000000"

data Path : Type where
  R : (len : Nat) -> Path
  L : (len : Nat) -> Path
  U : (len : Nat) -> Path
  D : (len : Nat) -> Path

Show Path where
  show (R len) = "R" ++ show len
  show (L len) = "L" ++ show len
  show (U len) = "U" ++ show len
  show (D len) = "D" ++ show len

Wire : Type
Wire = List Path

record Input where
  constructor MkInput
  wire1 : Wire
  wire2 : Wire

Show Input where
  show (MkInput w1 w2) = show w1 ++ "\n" ++ show w2

Pos : Type
Pos = (Integer, Integer)

Segment : Type
Segment = (Pos, Pos)

mkSegment : List Pos -> Path -> List Pos
mkSegment p@((x,y) :: rest) (R (S k)) = mkSegment ((x+1, y) :: p) (R k)
mkSegment p@((x,y) :: rest) (L (S k)) = mkSegment ((x-1, y) :: p) (L k)
mkSegment p@((x,y) :: rest) (U (S k)) = mkSegment ((x,   y+1) :: p) (U k)
mkSegment p@((x,y) :: rest) (D (S k)) = mkSegment ((x,   y-1) :: p) (D k)
mkSegment positions         (R Z)     = positions
mkSegment positions         (L Z)     = positions
mkSegment positions         (U Z)     = positions
mkSegment positions         (D Z)     = positions

segments : (start : Pos) -> Wire -> List Pos
segments start = foldl mkSegment [start]

wire1 : Wire
wire1 = [ R 75, D 30, R 83, U 83, L 12, D 49, R 71, U 7, L 72 ]

wire2 : Wire
wire2 = [ U 62, R 66, U 55, R 34, D 71, R 55, D 58, R 83 ]

sample1 : Input
sample1 = MkInput wire1 wire2

wire3 : Wire
wire3 = [ R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51 ]

wire4 : Wire
wire4 = [ U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7 ]

sample2 : Input
sample2 = MkInput wire3 wire4

l1_distance : Pos -> Integer
l1_distance (x,y) = abs x + abs y

closest_to_origin : Pos -> Pos -> Ordering
closest_to_origin x1 x2 =
  compare (l1_distance x1) (l1_distance x2)

closest_crosswire : Input -> Maybe Pos
closest_crosswire (MkInput wire1 wire2) = do
  p1 <- tail' $ reverse $ segments (0,0) wire1
  p2 <- tail' $ reverse $ segments (0,0) wire2
  let crossings = sortBy closest_to_origin $ intersect p1 p2
  head' crossings


parsePath : List Char -> Maybe Path
parsePath ('R'  :: xs) = R <$> parsePositive (pack xs)
parsePath ('U'  :: xs) = U <$> parsePositive (pack xs)
parsePath ('L'  :: xs) = L <$> parsePositive (pack xs)
parsePath ('D'  :: xs) = D <$> parsePositive (pack xs)
parsePath _ = Nothing

parseWire : String -> Maybe Wire
parseWire line =
  let paths = split (== ',') line
  in traverse (parsePath . unpack) paths


parseInput : Either FileError String -> Maybe Input
parseInput (Left l) = Nothing
parseInput (Right r) =
  let [ l1, l2 ] = lines r
  in MkInput <$> (parseWire l1) <*> parseWire l2

main : IO ()
main = do
  input <- parseInput <$> readFile "input"
  case input of
    Nothing => putStrLn "input format invalid"
    Just inp => do
      putStrLn (show input)
      putStrLn (show (closest_crosswire inp))
