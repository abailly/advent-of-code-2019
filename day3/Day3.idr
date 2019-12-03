import Data.List
import Data.String
import Data.String.Views

import Debug.Error
import Debug.Trace
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

Point : Type
Point = (Integer, Integer)

l1_distance : Point -> Integer
l1_distance (x,y) = abs x + abs y

closest_to_origin : Point -> Point -> Ordering
closest_to_origin x1 x2 =
  compare (l1_distance x1) (l1_distance x2)


data Segment : Type where
  ||| A Vertical segment
  ||| x beg == x end
  V : (beg : Point) -> (end : Point) -> Segment

  ||| A Horizontal segment
  ||| y beg == y end
  H : (beg : Point) -> (end : Point) -> Segment

Show Segment where
  show (V beg end) = "V " ++ show beg ++ " " ++ show end
  show (H beg end) = "H " ++ show beg ++ " " ++ show end

start : Segment -> Point
start (V beg end) = beg
start (H beg end) = beg

segmentLength : Segment -> Integer
segmentLength (V (x1, y1) (x2, y2)) = if y2 >= y1
                                      then (y2 - y1)
                                      else (y1 - y2)
segmentLength (H (x1, y1) (x2, y2)) = if x2 >= x1
                                      then (x2 - x1)
                                      else (x1 - x2)

stop_segment_at : Segment -> Point -> Segment
stop_segment_at (V beg end) y = V beg y
stop_segment_at (H beg end) y = H beg y

||| Ensures segment end is greater than or equal to start
norm : Segment -> Segment
norm v@(V beg@(x1,y1) end@(x2,y2)) = if y1 > y2
                                     then V end beg
                                     else v
norm v@(H beg@(x1,y1) end@(x2,y2)) = if x1 > x2
                                     then H end beg
                                     else v


||| Compute the `Point` at which 2 segments intersect, if it
||| exists
||| 2 segments have an intersection iff
intersection : Segment -> Segment -> Maybe Point
intersection (V (x1, y1) (x2, y2)) (V (x3, y3) (x4, y4)) =
  if x1 == x3  -- segments are aligned
  then if y3 >= y1 && y3 <= y2
          then Just (x3 ,y3)
          else if y4 >= y1 && y4 <= y2
                 then Just (x4, y4)
                 else Nothing
  else Nothing
intersection (V (x1, y1) (x2, y2)) (H (x3, y3) (x4, y4)) =
  if x1 >= x3 && x1 <= x4 && y1 <= y3 && y2 >= y3
  then Just (x1, y3)
  else Nothing
intersection v1@(H beg end) v2@(V x y) = intersection v2 v1
intersection (H (x1, y1) (x2, y2)) (H (x3, y3) (x4, y4)) =
  if y1 == y3
  then if x3 >= x1 && x3 <= x2
       then Just (x3, y3)
       else if x4 >= x1 && x4 <= x2
            then Just (x4, y4)
            else Nothing
  else Nothing

||| Build an oriented segment given a starting point and a path
mkSegment : Point -> Path -> Segment
mkSegment p@(x,y) (R len) = H p (x + cast len, y)
mkSegment p@(x,y) (L len) = H p (x - cast len, y)
mkSegment p@(x,y) (U len) = V p (x, y + cast len)
mkSegment p@(x,y) (D len) = V p (x, y - cast len)

mkSegments : List Segment -> Path -> List Segment
mkSegments [] x = [ mkSegment (0,0) x ]
mkSegments p@((V beg end) :: xs) x = mkSegment end x :: p
mkSegments p@((H beg end) :: xs) x = mkSegment end x :: p

segments : (start : Point) -> Wire -> List Segment
segments start = foldl mkSegments []

w1 : Wire
w1 = [ R 75, D 30, R 83, U 83, L 12, D 49, R 71, U 7, L 72 ]

w2 : Wire
w2 = [ U 62, R 66, U 55, R 34, D 71, R 55, D 58, R 83 ]

sample1 : Input
sample1 = MkInput w1 w2

wire3 : Wire
wire3 = [ R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51 ]

point_on_segment : Point -> Segment -> Bool
point_on_segment (x, y) (V (x1, y1) (x2, y2)) = x == x1 && y <= y2 && y >= y1
point_on_segment (x, y) (H (x1, y1) (x2, y2)) = y == y1 && x <= x2 && x >= x1

single_step_to : Point -> (Bool, Integer) -> Segment -> (Bool, Integer)
single_step_to x (True, b) seg = (True, b) -- stop accumulating once point is reached
single_step_to x (False, b) seg =
  if point_on_segment x (norm seg)
  then trace (show x ++ " on segment " ++ show seg ++ ", len = " ++ show (b + segmentLength (stop_segment_at seg x))) $ (True, b + segmentLength (stop_segment_at seg x))
  else trace (show $ b + segmentLength (seg)) $ (False, b + segmentLength seg)

wire4 : Wire
wire4 = [ U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7 ]

||| Compute the number of steps it takes for a wire to reach
||| given point
steps_to : Wire -> Point -> Integer
steps_to w y = snd $ foldl (single_step_to y) (False, 0) $ reverse $ segments (0,0) w

sample2 : Input
sample2 = MkInput wire3 wire4

num_steps : Input -> Point -> Integer
num_steps (MkInput wire1 wire2) x = steps_to wire1 x + steps_to wire2 x

combined_number_of_steps : (input : Input) -> Point -> Point -> Ordering
combined_number_of_steps input x y =
  let tox = num_steps input x
      toy = num_steps input y
  in compare tox toy

crosswires : Input -> List Point
crosswires (MkInput wire1 wire2) =
  catMaybes [ intersection s1 s2 | s1 <- map norm (segments (0,0) wire1)
                                 , s2 <- map norm (segments (0,0) wire2)
            ]

closest_crosswire : Input -> Maybe Point
closest_crosswire input =
  head' $ filter (/= (0,0)) $ sortBy closest_to_origin $ crosswires input


lowest_number_of_steps : Input -> Maybe Point
lowest_number_of_steps input =
  head' $ filter (/= (0,0)) $ sortBy (combined_number_of_steps input) $ crosswires input

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
      putStrLn (show (num_steps inp <$> lowest_number_of_steps inp))
