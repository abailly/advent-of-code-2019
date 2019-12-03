data Path : Type where
  R : (len : Nat) -> Path
  L : (len : Nat) -> Path
  U : (len : Nat) -> Path
  D : (len : Nat) -> Path

Wire : Type
Wire = List Path

record Input where
  constructor MkInput
  wire1 : Wire
  wire2 : Wire

Pos : Type
Pos = (Integer, Integer)

Segment : Type
Segment = (Pos, Pos)

mkSegment : (start : Pos) -> List Segment -> Path -> List Segment

segments : (start : Pos) -> Wire -> List Segment
segments start = foldl mkSegment [(start, start)]

sample1 : Input
sample1 = MkInput [ R 75, D 30, R 83, U 83, L 12, D 49, R 71, U 7, L 72 ]
                  [ U 62, R 66, U 55, R 34, D 71, R 55, D 58, R 83 ]
