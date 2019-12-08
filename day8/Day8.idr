%include C "image.h"
%link C "image.o"

data Pixel : Type where
  P0 : Pixel
  P1 : Pixel
  P2 : Pixel

record Image  where
  constructor MkImage
  ||| Underlying data stored as pointer
  _data : Ptr
  ||| Size of layers
  _size : (Nat, Nat)

Show Image where
  show (MkImage d (w,h)) = "(" ++ show w ++ "," ++ show h ++ ")"

||| Build image using C low-level routine
mkImage : List Pixel -> (width : Nat) -> (height : Nat) -> IO Image
mkImage pixels w h = do
  p <- foreign FFI_C "image_alloc" (Int -> IO Ptr) (cast $ List.length pixels)
  pure $ MkImage p (w, h)


main : IO ()
main = do
  img <- mkImage [ P0,P0 ] 2 1
  print img
