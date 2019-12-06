{-# LANGUAGE OverloadedStrings #-}
import           Algebra.Graph.Relation
import           Control.Arrow
import qualified Data.ByteString        as BS
import           Data.Sequence          (Seq ((:<|)), (><))
import qualified Data.Sequence          as S
import           Data.Set               hiding (foldl)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding
import           Data.Tuple
import           System.Environment

type Orbit = (Text,Text)

type Orbits = Relation Text

parseOrbit :: Text -> (Text, Text)
parseOrbit = swap . second T.tail . T.breakOn ")"

allOrbits :: [(Text,Text)] -> Orbits
allOrbits = transitiveClosure . edges

sample :: [ Text ]
sample =
 [ "COM)B"
 , "B)C"
 , "C)D"
 , "D)E"
 , "E)F"
 , "B)G"
 , "G)H"
 , "D)I"
 , "E)J"
 , "J)K"
 , "K)L"
 , "K)YOU"
 , "I)SAN"
 ]

sample_orbits = allOrbits $ fmap parseOrbit sample

newtype Path = Path [ Text ]
  deriving (Eq, Show)

instance Ord Path where
  Path p1 `compare` Path p2 =
    case compare (length p1) (length p2) of
      LT -> LT
      GT -> GT
      EQ -> compare p1 p2

bfs :: Orbits -> Text -> S.Seq Path -> Set Text -> [Text]
bfs orbits to S.Empty _ = []
bfs orbits to (Path p@(h : t) :<| rest) visited =
  if h == to
  then reverse p
  else bfs orbits to (rest >< neighbours p) (h `insert` visited)

  where
    neighbours p@(h : t) =
      foldl (\ ps x -> Path (x:p) :<| ps) S.empty (postSet h orbits \\ visited)

shortest_path :: Orbits -> Text -> Text -> [Text]
shortest_path orbits from to = bfs orbits to (S.singleton $ Path [from]) mempty

minimum_transfers :: Orbits -> (Int, [Text])
minimum_transfers orbits =
  let from = elemAt 0 $ postSet "YOU" orbits
      to = elemAt 0 $ postSet "SAN" orbits
      refl = symmetricClosure orbits
      path = shortest_path refl from to
  in (length path - 1, path)

main :: IO ()
main = do
  [input] <- getArgs
  orbits <- edges . fmap parseOrbit . T.lines . decodeUtf8 <$> BS.readFile input
  BS.putStr $ encodeUtf8 (T.pack $ show $ minimum_transfers orbits) <> "\n"
