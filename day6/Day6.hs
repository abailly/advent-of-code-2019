{-# LANGUAGE OverloadedStrings #-}
import Algebra.Graph.Relation
import Data.Set
import Data.Text as T
import Data.Text.Encoding
import Data.ByteString as BS
import Data.Tuple
import System.Environment
import Control.Arrow

type Orbit = (Text,Text)

type Orbits = Relation Text

parseOrbit :: Text -> (Text, Text)
parseOrbit = swap . second T.tail . T.breakOn ")"

allOrbits :: [(Text,Text)] -> Orbits
allOrbits = transitiveClosure . edges

sample :: [ T.Text ]
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

shortest_path :: Orbits -> Text -> Text -> (Int, [Text])
shortest_path orbits from to = (0, [])

minimum_transfers :: Orbits -> Int
minimum_transfers orbits =
  let from = elemAt 0 $ postSet "YOU" orbits
      to = elemAt 0 $ postSet "SAN" orbits
      refl = reflexiveClosure orbits
  in fst (shortest_path refl from to)

main :: IO ()
main = do
  [input] <- getArgs
  orbits <- allOrbits . fmap parseOrbit . T.lines . decodeUtf8 <$> BS.readFile input
  BS.putStr $ (encodeUtf8 $ T.pack $ show $ edgeCount orbits) <> "\n"
