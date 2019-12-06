{-# LANGUAGE OverloadedStrings #-}
import Algebra.Graph.Relation
import Data.Text as T
import Data.Text.Encoding
import Data.ByteString as BS
import Data.Tuple
import System.Environment
import Control.Arrow

type Orbit = (Text,Text)

parseOrbit :: Text -> (Text, Text)
parseOrbit = swap . second T.tail . T.breakOn ")"

allOrbits :: [(Text,Text)] -> Relation Text
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
 ]

main :: IO ()
main = do
  [input] <- getArgs
  orbits <- allOrbits . fmap parseOrbit . T.lines . decodeUtf8 <$> BS.readFile input
  BS.putStr $ (encodeUtf8 $ T.pack $ show $ edgeCount orbits) <> "\n"
