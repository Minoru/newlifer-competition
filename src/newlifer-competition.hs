import qualified Version1 as V1
import qualified Version2 as V2

import Control.Monad (forM_)

import Criterion.Main

tests = [
    ("abba", "abba")
  , ("xyabcdcbax", "abcdcba")
  , ("ThesampletextthatcouldbereadedthesameinbothordersArozaupalanalapuazorA"
    ,"ArozaupalanalapuazorA")
  ]

test fn = forM_ tests $ \(input, expected) -> do
  let result = fn input
  if result == expected
    then putStr "\x1b[32m✓\x1b[00m"
    else putStr "\x1b[31m✗\x1b[00m"
  putStrLn $ "  " ++ input ++ "  →  " ++ result

main = do
  putStrLn "Testing version 1"
  test V1.findLongestPalyndrome

  putStrLn "Testing version 2"
  test V2.findLongestPalyndrome

  putStrLn "Benchmarking"
  defaultMain [
      bgroup "v1" $ map ((benchmark V1.findLongestPalyndrome) . fst) tests
    , bgroup "v2" $ map ((benchmark V2.findLongestPalyndrome) . fst) tests
    ]

  where
    benchmark function string = bench string $ nf function string
