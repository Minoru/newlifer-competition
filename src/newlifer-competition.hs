import qualified Version1 as V1
import qualified Version2 as V2
import qualified Version3 as V3
import qualified Version4 as V4

import Control.DeepSeq (deepseq)
import Control.Monad (forM_)
import qualified Data.Text as T

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

  putStrLn "Testing version 3"
  test V3.findLongestPalyndrome

  putStrLn "Testing version 4"
  test (T.unpack . V4.findLongestPalyndrome . T.pack)

  putStrLn "Benchmarking"
  defaultMain [
      bgroup "v1" $ map ((benchmark V1.findLongestPalyndrome) . fst) tests
    , bgroup "v2" $ map ((benchmark V2.findLongestPalyndrome) . fst) tests
    , bgroup "v3" $ map ((benchmark V3.findLongestPalyndrome) . fst) tests
    , bgroup "v4" $
        map
          (\(input, _) ->
            let str = T.pack input
            in str `deepseq` bench input $ nf V4.findLongestPalyndrome str)
          tests
    ]

  where
    benchmark function string = bench string $ nf function string
