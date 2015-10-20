import Control.Monad (forM_)
import Data.List (foldl', tails, maximumBy)
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as MS

findLongestPalyndrome :: String -> String
findLongestPalyndrome input = helper 0 0 input
  where
    charmap :: MS.Map Char [String]
    charmap = foldl'
                (\map string -> MS.insertWith (++) (head string) [string] map)
                MS.empty
                (filter (not.null) $ tails $ reverse input)

    helper :: Int -> Int -> String -> String
    helper _ _ "" = ""
    helper n best string@(h:str)
      | length string < best = ""
      | otherwise =
        let alternative = helper (succ n) best str

            -- suppose the palyndrome starts right here, on character h. What are
            -- all possible endings a palyndrome starting right here can have?
            endings =
              filter ((> best) . length) $
              map (reverse . (drop n) . reverse) $
              fromMaybe [] $
              MS.lookup h charmap
            -- for each ending, check if we do have a palyndrome starting right
            -- here and ending with a given ending
            results =
              filter
                (\end -> (take (length end) string) == end)
                endings
            result =
              if null results
                then ""
                else
                  fst $
                  maximumBy
                    (\x y -> compare (snd x) (snd y))
                    (map (\x -> (x, length x)) results)
        in if (length alternative) > (length result)
          then alternative
          else result

tests = [
    "abba"
  , "xyabcdcbax"
  , "ThesampletextthatcouldbereadedthesameinbothordersArozaupalanalapuazorA"
  ]

isPalindrome :: String -> Bool
isPalindrome = reverse >>= (==)

main = forM_ tests $ \input -> do
  let result = findLongestPalyndrome input
  if isPalindrome result
    then putStr "\x1b[32m✓\x1b[00m"
    else putStr "\x1b[31m✗\x1b[00m"
  putStrLn $ "  " ++ input ++ "  →  " ++ result

