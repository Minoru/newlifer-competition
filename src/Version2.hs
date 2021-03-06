module Version2 (
  findLongestPalyndrome
) where

import Data.List (foldl', tails, maximumBy)
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as MS

findLongestPalyndrome :: String -> String
findLongestPalyndrome input = helper 0 "" input
  where
    charmap :: MS.Map Char [String]
    charmap = foldl'
                (\map string -> MS.insertWith (++) (head string) [string] map)
                MS.empty
                (filter (not.null) $ tails $ reverse input)

    helper :: Int -> String -> String -> String
    helper _ best "" = best
    helper n best string@(h:str)
      | length string < length best = best
      | otherwise =
        let -- suppose the palyndrome starts right here, on character h. What are
            -- all possible endings a palyndrome starting right here can have?
            endings =
              filter ((> (length best)) . length) $
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
        in if (length result) > (length best)
          then helper (succ n) result str
          else helper (succ n) best str
