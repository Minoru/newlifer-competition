module Version4 (
  findLongestPalyndrome
) where

import Data.List (foldl', tails, maximumBy)
import Data.Maybe (fromMaybe, fromJust)

import qualified Data.Map.Strict as MS
import qualified Data.Text as T

findLongestPalyndrome :: T.Text -> T.Text
findLongestPalyndrome input = helper 0 T.empty input
  where
    charmap :: MS.Map Char [T.Text]
    charmap = foldl'
                (\map string -> MS.insertWith (++) (T.head string) [string] map)
                MS.empty
                (filter (not. T.null) $ T.tails $ T.reverse input)

    helper :: Int -> T.Text -> T.Text -> T.Text
    helper n best string =
      if (T.length string) < (T.length best)
        then best
        else case T.uncons string of
          Nothing -> best
          Just (h, str) ->
            let -- suppose the palyndrome starts right here, on character h.
                -- What are all possible endings a palyndrome starting right
                -- here can have?
                endings =
                  filter ((> (T.length best)) . T.length) $
                  map (T.reverse . (T.drop n) . T.reverse) $
                  fromMaybe [] $ MS.lookup h charmap
                -- for each ending, check if we do have a palyndrome starting
                -- right here and ending with a given ending
                results =
                  filter
                    (\end -> (T.take (T.length end) string) == end)
                    endings
                result =
                  if null results
                    then T.empty
                    else
                      fst $
                      maximumBy
                        (\x y -> compare (snd x) (snd y))
                        (map (\x -> (x, T.length x)) results)
            in if (T.length result) > (T.length best)
              then helper (succ n) result str
              else helper (succ n) best str
