module Version3 (
  findLongestPalyndrome
) where

import Data.List (foldl', tails, maximumBy)
import Data.Maybe (fromMaybe, fromJust)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as MS

findLongestPalyndrome :: String -> String
findLongestPalyndrome input = BS.unpack $ helper 0 BS.empty input'
  where
    input' = BS.pack input

    charmap :: MS.Map Char [BS.ByteString]
    charmap = foldl'
                (\map string -> MS.insertWith (++) (BS.head string) [string] map)
                MS.empty
                (filter (not. BS.null) $ BS.tails $ BS.reverse input')

    helper :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
    helper n best string =
      if (BS.length string) < (BS.length best)
        then best
        else case BS.uncons string of
          Nothing -> best
          Just (h, str) ->
            let -- suppose the palyndrome starts right here, on character h.
                -- What are all possible endings a palyndrome starting right
                -- here can have?
                endings =
                  filter ((> (BS.length best)) . BS.length) $
                  map (BS.reverse . (BS.drop n) . BS.reverse) $
                  fromMaybe [] $ MS.lookup h charmap
                -- for each ending, check if we do have a palyndrome starting
                -- right here and ending with a given ending
                results =
                  filter
                    (\end -> (BS.take (BS.length end) string) == end)
                    endings
                result =
                  if null results
                    then BS.empty
                    else
                      fst $
                      maximumBy
                        (\x y -> compare (snd x) (snd y))
                        (map (\x -> (x, BS.length x)) results)
            in if (BS.length result) > (BS.length best)
              then helper (succ n) result str
              else helper (succ n) best str
