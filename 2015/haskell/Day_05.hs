module Day_05
( partOne
, partTwo
) where

-- import Data.List (group, isInfixOf)
import Text.Regex.Posix ((=~))

partOne :: IO Int
partOne = process isNice

partTwo :: IO Int
partTwo = process isNicer

process :: (String -> Bool) -> IO Int
process f = length . filter f . lines <$> readFile "../input/05.txt"

-- | Part One, using list manipulation and/or regex
-- A nice string is one with all of the following properties:
--
-- >>> isNice "ugknbfddgicrmopn" -- is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
-- True
-- >>> isNice "aaa"              -- is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
-- True
-- >>> isNice "jchzalrnumimnmhp" -- is naughty because it has no double letter.
-- False
-- >>> isNice "haegwjzuvuyypxyu" -- is naughty because it contains the string xy.
-- False
-- >>> isNice "dvszwmarrgswjxmb" -- is naughty because it contains only one vowel.
-- False
--
isNice :: String -> Bool
isNice xs = and $ ($ xs) <$> [hasTreePlusVowels, hasTwice, hasNoBadPair]

-- | Contains at least three vowels (aeiou only)
-- >>> and $ hasTreePlusVowels <$> ["aei", "xazegov", "aeiouaeiouaeiou"]
-- True
--
hasTreePlusVowels :: String -> Bool
hasTreePlusVowels = flip (=~) "[aeiou].*[aeiou].*[aeiou]"
-- hasTreePlusVowels = (>= 3) . length . filter (`elem` "aeiou") . map toLower

-- | Contains at least one letter that appears twice in a row
-- >>> and $ hasTwice <$> ["xx","abcdde","aabbccdd"]
-- True
-- >>> hasTwice "aaa"  -- 2nd test case imply >= 2 is also ok
-- True
--
hasTwice :: String -> Bool
hasTwice = flip (=~) "(.)\\1"
-- hasTwice = any ((>= 2) . length) . group


-- | Does not contain the strings ab, cd, pq, or xy,
--   even if they are part of one of the other requirements.
-- >>> or $ hasNoBadPair <$> ["iiabii","jjcdjj","kkpqkk","llxyll"]
-- False
-- >>> hasNoBadPair "acbdpxqy"
-- True
--
hasNoBadPair :: String -> Bool
hasNoBadPair = not . flip (=~) "(ab|cd|pq|xy)"
-- hasNoBadPair xs = not $ any (`isInfixOf` xs) bads
--   where bads = ["ab","cd","pq","xy"]


-- | Part Two, require capture back-referencing, need right tool (Regex) for the right job
-- >>> isNicer "qjhvhtzxzqqjkmpb" -- is nice because is has a pair that appears twice (qj) and a letter that repeats with exactly one letter between them (zxz).
-- True
-- >>> isNicer "xxyxx"            -- is nice because it has a pair that appears twice and a letter that repeats with one between, even though the letters used by each rule overlap.
-- True
-- >>> isNicer "uurcxstgmygtbstg" -- is naughty because it has a pair (tg) but no repeat with a single letter between them.
-- False
-- >>> isNicer "ieodomkazucvgmuy" -- is naughty because it has a repeating letter with one between (odo), but no pair that appears twice.
-- False
--
isNicer :: String -> Bool
isNicer xs = and $ ($ xs) <$> [hasTwiceDouble, hasRepeatBtwn]

-- | It contains a pair of any two letters that appears at least twice in the string
-- without overlapping,
-- >>> hasTwiceDouble "xyxy"       -- (xy)
-- True
-- >>> hasTwiceDouble "aabcdefgaa" -- (aa)
-- True
-- >>> hasTwiceDouble "aaa"        -- but not like aaa (aa, but it overlaps).
-- False
--
hasTwiceDouble :: String -> Bool
hasTwiceDouble = flip (=~) "(..).*\\1"

-- | It contains at least one letter which repeats with exactly one letter between them,
-- >>> hasRepeatBtwn "xyx"        -- x
-- True
-- >>> hasRepeatBtwn "abcdefeghi" -- (efe)
-- True
-- >>> hasRepeatBtwn "aaa"
-- True
--
hasRepeatBtwn :: String -> Bool
hasRepeatBtwn = flip (=~) "(.).\\1"

