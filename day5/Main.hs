import Data.Char (isSpace)
import Data.List
import qualified Data.Text as T

rstrip = reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
    input <- getContents
    let words = lines input
    putStrLn $ show $ length $ filter (\x -> x) $ map isNice words
    putStrLn $ show $ length $ filter (\x -> x) $ map isBetterNice words
    return ()

isNice word = (hasVowels word) && (hasDouble word) && (not $ hasNaughty word)

hasVowels word = (length $ filter isVowel word) >= 3

isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False

hasDouble word = (length $ filter (\x -> length x > 1) $ group word) > 0

hasNaughty word = (length (filter (\x -> x `isInfixOf` word) ["ab", "cd", "pq", "xy"])) > 0

isBetterNice word = (hasDoublePairOf word [ x:y:[] | x <- ['a'..'z'], y <- ['a'..'z']]) && hasTriplet word

hasDoublePairOf word pairs = (length $ filter (hasDoublePair word) pairs) > 0

hasDoublePair word pair = (length $ T.splitOn (T.pack pair) (T.pack word)) >= 3

hasTriplet :: String -> Bool
hasTriplet (x:y:z:xs)
           | x == z = True
           | otherwise = hasTriplet (y:z:xs)
hasTriplet _ = False
