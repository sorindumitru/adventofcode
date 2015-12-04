import Data.Char (isSpace)
import Data.List

rstrip = reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
    input <- getContents
    print $ countHouses $ visitedHouses input
    let indexedInput = zip [0..] input
    let santaHouses = visitedHouses [ x | (index, x) <- indexedInput, even index]
    let roboHouses = visitedHouses [ x | (index, x) <- indexedInput, odd index]
    print $ countHouses (santaHouses ++ roboHouses)
    return ()

visitedHouses input = getHouses $ rstrip input

countHouses houses = length $ group $ sort houses

startState = (0,0)

getHouses input = scanl moveSanta startState input

moveSanta (x, y) '^' = (x, y - 1)
moveSanta (x, y) '>' = (x + 1, y)
moveSanta (x, y) 'v' = (x, y + 1)
moveSanta (x, y) '<' = (x - 1, y)
