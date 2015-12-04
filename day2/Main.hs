import Data.List
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

presentFile = do
        result <- many line
        eof
        return result

line = do
        length <- many1 digit
        char 'x'
        width <- many1 digit
        char 'x'
        height <- many1 digit
        char '\n' -- This should probably eol, but I can't find what to import for it
        return (read length ::Int, read width ::Int, read height ::Int)

parsePresents :: String -> Either ParseError [(Int, Int, Int)]
parsePresents input = parse presentFile "(unknown)" input

unwrapPresents :: Either ParseError [(Int, Int, Int)] -> [(Int,Int,Int)]
unwrapPresents (Left error) = [(0,0,0)]
unwrapPresents (Right xs) = xs

wrapSize presents = sum $ map wrapSizePresent presents

wrapSizePresent :: (Int, Int, Int) -> Int
wrapSizePresent (length, width, height) = 2 * length * width + 2 * width * height + 2 * height * length + 
					 foldl1 min [length *width , width * height, height * length]

ribbonSize presents = sum $ map ribbonSizePresent presents

ribbonSizePresent (length, width, height) = length * width * height + 2 * sum smallesSides
	where smallesSides = take 2 $ sort [length, width, height]

main = do
	input <- getContents
	let presents = unwrapPresents $ parse presentFile "(unknown)" input
	putStrLn $ show $ wrapSize presents
	putStrLn $ show $ ribbonSize presents
	return ()


