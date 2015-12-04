import qualified Crypto.Hash as Hash
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (isSpace)
import Data.List (isPrefixOf)

rstrip = reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
    input <- getContents
    print $ take 1 $ dropWhile fiveZeroes $ processPrefix (rstrip input) [0..]
    print $ take 1 $ dropWhile sixZeroes $ processPrefix (rstrip input) [0..]
    return ()

fiveZeroes :: (Int, String) -> Bool
fiveZeroes (_, lst) = not $ isPrefixOf "00000" lst

-- ugh
sixZeroes :: (Int, String) -> Bool
sixZeroes (_, lst) = not $ isPrefixOf "000000" lst

processPrefix :: String -> [Int] -> [(Int, String)]
processPrefix key (x:xs) = (x, md5Hash key x) : processPrefix key xs
processPrefix key [] = []

md5Hash key x = show (Hash.hash (pack $ (key ++ (show x))) :: Hash.Digest Hash.MD5)
