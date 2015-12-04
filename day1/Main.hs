main :: IO ()
main = do
	input <- getContents
	putStrLn $ getFloor input
	putStrLn $ getBasementPos input
	return ()

getModifier '(' = 1
getModifier ')' = -1
getModifier _ = 0

getFloor input = show $ sum $ map getModifier input

getBasementPos input = show $ (head $ [index | (index, e) <- zip [0..] sums, e < 0]) + 1
	where sums = scanl1 (+) (map getModifier input)
