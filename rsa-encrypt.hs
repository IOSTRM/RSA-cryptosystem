import System.IO
import Data.Char

encrypt = do
		pubKey <- openFile "rsa-pub.txt" ReadMode
		key <- hGetLine pubKey
		contents <- getLine
		let (n,e) = read (key) :: (Integer,Integer)
		let encContents = encryptText contents (n,e)
		putStr encContents
		hClose pubKey

-- Returns a string of [Integer] representing the encrypted text
encryptText :: String -> (Integer, Integer) -> String
encryptText text (n,e) = show encodedIntegers 
	where encodedIntegers = map (\x -> powerMod x e n ) $ encodeTextInt text

-- Returns (b^e) mod m
powerMod :: (Integral a, Integral b) => a -> b -> a -> a
powerMod b e m 
	| e == 0 = 1
	| (odd e) = ( b * (powerMod (temp) (e `div` 2) m)) `mod` m 
	| otherwise = (powerMod (temp) (e `div` 2) m)
	where temp = (b * b) `mod` m

--Takes some text and returns a list of integers <= 350 bits each which represent it
encodeTextInt :: String -> [Integer]
encodeTextInt text = map (blockToInt 128 0) $ splitBlock 64 $ toBlock text 

-- Replace each char with its ASCII value
toBlock :: [Char] -> [Int]
toBlock msg = map (ord) msg

-- Split the ASCII values in blocks of size len
splitBlock :: Int -> [Int] -> [[Int]]
splitBlock _ [] = []
splitBlock len msgBlock = frontBlock : splitBlock len restOfTheBlock 
			 where frontBlock = take len msgBlock
			       restOfTheBlock = drop len msgBlock 

-- Create an intger base "base" from a list of Ints
blockToInt :: Integer -> Integer -> [Int] -> Integer
blockToInt _ _ [] = 0
blockToInt base exp (x:xs) = (fromIntegral (x) * base^exp) + (blockToInt base (exp+1) xs)