module rsa-decrypt where 

import System.IO
import Data.Char
import rsa-encrypt
import rsa-decrypt

decrypt = do
		prvKey <- openFile "rsa-prv.txt" ReadMode
		key <- hGetLine prvKey
		contents <- getLine
		let (n,d) = read (key) :: (Integer,Integer)
		let decContens = decryptText contents (n, d)
		putStrLn decContens
		hClose prvKey
		
-- decrypts text from a string containing an encrypted [Integer] list
decryptText :: String -> (Integer,Integer) -> String
decryptText text (n,d) = decodeIntText $ map (\x -> powerMod x d n ) $ encryptedList
	where encryptedList = read text :: [Integer] 

powerMod :: (Integral a, Integral b) => a -> b -> a -> a
powerMod b e m 
	| e == 0 = 1
	| (odd e) = ( b * (powerMod (mbsq) (e `div` 2) m)) `mod` m 
	| otherwise = (powerMod (mbsq) (e `div` 2) m)
	where mbsq = (b * b) `mod` m

-- Replace each ASCII value with the appropriate char
toText :: [Int] -> [Char]
toText list = map (chr) list 

-- Create a list containing the Integer's digits in base numberBase
intToBlock :: Integer -> [Int]
intToBlock 0 = []
intToBlock i = fromIntegral (i `mod` 128) : intToBlock (i `div` 128)

-- Takes a list of integers representing some text and returns the text
decodeIntText :: [Integer] -> String 
decodeIntText list = unwords splitText 
		  where splitText = map (toText . intToBlock) list