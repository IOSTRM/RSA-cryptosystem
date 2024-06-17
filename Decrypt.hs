module Decrypt (decryptText) where
import System.IO
import Data.Char
import Util

-- decrypts text from a string containing an encrypted [Integer] list
decryptText :: String -> (Integer,Integer) -> String
decryptText text (n,d) = decodeIntText $ map (\x -> powerMod x d n ) $ encryptedList
    where encryptedList = read text :: [Integer] 

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