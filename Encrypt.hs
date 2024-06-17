module Encrypt (encryptText) where
import System.IO
import Data.Char
import Util

-- Returns a string of [Integer] representing the encrypted text
encryptText :: String -> (Integer, Integer) -> String
encryptText text (n,e) = show encodedIntegers 
    where encodedIntegers = map (\x -> powerMod x e n ) $ encodeTextInt text

--Takes some text and returns a list of integers <= 350 bits each which represent it
encodeTextInt :: String -> [Integer]
encodeTextInt text = map (blockToInt 128) $ splitBlock 64 $ toBlock text 

-- Replace each char with its ASCII value
toBlock :: [Char] -> [Int]
toBlock msg = map (ord) msg

-- Split the ASCII values in blocks of size len
splitBlock :: Int -> [Int] -> [[Int]]
splitBlock _ [] = []
splitBlock len block = front : splitBlock len rest 
            where (front, rest) = splitAt len block 

-- Create an intger base "base" from a list of Ints
blockToInt :: Integer -> [Int] -> Integer
blockToInt _ [] = 0
blockToInt base (x:xs) = fromIntegral (x) + base * (blockToInt base xs)