import KeyGen
import Encrypt
import Decrypt
import System.IO
import Data.Char
import System.Random

keyGen = do
    pubKey <- openFile "rsa-pub.txt" ReadWriteMode
    prvKey <- openFile "rsa-prv.txt" ReadWriteMode
    randGen <- getStdGen
    putStr "Generating key, please wait...\n"
    let (p,q) = getFactors $ primeList randGen
    hPutStrLn pubKey $ getPubKey (p,q)
    hPutStrLn prvKey $ getPrvKey (p,q)
    putStr "Success!\n"
    putStr "Public Key: rsa-pub.txt\n"
    putStr "Private Key: rsa-prv.txt\n"
    hClose pubKey
    hClose prvKey

encrypt = do
    pubKey <- openFile "rsa-pub.txt" ReadMode
    key <- hGetLine pubKey
    fileName <- getLine
    contents <- readFile fileName
    putStrLn contents
    let (n,e) = read (key) :: (Integer,Integer)
    let encContents = encryptText contents (n,e)
    putStrLn encContents
    hClose pubKey

decrypt = do
    prvKey <- openFile "rsa-prv.txt" ReadMode
    key <- hGetLine prvKey
    contents <- getLine
    let (n,d) = read (key) :: (Integer,Integer)
    let decContens = decryptText contents (n, d)
    putStrLn decContens
    hClose prvKey