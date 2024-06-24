import KeyGen
import Encrypt
import Decrypt
import System.IO
import Data.Char
import System.Random
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("-gen-keys" : _)            -> keyGen
        ("-encrypt" : fileName : _)  -> encrypt fileName
        ("-decrypt" : fileName : _)  -> decrypt fileName
        _                            -> putStrLn "Invalid command. Use -gen-keys or -encrypt <file> or -decrypt <file>"


keyGen = do
    randGen <- getStdGen
    putStr "Generating key, please wait...\n"
    let (p,q) = getFactors $ primeList randGen
    writeFile "rsa-pub.txt" (getPubKey (p, q) ++ "\n")
    writeFile "rsa-prv.txt" (getPrvKey (p, q) ++ "\n")
    putStr "Success!\n"
    putStr "Public Key: rsa-pub.txt\n"
    putStr "Private Key: rsa-prv.txt\n"

encrypt :: String -> IO ()
encrypt fileName = do
    key <- readFile "rsa-pub.txt"
    contents <- readFile fileName
    let (n, e) = read key :: (Integer,Integer)
    let encContents = encryptText contents (n,e)
    putStrLn encContents

decrypt :: String -> IO ()
decrypt fileName = do
    key <- readFile "rsa-prv.txt"
    contents <- readFile fileName
    putStrLn contents
    let (n,d) = read key :: (Integer,Integer)
    let decContens = decryptText contents (n, d)
    putStrLn "your decrypted message: "
    putStrLn decContens
