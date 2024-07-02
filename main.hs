import KeyGen
import Encrypt
import Decrypt
import System.IO
import Data.Char
import System.Random
import System.Environment (getArgs)
import Control.Monad (forM_)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-gen-keys"]            -> keyGen
        ["-encrypt", fileName]  -> encrypt fileName
        ["-decrypt", fileName]  -> decrypt fileName
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
    let (n, e) = read key :: (Integer, Integer)
    contents <- readFile fileName
    let linesOfFile = lines contents
    forM_ linesOfFile $ \line -> do
        let encLine = encryptText line (n, e)
        putStrLn encLine

decrypt :: String -> IO ()
decrypt fileName = do
    key <- readFile "rsa-prv.txt"
    contents <- readFile fileName
    let (n,d) = read key :: (Integer,Integer)
    putStrLn "your decrypted message: "
    let linesOfFile = lines contents
    forM_ linesOfFile $ \line -> do
        let decLine = decryptText line (n, d)
        putStrLn decLine
