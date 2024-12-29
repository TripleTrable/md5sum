module CheckFlag where

import qualified Data.ByteString.Lazy as LB
import MD5 (md5sum)
import qualified Data.ByteString.Lazy.Char8 as LC

handleEntry :: LB.ByteString -> IO ()
handleEntry dat = do
    let tokens = LC.words dat
    fileContent <- LB.readFile $ LC.unpack (tokens !! 1)
    let calcMD = md5sum fileContent
    let entryMD = LC.unpack (head tokens)

    putStr $ LC.unpack (tokens !! 1) ++ ": "
    if calcMD == entryMD then
        putStrLn "OK"
    else
        putStrLn "Failed"

checkEntry :: LB.ByteString -> IO ()
checkEntry entry = do
    if entry == LB.empty then
        return ()
    else
        handleEntry entry


checkFile :: String -> IO String
checkFile path = do
    fileContent <- LB.readFile path
    let lineArray = LB.split 10 fileContent -- 10 is \n
    mapM_ checkEntry lineArray
    return (md5sum fileContent)


checkFlag :: [String] -> IO ()
checkFlag strings = do
    mapM_ checkFile strings

