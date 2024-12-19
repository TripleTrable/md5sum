module CheckFlag where

import qualified Data.ByteString.Lazy as LB
import MD5 (md5sum)

checkFile :: String -> IO String
checkFile path = do
    putStrLn $ "checkFile: " ++ path
    fileContent <- LB.readFile path

    return (md5sum fileContent)


checkFlag :: [String] -> IO ()
checkFlag strings = do
    print strings
    mapM_ checkFile strings

