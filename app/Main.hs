module Main where

import Options.Applicative
import CheckFlag
import qualified Control.Monad
import qualified Data.ByteString.Lazy as LB
import MD5 (md5sum)

data Opts = Opts
    {
        binary :: Bool
        ,check :: Bool
        ,text :: Bool
        ,files :: [String]
    }

opts :: Parser Opts
opts = Opts
    <$> switch (long "binary" <> short 'b' <> help "read in binary mode")
    <*> switch (long "check" <> short 'c' <> help "read checksums from the FILEs and check them")
    <*> switch (long "text" <> short 't' <> help "read in text mode (default)")
    <*> some (argument str (metavar "FILES..."))

main :: IO ()
main = handleParams =<< execParser params
    where
        params = info (opts <**> helper)
            (fullDesc <> progDesc "Print or check MD5 (128-bit) checksums.")

sumFile :: String -> IO ()
sumFile path = do
    fileContent <- LB.readFile path
    putStrLn $  md5sum fileContent ++ " " ++ path

handleParams :: Opts -> IO ()
handleParams (Opts _ c _ checkFileList) = do -- we must incude binary and test but these can be ignored as these do nothing on unix systems
    Control.Monad.when c $ checkFlag checkFileList
    Control.Monad.unless c $ mapM_ sumFile checkFileList




