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
        ,tag :: Bool
        ,text :: Bool
        ,zero :: Bool
        ,files :: [String]
    }

opts :: Parser Opts
opts = Opts
    <$> switch (long "binary" <> short 'b' <> help "read in binary mode")
    <*> switch (long "check" <> short 'c' <> help "read checksums from the FILEs and check them")
    <*> switch (long "tag" <>  help "create a BSD-style checksum")
    <*> switch (long "text" <> short 't' <> help "read in text mode (default)")
    <*> switch (long "zero" <> short 'z' <> help "end each output line with NUL, not newline, and disable file name escaping")
    <*> some (argument str (metavar "FILES..."))

main :: IO ()
main = handleParams =<< execParser params
    where
        params = info (opts <**> helper)
            (fullDesc <> progDesc "Print or check MD5 (128-bit) checksums.")

sumFile :: String -> IO ()
sumFile path = do
    fileContent <- LB.readFile path
    putStrLn $  show (md5sum fileContent) ++ " " ++ path

handleParams :: Opts -> IO ()
handleParams (Opts _ c ta t z checkFileList) = do
    putStrLn "foo"
    print ta
    print t
    print z
    Control.Monad.when c $ checkFlag checkFileList
    mapM_ sumFile checkFileList
    


