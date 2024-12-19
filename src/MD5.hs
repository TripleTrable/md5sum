{-# OPTIONS_GHC -Wno-name-shadowing #-}
module MD5 where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as LB
import Data.Binary (Word32)
import Data.Bits
import GHC.Arr (listArray, Array, (!))
import Control.Monad (replicateM)
import GHC.List (foldl')
import Data.Binary.Put 
import Numeric (showHex)
import Data.Int (Int64)


tableK :: Int -> Word32
-- Prec computed table for integer parts of sines: floor(232 × abs(sin(i + 1)))
tableK i = [
 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee ,
 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501 ,
 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be ,
 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821 ,
 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa ,
 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8 ,
 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed ,
 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a ,
 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c ,
 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70 ,
 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05 ,
 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665 ,
 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039 ,
 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1 ,
 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1 ,
 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391 ] !! i

tableS :: Int -> Int
tableS i = [
 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22 ,
 5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20 ,
 4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23 ,
 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21 ] !! i

tableG :: Int -> Int
tableG i = [
 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22 ,
 5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20 ,
 4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23 ,
 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21 ] !! i


funcF, funcG, funcH, funcI :: Word32 -> Word32 -> Word32 -> Word32

funcF b c d = (b .&. c) .|.  (complement b .&. d)
funcG b c d = (b .&. d) .|.  (complement c .&. d)
funcH b c d = b `xor` c `xor` d
funcI b c d = c `xor` (b .|. complement d)

data MD5Data = MD5Data
    {
     a:: !Word32
    ,b:: !Word32
    ,c:: !Word32
    ,d:: !Word32
    }

(<+>) :: MD5Data -> LB.ByteString -> MD5Data
infixl 6 <+>
md5@(MD5Data a b c d) <+> byteString =
    let newDat = listArray (0,15) $ replicateM 16 getWord32le `runGet` byteString -- create array 
                                                                        -- M from wikipedia example
        MD5Data a' b' c' d' = foldl' (md5iteration newDat) md5 [0 .. 63] -- [1..64] is just an 
                                        -- ascending array to run md5iteration
                                        -- 64 times. (verry dirty for loop)
    in MD5Data (a+a') (b+b') (c+c') (d+d')

md5iteration :: Array Int Word32 -> MD5Data -> Int -> MD5Data
md5iteration newData (MD5Data a b c d) i =
    MD5Data d a' b c
        where a' = b + rotateL (f b c d + a + tableK i + (newData ! g) )  (tableS i)
              g | i < 16 = i
                | i < 32 = (5 * i + 1) `mod` 16
                | i < 48 = (3 * i + 5) `mod` 16
                | otherwise = (7 * i) `mod` 16

              f b c d | i < 16 = funcF  b c d
                      | i < 32 = funcG  b c d
                      | i < 48 = funcH  b c d
                      | otherwise = funcI  b c d

md5sum :: LB.ByteString -> String
md5sum dat = 
    let MD5Data a b c d = runGet (md5sumInt start) dat
    in  foldr convToHex [] . LB.unpack . runPut $ mapM_ putWord32le [a,b,c,d]
    where 
        start = MD5Data  0x67452301 0xEFCDAB89 0x98BADCFE 0x10325476
        convToHex x s  | x < 16 = '0': showHex x s
                       | otherwise =   showHex x s

-- copied from Data.Binary.Get.internal as the failOnEOF must not happen. 
getLazyByteString' :: Int64 -> Get LB.ByteString
getLazyByteString' n = do
    S s ss bytes <- get
    let big = s `join` ss
    case splitAtST n big of
      (consume, rest) -> do put $ mkState rest (bytes + n)
                            return consume

md5sumInt :: MD5Data -> Get MD5Data
md5sumInt mdData = do
    chunk <- getLazyByteString 64
    let chunkLength = LB.length chunk

    if chunkLength == 64 then
        -- if we got here, this means we read a full chunk from the file. Now we
        -- can apply the the chunk of data to our md5 data object using the <+>
        -- operator and call this function recursivle using $! to force
        -- evaluation of the <+>
        md5sumInt $! mdData <+> chunk


    else do
        bytes <- bytesRead
        let originalDataLength = runPut . putWord64le $ fromIntegral (bytes - 64 + chunkLength) * 8
                -- converts the number of bytes into word64 and than uses put...
                -- and runPut to convert the number into a byteString for later
                -- processing
            padding len = LB.append chunk (LB.cons 0x80 (LB.replicate (len - 1) 0x00))

        return $
            if chunkLength > 55 then
                -- if more than 54 bytes are left, this means the original
                -- length does not fit into the chunk. To fix this, we fill to
                -- full 64 byte alignment and than add 56 times bytes with zero
                mdData <+> padding (64 - chunkLength) <+> LB.replicate 56 0x00 `LB.append` originalDataLength 
            else
                -- this means the length of the original message fits and we
                -- just padd to 56 byte chunk length
                mdData <+> padding (56 - chunkLength) `LB.append` originalDataLength







