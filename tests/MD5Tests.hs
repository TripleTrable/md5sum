module Main where

import Test.HUnit
import qualified Data.ByteString.Lazy as LB
import Data.Binary (Word32)
import Numeric (showHex)
import MD5

-- Test funcF
funcFTest :: Test
funcFTest = TestCase $ do
  assertEqual "funcF test 1" (funcF 0xFFFFFFFF 0x00000000 0x12345678) 0x00000000
  assertEqual "funcF test 2" (funcF 0x00000000 0xFFFFFFFF 0x12345678) 0x12345678
  assertEqual "funcF test 3" (funcF 0xAAAAAAAA 0x55555555 0x12345678) 269767760



-- Test funcG
funcGTest :: Test
funcGTest = TestCase $ do
  assertEqual "funcG test 1" (funcG 0xFFFFFFFF 0x00000000 0x12345678) 0x12345678
  assertEqual "funcG test 2" (funcG 0x00000000 0xFFFFFFFF 0x12345678) 0xEDCBA987
  assertEqual "funcG test 3" (funcG 0xAAAAAAAA 0x55555555 0x12345678) 1197540141

-- Test funcH
funcHTest :: Test
funcHTest = TestCase $ do
  assertEqual "funcH test 1" (funcH 0x00000000 0x00000000 0x00000000) 0x00000000
  assertEqual "funcH test 2" (funcH 0xFFFFFFFF 0x00000000 0x00000000) 0xFFFFFFFF
  assertEqual "funcH test 3" (funcH 0xFFFFFFFF 0xFFFFFFFF 0x00000000) 0x00000000
  assertEqual "funcH test 4" (funcH 0xFFFFFFFF 0xFFFFFFFF 0xFFFFFFFF) 0xFFFFFFFF
  assertEqual "funcH test 5" (funcH 0x12345678 0x9ABCDEF0 0xFEDCBA98) 1985229328

-- Test funcI
funcITest :: Test
funcITest = TestCase $ do
  assertEqual "funcI test 1" (funcI 0x00000000 0x00000000 0x00000000) 0xFFFFFFFF
  assertEqual "funcI test 2" (funcI 0xFFFFFFFF 0x00000000 0x00000000) 4294967295
  assertEqual "funcI test 3" (funcI 0x00000000 0xFFFFFFFF 0x00000000) 0x00000000
  assertEqual "funcI test 4" (funcI 0xFFFFFFFF 0xFFFFFFFF 0x00000000) 0x00000000
  assertEqual "funcI test 5" (funcI 0xAAAAAAAA 0x55555555 0x12345678) 3133079290


-- Test md5sum with known values
md5sumTest :: Test
md5sumTest = TestCase $ do
  let input1 = LB.pack [0x61, 0x62, 0x63]  -- "abc"
      input2 = LB.pack []                 -- empty string
      expected1 = "900150983cd24fb0d6963f7d28e17f72"  -- md5("abc")
      expected2 = "d41d8cd98f00b204e9800998ecf8427e"  -- md5("")
  assertEqual "md5sum test 1" (md5sum input1) expected1
  assertEqual "md5sum test 2" (md5sum input2) expected2

-- Test md5sumInt with chunked input
md5sumChunkedTest :: Test
md5sumChunkedTest = TestCase $ do
  let input = LB.pack $ replicate 64 0x61  -- 64 'a' characters
      expected = "014842d480b571495a4a0363793f7367"  -- md5(64 'a')
  assertEqual "md5sum chunked test" (md5sum input) expected

-- Run all tests
main :: IO Counts
main = runTestTT $ TestList [
    TestLabel "funcF Tests" funcFTest,
    TestLabel "funcG Tests" funcGTest,
    TestLabel "funcH Tests" funcHTest,
    TestLabel "funcI Tests" funcITest,
    TestLabel "MD5 Sum Tests" md5sumTest,
    TestLabel "MD5 Sum Chunked Tests" md5sumChunkedTest
  ]

