{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Codec.Compression.LibDeflate.GZip
-- reference implementation:
import qualified Codec.Compression.GZip as Zlib
import Test.QuickCheck.Instances.ByteString ()
import Control.Monad


prop_roundTripCompression :: BS.ByteString -> Property
prop_roundTripCompression inputOrig = 
  forAll (choose (0, 12)) $ \compressionLevel ->
    -- excercise length and offset, past word boundary:
    forAll (choose (0, 9)) $ \offsL -> forAll (choose (0, 9)) $ \offsR ->
     let inputDropped = BS.drop offsL inputOrig
         input = BS.take (BS.length inputDropped - offsR) inputDropped
      in BL.toStrict (Zlib.decompress $ BL.fromStrict $ gzipCompressSimple input compressionLevel) === input

-- same as above,  but make sure we're testing the high-compression path 
--  by building a string with lots of repetition:
prop_roundTripCompressionCompressible :: BS.ByteString -> Property
prop_roundTripCompressionCompressible inputOrigToBeRepeated = 
  forAll (choose (0, 12)) $ \compressionLevel ->
    -- excercise length and offset, past word boundary:
    forAll (choose (0, 9)) $ \offsL -> forAll (choose (0, 9)) $ \offsR ->
      forAll (choose (100, 500)) $ \repetitions ->
         let inputDropped = BS.drop offsL (mconcat $ replicate repetitions inputOrigToBeRepeated)
             input = BS.take (BS.length inputDropped - offsR) inputDropped
          in BL.toStrict (Zlib.decompress $ BL.fromStrict $ gzipCompressSimple input compressionLevel) === input

smokeCheckWeActuallyCompress :: IO ()
smokeCheckWeActuallyCompress = do
    let str = "And the haters gonna hate, hate, hate, hate, hate (haters gonna hate)"
        compressedStr = gzipCompressSimple str 6
    unless (BS.length compressedStr == 53 && BS.length str == 69) $
        error "FIXME!!!: The numbers don't actually matter here, we're just proving that we're actually compressing the string."
    putStrLn "Okay smokeCheckWeActuallyCompress"

main :: IO ()
main = do
    smokeCheckWeActuallyCompress
    quickCheckWith (stdArgs { maxSuccess = 10000, chatty = True, maxSize = 50000 }) prop_roundTripCompression
    quickCheckWith (stdArgs { maxSuccess = 50000, chatty = True, maxSize = 50 }) prop_roundTripCompressionCompressible
