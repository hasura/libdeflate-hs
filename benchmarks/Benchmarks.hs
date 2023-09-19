{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main where

import Codec.Compression.LibDeflate.GZip
import qualified Codec.Compression.GZip as Zlib
import Foreign
import Gauge.Main
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Maybe

main :: IO ()
main = do
  -- For now assume benchmarks run from the top level of repo:
  !zipPayloadChinookFullIntrospection_200k <- B.readFile "benchmarks/chinookFullIntrospection.json"
  !zipPayloadChinookFullIntrospection_20k_truncated <- B.readFile "benchmarks/chinookFullIntrospection-20k-truncated.json"
  !zipPayloadChinookSimpleQuery_400 <- B.readFile "benchmarks/chinookSimpleQuery.json"

  putStrLn "---------------------------------------------"
  putStrLn "First some compression stats:"
  let sz_zlib_lev1_zipPayloadChinookSimpleQuery_400 = BL.length $ 
            (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 1}))
            $ BL.fromStrict zipPayloadChinookSimpleQuery_400
  let sz_zlib_lev6_zipPayloadChinookSimpleQuery_400 = BL.length $ 
            (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 6}))
            $ BL.fromStrict zipPayloadChinookSimpleQuery_400

  -- 20k json
  let sz_zlib_lev1_zipPayloadChinookFullIntrospection_20k_truncated = BL.length $
            (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 1}))
            $ BL.fromStrict zipPayloadChinookFullIntrospection_20k_truncated
  let sz_zlib_lev6_zipPayloadChinookFullIntrospection_20k_truncated = BL.length $
            (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 6}))
            $ BL.fromStrict zipPayloadChinookFullIntrospection_20k_truncated
  -- 200k json
  let sz_zlib_lev1_zipPayloadChinookFullIntrospection_200k = BL.length $
            (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 1}))
            $ BL.fromStrict zipPayloadChinookFullIntrospection_200k
  let sz_zlib_lev6_zipPayloadChinookFullIntrospection_200k = BL.length $
            (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 6}))
            $ BL.fromStrict zipPayloadChinookFullIntrospection_200k

  let sz_libdeflate_lev1_zipPayloadChinookSimpleQuery_400 = B.length $ fromJust $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookSimpleQuery_400) * (0.8 :: Float)
                 in (\p -> gzipCompress p 1 outpBuffSz) zipPayloadChinookSimpleQuery_400
  let sz_libdeflate_lev6_zipPayloadChinookSimpleQuery_400 = B.length $ fromJust $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookSimpleQuery_400) * (0.8 :: Float)
                 in (\p -> gzipCompress p 6 outpBuffSz) zipPayloadChinookSimpleQuery_400
  let sz_libdeflate_lev12_zipPayloadChinookSimpleQuery_400 = B.length $ fromJust $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookSimpleQuery_400) * (0.8 :: Float)
                 in (\p -> gzipCompress p 12 outpBuffSz) zipPayloadChinookSimpleQuery_400

  -- 20k json
  let sz_libdeflate_lev1_zipPayloadChinookFullIntrospection_20k_truncated = B.length $ fromJust $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookFullIntrospection_20k_truncated) * (0.8 :: Float)
                 in (\p -> gzipCompress p 1 outpBuffSz) zipPayloadChinookFullIntrospection_20k_truncated
  let sz_libdeflate_lev6_zipPayloadChinookFullIntrospection_20k_truncated = B.length $ fromJust $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookFullIntrospection_20k_truncated) * (0.8 :: Float)
                 in (\p -> gzipCompress p 6 outpBuffSz) zipPayloadChinookFullIntrospection_20k_truncated
  let sz_libdeflate_lev12_zipPayloadChinookFullIntrospection_20k_truncated = B.length $ fromJust $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookFullIntrospection_20k_truncated) * (0.8 :: Float)
                 in (\p -> gzipCompress p 12 outpBuffSz) zipPayloadChinookFullIntrospection_20k_truncated
  -- 200k json
  let sz_libdeflate_lev1_zipPayloadChinookFullIntrospection_200k = B.length $ fromJust $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookFullIntrospection_200k) * (0.8 :: Float)
                 in (\p -> gzipCompress p 1 outpBuffSz) zipPayloadChinookFullIntrospection_200k
  let sz_libdeflate_lev6_zipPayloadChinookFullIntrospection_200k = B.length $ fromJust $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookFullIntrospection_200k) * (0.8 :: Float)
                 in (\p -> gzipCompress p 6 outpBuffSz) zipPayloadChinookFullIntrospection_200k
  let sz_libdeflate_lev12_zipPayloadChinookFullIntrospection_200k = B.length $ fromJust $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookFullIntrospection_200k) * (0.8 :: Float)
                 in (\p -> gzipCompress p 12 outpBuffSz) zipPayloadChinookFullIntrospection_200k

  print (sz_zlib_lev1_zipPayloadChinookSimpleQuery_400 , "    sz_zlib_lev1_zipPayloadChinookSimpleQuery_400" ::String)
  print ( sz_zlib_lev6_zipPayloadChinookSimpleQuery_400 , "    sz_zlib_lev6_zipPayloadChinookSimpleQuery_400" ::String)
  print ( sz_zlib_lev1_zipPayloadChinookFullIntrospection_20k_truncated , "    sz_zlib_lev1_zipPayloadChinookFullIntrospection_20k_truncated" ::String)
  print ( sz_zlib_lev6_zipPayloadChinookFullIntrospection_20k_truncated , "    sz_zlib_lev6_zipPayloadChinookFullIntrospection_20k_truncated" ::String)
  print ( sz_zlib_lev1_zipPayloadChinookFullIntrospection_200k , "    sz_zlib_lev1_zipPayloadChinookFullIntrospection_200k" ::String)
  print ( sz_zlib_lev6_zipPayloadChinookFullIntrospection_200k , "    sz_zlib_lev6_zipPayloadChinookFullIntrospection_200k" ::String)
  print ( sz_libdeflate_lev1_zipPayloadChinookSimpleQuery_400 , "    sz_libdeflate_lev1_zipPayloadChinookSimpleQuery_400" ::String)
  print ( sz_libdeflate_lev6_zipPayloadChinookSimpleQuery_400 , "    sz_libdeflate_lev6_zipPayloadChinookSimpleQuery_400" ::String)
  print ( sz_libdeflate_lev12_zipPayloadChinookSimpleQuery_400 , "    sz_libdeflate_lev12_zipPayloadChinookSimpleQuery_400" ::String)

  print ( sz_libdeflate_lev1_zipPayloadChinookFullIntrospection_20k_truncated , "    sz_libdeflate_lev1_zipPayloadChinookFullIntrospection_20k_truncated" ::String)
  print ( sz_libdeflate_lev6_zipPayloadChinookFullIntrospection_20k_truncated , "    sz_libdeflate_lev6_zipPayloadChinookFullIntrospection_20k_truncated" ::String)
  print ( sz_libdeflate_lev12_zipPayloadChinookFullIntrospection_20k_truncated , "    sz_libdeflate_lev12_zipPayloadChinookFullIntrospection_20k_truncated" ::String)

  print ( sz_libdeflate_lev1_zipPayloadChinookFullIntrospection_200k , "    sz_libdeflate_lev1_zipPayloadChinookFullIntrospection_200k" ::String)
  print ( sz_libdeflate_lev6_zipPayloadChinookFullIntrospection_200k , "    sz_libdeflate_lev6_zipPayloadChinookFullIntrospection_200k" ::String)
  print ( sz_libdeflate_lev12_zipPayloadChinookFullIntrospection_200k , "    sz_libdeflate_lev12_zipPayloadChinookFullIntrospection_200k" ::String)

  putStrLn "---------------------------------------------"

  defaultMain
    [ bgroup "reference" [
        -- We'd expect these to be best-case performance, since single chunk:
          bench "zlib gzip lev1 1k" $
            nf (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 1}))
            $ BL.fromStrict zipPayload1k
       ,  bench "zlib gzip lev1 10k" $
            nf (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 1}))
            $ BL.fromStrict zipPayload10k
       ,  bench "zlib gzip lev1 100k" $
            nf (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 1}))
            $ BL.fromStrict zipPayload100k
       ,  bench "zlib gzip lev1 10000k" $
            nf (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 1}))
            $ BL.fromStrict zipPayload10000k

       ,  bench "zlib gzip lev1 zipPayloadChinookSimpleQuery_400" $
            nf (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 1}))
            $ BL.fromStrict zipPayloadChinookSimpleQuery_400
       ,  bench "zlib gzip lev6 zipPayloadChinookSimpleQuery_400" $
            nf (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 6}))
            $ BL.fromStrict zipPayloadChinookSimpleQuery_400

       ,  bench "zlib gzip lev1 zipPayloadChinookFullIntrospection_200k" $
            nf (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 1}))
            $ BL.fromStrict zipPayloadChinookFullIntrospection_200k
       ,  bench "zlib gzip lev6 zipPayloadChinookFullIntrospection_200k" $
            nf (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 6}))
            $ BL.fromStrict zipPayloadChinookFullIntrospection_200k
       ]

    , bgroup "libdeflate"
        [ bench "alloc free compressor 1" $
            whnfIO $ do
              libdeflate_alloc_compressor 1 >>= libdeflate_free_compressor
        , bench "alloc free compressor 12" $
            whnfIO $ do
              libdeflate_alloc_compressor 12 >>= libdeflate_free_compressor

        , bgroup "level 1" [
              bench "gzipCompress 1k" $
                nf (\p -> gzipCompress p 1 1000) zipPayload1k
            , bench "gzipCompress 10k" $
                nf (\p -> gzipCompress p 1 10000) zipPayload10k
            , bench "gzipCompress 100k" $
                nf (\p -> gzipCompress p 1 100000) zipPayload100k
            , bench "gzipCompress 10000k" $
                nf (\p -> gzipCompress p 1 10000000) zipPayload10000k

            -- NOTE: arbitrarily use output buffer of size fact 0.8:
            , bench "gzipCompress zipPayloadChinookSimpleQuery_400" $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookSimpleQuery_400) * (0.8 :: Float)
                 in nf (\p -> fromJust $ gzipCompress p 1 outpBuffSz) zipPayloadChinookSimpleQuery_400
            , bench "gzipCompress zipPayloadChinookFullIntrospection_20k_truncated" $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookFullIntrospection_20k_truncated) * (0.8 :: Float)
                 in nf (\p -> fromJust $ gzipCompress p 1 outpBuffSz) zipPayloadChinookFullIntrospection_20k_truncated
            , bench "gzipCompress zipPayloadChinookFullIntrospection_200k" $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookFullIntrospection_200k) * (0.8 :: Float)
                 in nf (\p -> fromJust $ gzipCompress p 1 outpBuffSz) zipPayloadChinookFullIntrospection_200k
            ]

        , bgroup "level 6" [
              bench "gzipCompress 1k" $
                nf (\p -> fromJust $ gzipCompress p 6 1000) zipPayload1k
            , bench "gzipCompress 100k" $
                nf (\p -> fromJust $ gzipCompress p 6 100000) zipPayload100k
            , bench "gzipCompress 10000k" $
                nf (\p -> fromJust $ gzipCompress p 6 10000000) zipPayload10000k

            -- NOTE: arbitrarily use output buffer of size fact 0.8:
            , bench "gzipCompress zipPayloadChinookSimpleQuery_400" $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookSimpleQuery_400) * (0.8 :: Float)
                 in nf (\p -> fromJust $ gzipCompress p 6 outpBuffSz) zipPayloadChinookSimpleQuery_400
            , bench "gzipCompress zipPayloadChinookFullIntrospection_20k_truncated" $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookFullIntrospection_20k_truncated) * (0.8 :: Float)
                 in nf (\p -> fromJust $ gzipCompress p 6 outpBuffSz) zipPayloadChinookFullIntrospection_20k_truncated
            , bench "gzipCompress zipPayloadChinookFullIntrospection_200k" $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookFullIntrospection_200k) * (0.8 :: Float)
                 in nf (\p -> fromJust $ gzipCompress p 6 outpBuffSz) zipPayloadChinookFullIntrospection_200k
            ]

        , bgroup "level 12" [
              bench "gzipCompress 1k" $
                nf (\p -> fromJust $ gzipCompress p 12 1000) zipPayload1k
            , bench "gzipCompress 100k" $
                nf (\p -> fromJust $ gzipCompress p 12 100000) zipPayload100k
            , bench "gzipCompress 10000k" $
                nf (\p -> fromJust $ gzipCompress p 12 10000000) zipPayload10000k
            ]
        ],
      bgroup "misc"
        [ -- bump allocated, so pretty cheap:
          bench "mallocForeignPtrBytes 100" $ whnfAppIO mallocForeignPtrBytes 100,
          bench "mallocForeignPtrBytes 10000" $ whnfAppIO mallocForeignPtrBytes 10000,
          bench "mallocForeignPtrBytes 1000000" $ whnfAppIO mallocForeignPtrBytes 1000000,

          bench "BL.toStrict noop" $ whnf BL.toStrict lbs100,
          bench "BL.toStrict 1k, 10 chunks" $ whnf BL.toStrict lbs1k,
          bench "BL.toStrict 1k, 1 chunk" $ whnf BL.toStrict lbs1k_1chunk
        ]
    ]
  where !zipPayload100 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbccccc01234567890123456789"
        !zipPayload1k = mconcat $ replicate 10 zipPayload100
        !zipPayload10k = mconcat $ replicate 10 zipPayload1k
        !zipPayload100k = mconcat $ replicate 100 zipPayload1k
        !zipPayload10000k = mconcat $ replicate 100 zipPayload100k

        lbs100 :: BL.ByteString
        -- one chunk:
        !lbs100 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbccccc01234567890123456789"
        !lbs1k = BL.fromChunks $ replicate 10 zipPayload100
        !lbs1k_1chunk = BL.fromStrict $ BL.toStrict lbs1k
