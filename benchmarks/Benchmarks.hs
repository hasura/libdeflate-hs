{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main where

import Codec.Compression.LibDeflate.GZip
import qualified Codec.Compression.GZip as Zlib
import Foreign
import Gauge.Main
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

main :: IO ()
main = do
  -- For now assume benchmarks run from the top level of repo:
  !zipPayloadChinookFullIntrospection_200k <- B.readFile "benchmarks/chinookFullIntrospection.json"
  !zipPayloadChinookSimpleQuery_400 <- B.readFile "benchmarks/chinookSimpleQuery.json"

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
       ,  bench "zlib gzip lev1 zipPayloadChinookFullIntrospection_200k" $
            nf (Zlib.compressWith (Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.compressionLevel 1}))
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
                whnf (\p -> gzipCompress p 1 1000) zipPayload1k
            , bench "gzipCompress 10k" $
                whnf (\p -> gzipCompress p 1 10000) zipPayload10k
            , bench "gzipCompress 100k" $
                whnf (\p -> gzipCompress p 1 100000) zipPayload100k
            , bench "gzipCompress 10000k" $
                whnf (\p -> gzipCompress p 1 10000000) zipPayload10000k

            -- More closely matching hasura benchmarks:
            , bench "gzipCompress zipPayloadChinookSimpleQuery_400" $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookSimpleQuery_400) * (0.5 :: Float)
                 in whnf (\p -> gzipCompress p 1 outpBuffSz) zipPayloadChinookSimpleQuery_400
            , bench "gzipCompress zipPayloadChinookFullIntrospection_200k" $
                let !outpBuffSz = floor $ fromIntegral (B.length zipPayloadChinookFullIntrospection_200k) * (0.5 :: Float)
                 in whnf (\p -> gzipCompress p 1 outpBuffSz) zipPayloadChinookFullIntrospection_200k
            ]

        , bgroup "level 6" [
              bench "gzipCompress 1k" $
                whnf (\p -> gzipCompress p 6 1000) zipPayload1k
            , bench "gzipCompress 100k" $
                whnf (\p -> gzipCompress p 6 100000) zipPayload100k
            , bench "gzipCompress 10000k" $
                whnf (\p -> gzipCompress p 6 10000000) zipPayload10000k
            ]

        , bgroup "level 12" [
              bench "gzipCompress 1k" $
                whnf (\p -> gzipCompress p 12 1000) zipPayload1k
            , bench "gzipCompress 100k" $
                whnf (\p -> gzipCompress p 12 100000) zipPayload100k
            , bench "gzipCompress 10000k" $
                whnf (\p -> gzipCompress p 12 10000000) zipPayload10000k
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
