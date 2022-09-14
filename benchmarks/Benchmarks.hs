{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main where

import Codec.Compression.LibDeflate.GZip
import Foreign
import Gauge.Main
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main =
  defaultMain
    [ bgroup "libdeflate"
        [ bench "alloc free compressor 1" $
            whnfIO $ do
              libdeflate_alloc_compressor 1 >>= libdeflate_free_compressor
        , bench "alloc free compressor 12" $
            whnfIO $ do
              libdeflate_alloc_compressor 12 >>= libdeflate_free_compressor

        , bgroup "level 1" [
              bench "gzipCompress 1k" $
                whnf (\p -> gzipCompress p 1 1000) zipPayload1k
            , bench "gzipCompress 100k" $
                whnf (\p -> gzipCompress p 1 100000) zipPayload100k
            , bench "gzipCompress 10000k" $
                whnf (\p -> gzipCompress p 1 10000000) zipPayload10000k
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
          bench "BL.toStrict 1k, 10 chunks" $ whnf BL.toStrict lbs1k
        ]
    ]
  where !zipPayload100 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbccccc01234567890123456789"
        !zipPayload1k = mconcat $ replicate 10 zipPayload100
        !zipPayload100k = mconcat $ replicate 100 zipPayload1k
        !zipPayload10000k = mconcat $ replicate 100 zipPayload100k

        lbs100 :: BL.ByteString
        -- one chunk:
        !lbs100 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbccccc01234567890123456789"
        !lbs1k = BL.fromChunks $ replicate 10 zipPayload100
