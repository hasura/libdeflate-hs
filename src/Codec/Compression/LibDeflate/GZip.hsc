{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Codec.Compression.LibDeflate.GZip (
    LibDeflateCompressor,
    gzipCompress_reuse,
    gzipCompress
  ) where

import Foreign
import Foreign.C.Types
import qualified Data.ByteString.Internal as BS
import System.IO.Unsafe
import Control.Monad
import Control.Concurrent
import Control.Exception

newtype LibDeflateCompressor = LibDeflateCompressor (Ptr LibDeflateCompressor)

foreign import capi unsafe "libdeflate.h libdeflate_alloc_compressor"
  libdeflate_alloc_compressor :: CInt -> IO LibDeflateCompressor

foreign import capi unsafe "libdeflate.h libdeflate_gzip_compress"
  libdeflate_gzip_compress 
      :: LibDeflateCompressor
      -- input buffer/size:
      -> Ptr Word8 -> CInt
      -- output buffer/size:
      -> Ptr Word8 -> CInt 
      -- Output compressed size, in bytes, or 0 if couldn't fit:
      -> IO CInt

foreign import capi unsafe "libdeflate.h libdeflate_free_compressor"
  libdeflate_free_compressor :: LibDeflateCompressor -> IO ()

gzipCompress :: BS.ByteString -> Int -> Int -> Maybe BS.ByteString
{-# NOINLINE gzipCompress #-}
gzipCompress (BS.PS inputFpDirty off inputLen) compressionLevel maxCompressedSizeBytes = unsafePerformIO $ do
    -- NOTE: `PS` is a compatibility pattern synonym on bytestring <0.11
    let inputFp = inputFpDirty `plusForeignPtr` off
    bracket
    -- NOTE: this takes ~60us; these could be reused in threadsafe pool impl
      (libdeflate_alloc_compressor $ fromIntegral compressionLevel)
      libdeflate_free_compressor 
       $ \compressor -> do
            withForeignPtr inputFp $ \inputPtr -> do
                outputFp <- mallocForeignPtrBytes maxCompressedSizeBytes
                compressedSizeBytes <- withForeignPtr outputFp $ \outputPtr -> do
                    libdeflate_gzip_compress compressor 
                      inputPtr (fromIntegral inputLen) 
                      outputPtr (fromIntegral maxCompressedSizeBytes)
                return $
                     BS.PS outputFp 0 (fromIntegral compressedSizeBytes) <$ guard (compressedSizeBytes > 0)

-- testing:
gzipCompress_reuse :: BS.ByteString -> Int -> Maybe BS.ByteString
{-# NOINLINE gzipCompress_reuse #-}
gzipCompress_reuse (BS.PS inputFpDirty off inputLen) maxCompressedSizeBytes = unsafePerformIO $ do
    -- NOTE: `PS` is a compatibility pattern synonym on bytestring <0.11
    let inputFp = inputFpDirty `plusForeignPtr` off
    withForeignPtr inputFp $ \inputPtr -> do
        outputFp <- mallocForeignPtrBytes maxCompressedSizeBytes
        withMVar theCompressor $ \compressor -> do
                compressedSizeBytes <- withForeignPtr outputFp $ \outputPtr -> do
                    libdeflate_gzip_compress compressor 
                      inputPtr (fromIntegral inputLen) 
                      outputPtr (fromIntegral maxCompressedSizeBytes)
                return $
                     BS.PS outputFp 0 (fromIntegral compressedSizeBytes) <$ guard (compressedSizeBytes > 0)

-- TODO proper pool
theCompressor :: MVar LibDeflateCompressor
{-# NOINLINE theCompressor #-}
theCompressor = unsafePerformIO $ do
    libdeflate_alloc_compressor 1 >>= newMVar
