{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Codec.Compression.LibDeflate.GZip (
    LibDeflateCompressor,
    gzipCompress
  ) where

import Foreign
import Foreign.C.Types
import qualified Data.ByteString.Internal as BS
import System.IO.Unsafe
import Control.Monad

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

gzipCompress :: BS.ByteString -> Int -> Int -> Maybe BS.ByteString
{-# NOINLINE gzipCompress #-}
gzipCompress (BS.PS inputFpDirty off inputLen) compressionLevel maxCompressedSizeBytes = unsafeDupablePerformIO $ do
    -- NOTE: `PS` is a compatibility pattern synonym on bytestring <0.11
    let inputFp = inputFpDirty `plusForeignPtr` off
    -- NOTE: this takes ~60us; these could be reused in threadsafe pool impl
    compressor <- libdeflate_alloc_compressor $ fromIntegral compressionLevel
    withForeignPtr inputFp $ \inputPtr -> do
        outputFp <- mallocForeignPtrBytes maxCompressedSizeBytes
        compressedSizeBytes <- withForeignPtr outputFp $ \outputPtr -> do
            libdeflate_gzip_compress compressor 
              inputPtr (fromIntegral inputLen) 
              outputPtr (fromIntegral maxCompressedSizeBytes)
        return $
             BS.PS outputFp 0 (fromIntegral compressedSizeBytes) <$ guard (compressedSizeBytes > 0)
