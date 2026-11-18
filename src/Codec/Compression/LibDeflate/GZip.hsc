{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Codec.Compression.LibDeflate.GZip (
    -- gzipCompress_reuse,
    gzipCompress,
    gzipCompressSimple,
    gzipMaxSize,
    -- * Low-level API
    -- | See libdeflate.h for docs
    LibDeflateCompressor,
    libdeflate_free_compressor,
    libdeflate_alloc_compressor,
    libdeflate_gzip_compress,
    libdeflate_gzip_compress_bound
  ) where

import Foreign
import Foreign.C.Types
import GHC.ForeignPtr (unsafeWithForeignPtr)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString as BS
import System.IO.Unsafe
import Control.Monad
-- import Control.Concurrent
import Control.Exception
import Data.Maybe

newtype LibDeflateCompressor = LibDeflateCompressor (Ptr LibDeflateCompressor)

foreign import capi unsafe "libdeflate.h libdeflate_alloc_compressor"
  libdeflate_alloc_compressor :: CInt -> IO LibDeflateCompressor

-- 'safe' is important for performance here, otherwise we get long sync periods
-- before GC and these interact particularly badly with large objects (that we
-- might be compressing)
--
-- NOTE: sizes must be 'CSize' to match libdeflate's @size_t@. These used to be
-- 'CInt', so an input of 2^31..2^32-1 bytes was passed as a negative int that
-- C sign-extended to a ~1.8e19 byte length; the lazy compressor (levels 5-9)
-- then wrote past the end of its matchfinder table and segfaulted. Larger
-- inputs could also wrap to a small positive length and be silently truncated.
foreign import capi   safe "libdeflate.h libdeflate_gzip_compress"
  libdeflate_gzip_compress 
      :: LibDeflateCompressor
      -- input buffer/size:
      -> Ptr Word8 -> CSize
      -- output buffer/size:
      -> Ptr Word8 -> CSize
      -- Output compressed size, in bytes, or 0 if couldn't fit:
      -> IO CSize

foreign import capi unsafe "libdeflate.h libdeflate_free_compressor"
  libdeflate_free_compressor :: LibDeflateCompressor -> IO ()

foreign import capi unsafe "libdeflate.h libdeflate_gzip_compress_bound"
  libdeflate_gzip_compress_bound  :: LibDeflateCompressor -> CSize -> CSize

-- | return the maximum  (worst case) buffer size needed to  store a
-- compressed string of the given length
gzipMaxSize 
  :: Int -- ^ input size in bytes
  -> Int -- ^ maximum possible compressed size
gzipMaxSize =
  -- nullPtr means we want the max regardless of compression level
  fromIntegral . libdeflate_gzip_compress_bound (LibDeflateCompressor nullPtr) . fromIntegral


-- | Like gzipCompress But use a heuristic to choose an appropriate buffer size, never failing
gzipCompressSimple
  :: BS.ByteString -- ^ input
  -> Int -- ^ compression level (0 - 12)
  -> BS.ByteString -- ^ output
gzipCompressSimple inp compressionLevel
  | len < 1000 = gzippedNeverFails
  | len < 10000 = fromMaybe gzippedNeverFails $ gzip (len `div` 2) 
  -- 95% of json responses in hasura cloud larger than 20kb compress below 32%:
  | otherwise = fromMaybe gzippedNeverFails $ gzip (len `div` 3)
  where len = BS.length inp
        maxCompressedLen = gzipMaxSize len
        gzip = gzipCompress inp compressionLevel
        gzippedNeverFails = fromMaybe (error "libdeflate-hs: FIXME gzipMaxSize") $
          gzip maxCompressedLen

-- | Compress a string so long as it can compress down to the requested size
gzipCompress 
  :: BS.ByteString -- ^ input
  -> Int -- ^ compression level (0 - 12)
  -> Int -- ^ max compressed size
  -> Maybe BS.ByteString -- ^ @Nothing@ if the compressed string was > max compressed size
{-# NOINLINE gzipCompress #-}
gzipCompress (BS.PS inputFpDirty off inputLen) compressionLevel maxCompressedSizeBytes = unsafePerformIO $ do
    -- NOTE: `PS` is a compatibility pattern synonym on bytestring <0.11
    let inputFp = inputFpDirty `plusForeignPtr` off
    outputFp <- mallocForeignPtrBytes maxCompressedSizeBytes
    bracket
    -- NOTE: this takes ~3us to 13us depending on compression level; these
    -- could be reused in threadsafe pool impl, but probably not warranted
      allocCompressor
      libdeflate_free_compressor
       $ \compressor -> do
            -- NOTE unsafeWithForeignPtr: we must not loop or throw exception in continuation:
            unsafeWithForeignPtr inputFp $ \inputPtr -> do
                compressedSizeBytes <- unsafeWithForeignPtr outputFp $ \outputPtr -> do
                    libdeflate_gzip_compress compressor 
                      inputPtr (fromIntegral inputLen) 
                      outputPtr (fromIntegral maxCompressedSizeBytes)
                return $
                     BS.PS outputFp 0 (fromIntegral compressedSizeBytes) <$ guard (compressedSizeBytes > 0)
  where
    -- libdeflate_alloc_compressor returns NULL for a level outside 0-12, or if
    -- allocation fails. Passing NULL on to libdeflate_gzip_compress segfaults,
    -- so throw instead. (Throwing here in bracket's acquire means there's
    -- nothing for the release action to free.)
    allocCompressor = do
        c@(LibDeflateCompressor p) <- libdeflate_alloc_compressor $ fromIntegral compressionLevel
        when (p == nullPtr) $ throwIO $ ErrorCall $
          if compressionLevel < 0 || compressionLevel > 12
            then "libdeflate-hs: invalid compression level " ++ show compressionLevel ++ " (must be 0-12)"
            else "libdeflate-hs: libdeflate_alloc_compressor failed for compression level "
                   ++ show compressionLevel ++ " (out of memory?)"
        return c

{-
-- XXX testing:
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
-}
