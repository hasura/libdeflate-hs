{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main where

import Codec.Compression.LibDeflate.GZip
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Monad
import GHC.Stats
import System.IO

main :: IO ()
main = do
  -- Fork a thread to print RTS stats every second
  _ <- forkIO $ forever $ do
    threadDelay 1000000  -- 1 second
    enabled <- getRTSStatsEnabled
    if enabled
      then do
        stats <- getRTSStats
        putStrLn $ "\n=== RTS Stats ===" ++ show (major_gcs stats) ++ " " 
          -- in ms:
          ++ show ((gcdetails_sync_elapsed_ns $ gc stats) `div` 1000000) ++ " " 
          ++ show ((gcdetails_elapsed_ns $ gc stats) `div` 1000000)
      else do
        putStrLn "RTS stats not enabled! Run with +RTS -T"

  putStrLn "Starting 1000 compression threads, concurrency 100..."

  -- repeatedly launch 100 threads that all compress the 100MB file
  replicateM_ 5 $ do
      mvars <- replicateM 100 $ do
        mvar <- newEmptyMVar
        _ <- forkIO $ do
          -- --- Do this here to simulate e.g. a fetch from redis
          -- Load the 100MB payload
          !zipPayloadChinookFullIntrospection_200k <- B.readFile "benchmarks/chinookFullIntrospection.json"
          let !zipPayloadChinook_100MB = mconcat $ replicate 500 zipPayloadChinookFullIntrospection_200k
          -- putStrLn $ "Loaded 100MB payload (" ++ show (B.length zipPayloadChinook_100MB) ++ " bytes)"
          putStr "." >> hFlush stdout

          let !_compressed = gzipCompressSimple zipPayloadChinook_100MB 6
          -- putStrLn $ "Thread completed compression: " ++ show (B.length compressed) ++ " bytes"
          putStr "x" >> hFlush stdout
          putMVar mvar ()
        return mvar

      -- Wait for all threads to complete
      mapM_ takeMVar mvars
      putStrLn "----------------------------"

  putStrLn "All threads completed!"
