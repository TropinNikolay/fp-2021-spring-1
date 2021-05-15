module Main where

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (join, when)
import Data.IORef (modifyIORef, newIORef, readIORef)
import System.IO (IOMode (..), hGetContents, hPutStrLn, isEOF, putStrLn, withFile)

forceEvalIO :: NFData a => IO a -> IO a
forceEvalIO = join . fmap (evaluate . force)

main :: IO ()
main = go 0
  where
    go n = do
      let fileName = "text_copy_" <> (show n) <> ".log"
      nOfLinesRef <- newIORef 0
      continue <- withFile fileName WriteMode $ \h -> forceEvalIO $ while h nOfLinesRef
      when continue $ go $ n + 1
      where
        while h nOfLinesRef = do
          done <- isEOF
          nOfLines <- readIORef nOfLinesRef
          if nOfLines == 1000
            then pure True
            else
              if done
                then pure False
                else do
                  modifyIORef nOfLinesRef (+ 1)
                  line <- getLine
                  putStrLn line
                  hPutStrLn h line
                  while h nOfLinesRef
