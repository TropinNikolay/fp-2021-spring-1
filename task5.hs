module Main where

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (join, sequence)
import Control.Monad.State.Lazy
import Data.Array (Array, array, elems, (!))
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (intercalate, reverse)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import System.Environment (getArgs)
import System.IO (Handle, IOMode (..), hClose, hGetLine, hIsEOF, hPutStr, hPutStrLn, openFile, withFile)

forceEvalIO :: NFData a => IO a -> IO a
forceEvalIO = join . fmap (evaluate . force)

merge :: Array Int Handle -> Handle -> IO ()
merge hs outputHandle = do
  firstsList <- sequence $ fmap getNext hs
  let heap = S.fromList $ [(fromJust mx, i) | (mx, i) <- zip (elems firstsList) [1 ..], isJust mx]
  go heap
  where
    getNext :: Handle -> IO (Maybe Int)
    getNext h = do
      isEOF <- hIsEOF h
      if isEOF
        then pure Nothing
        else hGetLine h >>= pure . Just . read
    getNextIth i = getNext $ hs ! i

    go heap = case S.minView heap of
      Nothing -> hPutStrLn outputHandle ""
      Just ((x, i), heap') -> do
        hPutStr outputHandle (show x ++ " ")
        mx' <- getNextIth i
        if isJust mx'
          then go ((fromJust mx', i) `S.insert` heap')
          else go heap'

main :: IO ()
main = do
  files <- getArgs
  handles <- mapM (flip openFile ReadMode) files
  withFile "sort_res.txt" WriteMode $
    \outputHandle ->
      forceEvalIO $
        merge (array (1, length handles) $ zip [1 ..] handles) outputHandle
  mapM_ hClose handles
