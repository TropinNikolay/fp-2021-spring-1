module Main where

-- Функция getArgs позволяет получить список аргументов,
-- с которыми был запущен хаскельный скрипт.
--
import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (filterM, forM_, join, when)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix (takeFileName)
import System.IO (IOMode (..), hGetContents, hPutStrLn, isEOF, putStrLn, withFile)

forceEvalIO :: NFData a => IO a -> IO a
forceEvalIO = join . fmap (evaluate . force)

main :: IO ()
main = do
  args <- getArgs
  let [dirPath, pattern] = args
  go (pred pattern) dirPath
  where
    pred :: String -> FilePath -> Bool
    pred "" = (== "")
    pred p =
      if head p == '*'
        then ((tail p) `isSuffixOf`)
        else
          if last p == '*'
            then ((init p) `isPrefixOf`)
            else (== p)

    go :: (FilePath -> Bool) -> FilePath -> IO ()
    go p dp = do
      contents' <- listDirectory dp
      let contents = map ((dp <> "/") <>) contents'
      dirs <- filterM doesDirectoryExist contents
      files <- filterM doesFileExist contents
      forM_ (filter (p . takeFileName) files) putStrLn
      forM_ dirs (go p)
