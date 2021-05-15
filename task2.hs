{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

-- Для представления файла как потока байт в Хаскеле есть тип `ByteString`.
-- Он может пригодиться вам для подсчёта числа байт в файле.
--
import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (join, when)
import Control.Monad.State.Lazy
import Data.ByteString (ByteString, cons, empty, singleton, uncons)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal (c2w)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.IO (Handle, IOMode (..), hGetContents, hIsEOF, hPutStrLn, putStrLn, withFile)

infixr 5 :<

pattern b :< bs <- (uncons -> Just (b, bs))

pattern Empty <- (uncons -> Nothing)

forceEvalIO :: NFData a => IO a -> IO a
forceEvalIO = join . fmap (evaluate . force)

main :: IO ()
main = do
  args <- getArgs
  let fName = head args
  ((), (nLines, nWords, nBytes)) <- withFile fName ReadMode $ \h -> forceEvalIO $ runStateT (wc h) (0, 0, 0)
  putStrLn $ "\t" <> (intercalate "\t" $ map show [nLines, nWords, nBytes]) <> " " <> fName
  where
    wc :: Handle -> StateT (Int, Int, Int) IO ()
    wc h = do
      done <- liftIO $ hIsEOF h
      if done
        then pure ()
        else do
          line <- liftIO $ B.hGetLine h
          let (nWords, nBytes) = countWB (line `B.snoc` 32) True
          modify (\(x, y, z) -> (x + 1, y + nWords, z + nBytes))
          wc h

    apply (f, g) (x, y) = (f x, g y)
    countWB Empty _ = (0, 0)
    countWB (32 :< xs) False = apply ((+ 1), (+ 1)) $ countWB xs True
    countWB (32 :< xs) True = apply (id, (+ 1)) $ countWB xs True
    countWB (_ :< xs) _ = apply (id, (+ 1)) $ countWB xs False
