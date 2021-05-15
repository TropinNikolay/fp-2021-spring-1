{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (join, when)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (drop, dropWhile, intercalate, take, takeWhile)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath.Posix (joinPath)
import System.IO (IOMode (..), hGetContents, hPutStr, hPutStrLn, isEOF, putStr, putStrLn, withFile)

forceEvalIO :: NFData a => IO a -> IO a
forceEvalIO = join . fmap (evaluate . force)

class Hashable a where
  hash :: a -> Int

instance Hashable Int where
  hash x = (x * 2654435761) `mod` (2^32)

-- | В Хаскеле есть возможность указать переменные типа,
--   которые никак не будут использоваться в его реализации.
--
--   Тут это пригодится: объекты хранятся на диске, но мы всё равно хотим предоставлять
--   информацию про их типы компилятору.
data FileSystemKVS k v = FileSystemKVS FilePath

-- TODO: реализуйте функцию createFileSystemKVS

-- | Создаёт объект 'FileSystemKVS', который сохраняет объекты в директорию @pathToDir@.
createFileSystemKVS :: FilePath -> FileSystemKVS k v
createFileSystemKVS pathToDir = FileSystemKVS pathToDir

-- | Класс типов, описывающий объекты, которые можно представить в виде строк.
class Serializable a where
  -- | Превращает объект в строку.
  toS :: a -> String

  -- | Парсит строку в объект.
  fromS :: String -> a

  fromFile :: FilePath -> IO a
  fromFile = fmap fromS . readFile

  toFile :: FilePath -> a -> IO ()
  toFile file = writeFile file . toS

-- TODO: сделайте 'Int' представителем 'Serializable'
instance Serializable Int where
  toS = show
  fromS = read

-- TODO: сделайте 'String' представителем 'Serializable'
instance Serializable String where
  toS = show
  fromS = read

data Tree a = Leaf | Node a (Tree a) (Tree a)

-- TODO: сделайте 'Tree a' представителем 'Serializable'
instance Serializable a => Serializable (Tree a) where
  toS Leaf = "0 "
  toS (Node x l r) =
    let serialize obj = let s = toS obj in (toS $ length s) <> " " <> s
     in serialize x <> serialize l <> serialize r

  fromS ser =
    let readLength :: String -> (Int, String)
        readLength s = (fromS $ takeWhile (/= ' ') s, tail $ dropWhile (/= ' ') s)

        readWithLength :: Serializable x => String -> Int -> (x, String)
        readWithLength s l = (fromS $ take l s, drop l s)

        readObj :: Serializable x => String -> (x, String)
        readObj s =
          let (l, s') = readLength s
           in readWithLength s' l

        (len, ser') = readLength ser
     in if len == 0
          then Leaf
          else
            let (x, ser'') = readWithLength ser' len
                (l, ser''') = readObj ser''
                (r, _) = readObj ser'''
             in Node x l r

instance Serializable a => Show (Tree a) where
  show t = toS t

-- | KVS, допускающий вычисления в IO при работе с ключами и значениями.
--
--   Ключи в 'KVS'е являются 'Hashable' для того, чтобы можно было использовать
--   их в качестве уникальных имён файлов в системе.
--
--   Значения в 'KVS'е являются 'Serializable' для того, чтобы можно было
--   записывать их в файлы.
class (Hashable (Key a), Serializable (Value a)) => KVS a where
  type Key a
  type Value a

  -- | Получает из @a@ значение по ключу @Key a@.
  --   Если по данному ключу нет значения, то возвращает @Nothing@.
  get :: a -> Key a -> IO (Maybe (Value a))

  -- | Кладёт в @a@ по ключу @Key a@ значение @Value a@.
  --   Если значение по данному ключу уже есть, то перезаписывает его.
  put :: a -> Key a -> Value a -> IO ()

instance (Hashable k, Serializable v) => KVS (FileSystemKVS k v) where
  type Key (FileSystemKVS k v) = k
  type Value (FileSystemKVS k v) = v

  get (FileSystemKVS p) key = do
    let fName = joinPath [p, show $ hash key]
    doesExist <- doesFileExist fName
    if doesExist
      then do
        serialized <- fromFile fName
        pure $ Just $ serialized
      else pure Nothing

  put (FileSystemKVS p) key value = do
    let fName = joinPath [p, show $ hash key]
    toFile fName value

main :: IO ()
main = do
  let kvs = (createFileSystemKVS ".") :: FileSystemKVS Int String
  put kvs 1 "test"
  s <- get kvs 1
  print s
  s <- get kvs 2
  print s
  put kvs 3 "another_test"
  s <- get kvs 3
  print s
