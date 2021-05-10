import System.IO        (IOMode (..), hPutStrLn, withFile, hGetContents)
import System.Directory (removeFile)

import Control.DeepSeq   (NFData, force)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad     (forM, forM_, join)

import Control.Concurrent.Async (forConcurrently)
import UnliftIO.Async           (pooledForConcurrentlyN)

-- 1. Ленивое IO

printExample :: IO ()
printExample = do
    let filePath = "example.txt"

    fileS <- readFile filePath

    putStrLn fileS

forceEvalIO :: NFData a => IO a -> IO a
forceEvalIO = join . fmap (evaluate . force)

lazyExample :: IO ()
lazyExample = do
    let filePath = "example.txt"

    -- withFile реализован таким образом, что сначала происходит действие открытия хэндла,
    -- потом производится переданное действие над хэндлом, а потом происходит закрытие хэндла.
    --
    -- Опасность тут в том, что закрытие хэндла происходит __энергично__ (то есть нелениво), то есть после вызова
    -- withFile хэндл __гарантированно будет закрыт__. Поэтому если действие над хэндлом выполняется 
    -- лениво, то при вычислении результата работы withFile будет падать ошибка "Работа с закрытым хэндлом":
    -- вычисление результата происходит сильно после того, как хэндл закрыли.
    --
    -- Чтобы это побороть, можно сделать действие над хэндлом энергичным, обернув его в функцию forceEvalIO.
    --
    -- Обращаю внимание на то, почему оборачивание всего withFile в forceEvalIO не решает проблему:
    -- в forceEvalIO передаётся пусть и ленивый, но __результат вызова__ withFile. Как мы поняли, 
    -- если вызов withFile произошёл, то хэндл __уже закрыт__. А вот считывание файла ещё не началось
    -- по причине своей ленивости.
    --
    c <- withFile filePath ReadMode $ \h -> forceEvalIO $ do
        contents <- hGetContents h
        pure contents

    putStrLn "passed"

    print c
  where
    errHandler :: SomeException -> IO String
    errHandler _ = pure "aaa"

-- 2. Ловим ошибки

catchExample :: IO ()
catchExample = do
    let filePath = "example.txt"

    -- При отлове ошибок IO __обязательно__ нужно форсировать действие, в котором хотим
    -- поймать ошибку
    -- 
    -- В противном случае может произойти такое, что при проверке catch ошибка
    -- ещё не вычислена. Поэтому catch отработает, ничего не поймав
    --
    -- Если убрать вызов forceEvalIO в примере ниже, то выведется строка "PASSED", а
    -- потом всё упадёт с ошибкой. Несмотря на наличие catch
    --
    c <- withFile filePath ReadMode $ \h -> forceEvalIO (do
        contents <- hGetContents h
        pure $ error "Error!") `catch` errHandler

    putStrLn "PASSED"

    print (c :: String)
  where
    errHandler :: SomeException -> IO String
    errHandler _ = pure "aaa"

-- 2. Пример IO

splitLinesOfFile :: FilePath -> IO ()
splitLinesOfFile filePath = do
    ls <- lines <$> readFile filePath

    withFile evenFilePath WriteMode $ \evenH ->
      withFile oddFilePath WriteMode $ \oddH ->
        forM_ (zip [1 ..] ls) $ \(i, l) ->
          if i `mod` 2 == 0
              then hPutStrLn evenH l
              else hPutStrLn oddH l
  where
    evenFilePath = filePath <> "_even"
    oddFilePath = filePath <> "_odd"

-- 3. Параллелизм

collectData :: IO ()
collectData = do
    let nThreads = 2

    l <- pooledForConcurrentlyN nThreads ["1", "2", "3"] $ \dirName -> do
        let filePath = dirName <> "/" <> dirName <> ".txt"

        file <- lines <$> readFile filePath
        pure file

    print l
