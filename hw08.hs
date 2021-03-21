import Data.Char           (digitToInt, isAlphaNum, isSpace, isDigit)
import Control.Applicative (Alternative (..))

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap g aP = Parser f
      where
        f s = case runParser aP s of
            Nothing      -> Nothing
            Just (a, s') -> Just (g a, s')

instance Applicative Parser where 
  pure a = Parser $ \s -> Just (a, s)

  fP <*> aP = Parser f
    where
      f s = case runParser fP s of
        Nothing      -> Nothing
        Just (g, s') -> case runParser aP s' of
          Nothing       -> Nothing
          Just (a, s'') -> Just (g a, s'')

instance Alternative Parser where 
  empty = Parser $ \_ -> Nothing

  pA <|> pA' = Parser f
    where
      f s = case runParser pA s of
        Nothing -> runParser pA' s
        x       -> x

satisfyP :: (Char -> Bool) -> Parser Char
satisfyP p = Parser f
  where
    f :: String -> Maybe (Char, String)
    f []       = Nothing
    f (x : xs) | p x       = Just (x, xs)
               | otherwise = Nothing

symbolP :: Parser Char
symbolP = satisfyP isAlphaNum

oneSpaceP :: Parser Char
oneSpaceP = satisfyP isSpace

digitP :: Parser Int
digitP = digitToInt <$> satisfyP isDigit

digitsP :: Parser [Int]
digitsP = some digitP

spaceP :: Parser String
spaceP = many oneSpaceP

charP :: Parser String
charP = some symbolP

newLineP :: Parser Char
newLineP = satisfyP (== '\n')

-- | Чтобы загрузить текст из файла "test.txt" в переменную, 
--   нужно написать в интерпретаторе следующее:
--     test <- testIO
--
testIO :: IO String
testIO = readFile "test.txt"

-- 1. Реализуйте следующие функции-парсеры:

-- | 1.1. Парсит только заданную строку (0.25 б)
--
stringP :: String -> Parser String
stringP = undefined

-- | 1.2. Парсит целое число (0.25 б)
--
intP :: Parser Int
intP = undefined

-- | 1.3. Парсит вещественное число (0.5 б)
--
floatP :: Parser Float
floatP = undefined

-- | 1.4. Парсит весь поток, пока символы потока удовлетворяют 
--        заданному условию (0.5 б)
--
takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP = undefined

-- | 1.5. Парсер, который падает с ошибкой, если в потоке что-то осталось.
--        В противном случае парсер отрабатывает успешно (0.5 б)
--
eofP :: Parser String ()
eofP = undefined

-- | 1.6. Парсер, который парсит символ @lBorder@, всё, что парсит переданный парсер @p@,
--        а потом — правый символ @rBorder@. Возвращает то, что напарсил парсер @p@ (0.25 б)
--
inBetweenP :: String -> String -> Parser a -> Parser String a
inBetweenP lBorder rBorder p = undefined 

-- | 2. Реализуйте функцию, которая парсит списки вида "[1, a   , bbb , 23     , -7]".
--      Функция принимает парсер, которым парсятся элементы списка (0.5 б)
--
listP :: Parser a -> Parser [a]
listP = undefined

-- | Тип, представляющий значение, которое может храниться в списке.
--
data Value 
  = IntValue Int
  | FloatValue Float
  | StringValue String
  deriving (Eq, Show)

-- | 3. Реализуйте парсер 'Value', а потом парсер списка 'Value' (0.5 б)
--
--      Значения 'Value' получаются из текста по таким правилам:
--        a) Целые числа превращаются в IntValue.
--        б) Вещественные числа превращаются в FloatValue.
--        в) Всё остальное превращается в StringValue.
--
valueP :: Parser Value
valueP = undefined

valueListP :: Parser [Value]
valueListP = undefined

-- 4. Самый популярный формат для хранения табличных данных — csv (https://en.wikipedia.org/wiki/Comma-separated_values).
--    Реализуйте все вспомогательные функции-парсеры и итоговый парсер csv.

-- | csv представляет из себя список названий колонок и список строк со значениями.
--
data CSV 
  = CSV 
      {  colNames :: [String] -- названия колонок в файле
      ,  rows     :: [Row]    -- список строк со значениями из файла
      }

-- | Строка CSV представляет из себя отображение названий колонок
--   в их значения в данной строке. Если у колонки нет значения
--   в данной строке, то оно помечается как Nothing.
--
newtype Row = Row (Map String (Maybe Value))

-- | 4.1 Реализуйте парсер, который парсит строку из значений, 
--       которые могут парситься заданным парсером @p@ и разделённых заданным разделителем @sep@. 
--       Если какое-то значение не распарсилось, то оно помечается Nothing (0.5 б)
--
abstractRowP :: String -> Parser a -> Parser [Maybe a]
abstractRowP sep p = undefined

-- | 4.2 Реализуйте парсер, который парсит 'Row'. 
--       Названия колонок файла передаются аргументом (0.1 б)
--
rowP :: [String] -> Parser Row
rowP colNames = undefined

-- | 4.3 Реализуйте итоговый парсер CSV (1 б)
--
csvP :: Parser CSV
csvP = undefined

-- 5. PDB — главный формат для хранения информации о трёхмерных структурах молекул.
--
--    Вот спецификация формата данных: https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html
--    Она довольно большая, но мы не будем парсить всё, что в ней есть.
--
--    За задание можно получить разное количество баллов в зависимости от того, 
--    насколько клёвый у вас парсер.
--
--    В репозитории лежат два PDB-файла: only_atoms.pdb, atoms_and_bonds.pdb.
--    Если ваш парсер корректно парсит:
--      1. only_atoms.pdb, то вы получите 2 б. 
--         Для выполнения задания фактически нужно научиться парсить только секцию MODEL, 
--         в которой может содержаться только секция ATOM.
--      2. atoms_and_bonds.pdb, то вы получите 1 бонусный балл. (задание необязательное)
--         Придётся научиться парсить секцию CONNECT.
--

-- | Тип, представляющий из себя ATOM
--
data PDBAtom = YourImplementationOfPDBAtom

-- | Тип, представляющий из себя CONNECT
--
data PDBBond = YourImplementationOfPDBBond

-- | Тип, представляющий из себя MODEL
--
data PDBModel 
  = PDBModel 
      { atoms :: [PDBAtom] -- атомы из секции ATOM
      , bonds :: [PDBBond] -- связи из секции CONNECT
      }

-- | PDB-файл
--
newtype PDB = PDB [PDBModel]