import Control.Applicative (Alternative (..))
import Data.Char (digitToInt, isAlphaNum, isDigit, isPrint, isSpace)
import Data.Map.Strict (Map, fromList)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

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
  many pa = (:) <$> pa <*> (many pa <|> pure [])

instance Monad Parser where
  m >>= f = Parser $ \str -> case runParser m str of
                                  Nothing -> Nothing
                                  Just (g, s') -> runParser (f g) s'


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

charAnyP :: Parser String
charAnyP = some (satisfyP (\x -> isPrint x && not (isSpace x)))

valueP' :: Parser Value
valueP' = (FloatValue <$> floatP) <|> ( do
                                          v <- integer
                                          case readMaybe v of
                                               Just v -> return $ IntValue v
                                               Nothing -> StringValue <$> charAnyP
                                      ) <|> (StringValue <$> charAnyP)

-- | Чтобы загрузить текст из файла "test.txt" в переменную, 
--   нужно написать в интерпретаторе следующее:
--     test <- testIO
--
testIO :: IO String
testIO = readFile "test.txt"

-- 1. Реализуйте следующие функции-парсеры:

-- | 1.1. Парсит только заданную строку (0.25 б)
--
charP' :: Char -> Parser Char
charP' c = satisfyP (== c)

stringP :: String -> Parser String
stringP []     = pure []
stringP (c:cs) = (:) <$> charP' c <*> stringP cs

-- | 1.2. Парсит целое число (0.25 б)
--
number :: Parser String
number = many (satisfyP isDigit)

plus :: Parser String
plus = charP' '+' *> number

minus :: Parser String
minus = (:) <$> charP' '-' <*> number

integer :: Parser String
integer = plus <|> minus <|> number

intP :: Parser Int
intP = read <$> integer

-- | 1.3. Парсит вещественное число (0.5 б)
--
decimal :: Parser String
decimal = (:) <$> charP' '.' <*> number

floatP :: Parser Float
floatP = read <$> ((++) <$> integer <*> decimal)

-- | 1.4. Парсит весь поток, пока символы потока удовлетворяют 
--        заданному условию (0.5 б)
--
takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP p = Parser (\str -> Just (takeWhile p str, dropWhile p str))

-- | 1.5. Парсер, который падает с ошибкой, если в потоке что-то осталось.
--        В противном случае парсер отрабатывает успешно (0.5 б)
--
eofP :: Parser ()
eofP = Parser (\str -> if str /= "" then fail "Unexpected end." else return ((), ""))

-- | 1.6. Парсер, который парсит символ @lBorder@, всё, что парсит переданный парсер @p@,
--        а потом — правый символ @rBorder@. Возвращает то, что напарсил парсер @p@ (0.25 б)
--
inBetweenP :: String -> String -> Parser a -> Parser a
inBetweenP lBorder rBorder p = do lb <- stringP lBorder
                                  val <- p
                                  rb <- stringP rBorder
                                  return val

-- | 2. Реализуйте функцию, которая парсит списки вида "[1, a   , bbb , 23     , -7]".
--      Функция принимает парсер, которым парсятся элементы списка (0.5 б)
--
sepBy :: Parser String -> Parser a -> Parser [a]
sepBy str p = do x <- p
                 xs <- many (str >> p)
                 return (x : xs)

listP :: Parser a -> Parser [a]
listP p = inBetweenP "[" "]" (sepBy (stringP ", ") p <|> pure [])

-- | Тип, представляющий значение, которое может храниться в списке.
--
data Value = IntValue Int | FloatValue Float | StringValue String deriving (Eq, Show)

-- | 3. Реализуйте парсер 'Value', а потом парсер списка 'Value' (0.5 б)
--
--      Значения 'Value' получаются из текста по таким правилам:
--        a) Целые числа превращаются в IntValue.
--        б) Вещественные числа превращаются в FloatValue.
--        в) Всё остальное превращается в StringValue.
--
valueP :: Parser Value
valueP = (FloatValue <$> floatP) <|> ( do
                                          v <- integer
                                          case readMaybe v of
                                               Just v -> return $ IntValue v
                                               Nothing -> StringValue <$> charAnyP
                                      ) <|> (StringValue <$> charP)


valueListP :: Parser [Value]
valueListP = listP valueP

-- 4. Самый популярный формат для хранения табличных данных — csv (https://en.wikipedia.org/wiki/Comma-separated_values).
--    Реализуйте все вспомогательные функции-парсеры и итоговый парсер csv.

-- | csv представляет из себя список названий колонок и список строк со значениями.
--
data CSV 
  = CSV 
      {  colNames :: [String] -- названия колонок в файле
      ,  rows     :: [Row]    -- список строк со значениями из файла
      } deriving (Show)

-- | Строка CSV представляет из себя отображение названий колонок
--   в их значения в данной строке. Если у колонки нет значения
--   в данной строке, то оно помечается как Nothing.
--
newtype Row = Row (Map String (Maybe Value)) deriving (Show)

-- | 4.1 Реализуйте парсер, который парсит строку из значений, 
--       которые могут парситься заданным парсером @p@ и разделённых заданным разделителем @sep@. 
--       Если какое-то значение не распарсилось, то оно помечается Nothing (0.5 б)
--
maybeAlt :: Parser a -> Char -> Parser (Maybe a)
maybeAlt p split = Parser f
  where
    f str = case runParser p str of
                 Just (v, s) -> Just (Just v, s)
                 Nothing -> Just (Nothing, drop str)
    drop st = let (x, xs) = break (split ==) st in if x == st then "" else xs

abstractRowP :: Char -> Parser a -> Parser [Maybe a]
abstractRowP sep p = do x <- maybeAlt p sep
                        xs <- many (charP' sep >> maybeAlt p sep) <|> pure []
                        return (x : xs)

-- | 4.2 Реализуйте парсер, который парсит 'Row'. 
--       Названия колонок файла передаются аргументом (0.1 б)
--
rowP :: [String] -> Parser Row
rowP colNames = Row . fromList . zip colNames <$> abstractRowP ' ' valueP'

-- | 4.3 Реализуйте итоговый парсер CSV (1 б)
--
csvP :: Parser CSV
csvP = do columns <- sepBy spaceP charP
          rows <- sepBy (stringP "\n") $ rowP columns
          return $ CSV columns rows

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
eachP :: Parser [Value]
eachP = catMaybes <$> abstractRowP ' ' valueP'

pdbP :: Parser PDBModel
pdbP = do model <- stringP "MODEL"
          some spaceP
          x <- intP
          allLines <- sepBy (stringP "\r\n" <|> stringP "\n" <|> stringP "\r") eachP
          let (at, bo) = foldl (\(at, b) x -> if isAtom x then (at ++ [x], b) else if isBond x then (at, b ++ [x]) else (at, b)) ([], []) allLines
          return $ PDBModel (PDBAtom <$> at) (PDBBond <$> bo)
          where
            isAtom ((StringValue "ATOM") : xs) = True
            isAtom _ = False
            isBond ((StringValue "CONECT") : xs) = True
            isBond _ = False

-- | Тип, представляющий из себя ATOM
--
data PDBAtom = PDBAtom [Value] deriving (Show)

-- | Тип, представляющий из себя CONNECT
--
data PDBBond = PDBBond [Value] deriving (Show)

-- | Тип, представляющий из себя MODEL
--
data PDBModel 
  = PDBModel 
      { atoms :: [PDBAtom] -- атомы из секции ATOM
      , bonds :: [PDBBond] -- связи из секции CONNECT
      } deriving (Show)

-- | PDB-файл
--
newtype PDB = PDB [PDBModel]