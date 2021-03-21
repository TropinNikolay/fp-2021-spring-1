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
  -- pure :: a -> ParserS a
  pure a = Parser $ \s -> Just (a, s)

  -- (<*>) :: ParserS (a -> b) -> ParserS a -> ParserS b
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

data DNA = A | T | G | C
  deriving (Eq, Show)

dnaP :: Parser DNA
dnaP = Parser f
  where
    f :: String -> Maybe (DNA, String)
    f []       = Nothing
    f (x : xs) | x == 'A'  = Just (A, xs)
               | x == 'T'  = Just (T, xs)
               | x == 'G'  = Just (G, xs)
               | x == 'C'  = Just (C, xs)
               | otherwise = Nothing

dnaSeqP :: Parser [DNA]
dnaSeqP = some dnaP

newtype Fasta = Fasta { seqs :: [(String, [DNA])] }
  deriving (Eq, Show)

charP :: Parser String
charP = some symbolP

newLineP :: Parser Char
newLineP = satisfyP (== '\n')

fastaP :: Parser Fasta
fastaP =  Fasta 
      <$> some fastaSeqP
  where
    fastaSeqP :: Parser (String, [DNA])
    fastaSeqP =  (,) 
             <$> nameP <* newLineP 
             <*> (concat <$> some ((dnaSeqP <* newLineP) <|> dnaSeqP))
      where
        nameP :: Parser String
        nameP = satisfyP (== '>') *> spaceP *> charP

testIO :: IO String
testIO = readFile "test.fasta"