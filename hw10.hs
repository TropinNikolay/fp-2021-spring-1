{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DatatypeContexts #-}


import Data.List (elemIndex)
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- 1. Перепешите код без do-нотации, используя bind (>>=), then (>>) и обычные let'ы.

-- | 1.1. (0.25 б)
--
fromDo11 :: Maybe Int -> Maybe String -> Maybe (Int, String)
fromDo11 aM bM =
    fmap (+ 10) aM >>= (\a ->
    let aL = [a, a, a]
        a  = a + length aL
     in bM >>
    (let [a, b, c] = aL in
    (fmap (<> "abcd") bM) >>= (\b ->
    pure (c, b))))

-- | 1.2. (0.25 б)
--
fromDo12 :: [Int] -> Maybe Char -> [(Char, Int)]
fromDo12 isL cM =
    isL >>= (\curI ->
    tail isL >>= (\nextI ->
    if nextI > curI
        then
            let a = curI + nextI
             in
            (tail $ tail isL) >>= (\nextNextI ->
            case (cM, (curI, nextI, nextNextI)) of
              (Nothing, _)         -> fail ""
              (Just ch, (0, 0, 0)) -> pure (ch, a)
              _                    -> fail ""
            )
        else pure ('0', 0)
    ))

-- 2. Сделайте (Either e) монадой (не забудьте про 'MonadFail'). (0.25 б)
--

-- instance Monad (Either e) where
--     (Left l) >>= _ = Left l
--     (Right r) >>= f = f r

instance MonadFail (Either String) where
    fail s = Left s

-- 3. Сделайте (WithData d) монадой (не забудьте про 'MonadFail').
--    Опишите словами, какой эффект получился у созданной вами монады.
--    Без этого описания задание не засчитывается. (0.5 б)
--

newtype WithData d a
  = WithData
      { runWithData :: d -> a
      }

instance Functor (WithData d) where
    f `fmap` (WithData x) = WithData (f . x)

instance Applicative (WithData d) where
    pure x = WithData{runWithData = \_ -> x}
    (WithData f) <*> (WithData x) = WithData $ \d -> f d (x d)

instance Monad (WithData d) where
    WithData{runWithData = x} >>= f = WithData{runWithData = \d -> let WithData f' = f (x d)  in f' d}

-- 4. С помощью монады списка создайте список, содержащий в себе все пифагоровы тройки.
--    В списке не должно быть дублей.
--    Дублирования нужно убрать за счёт дополнительного условия в do-нотации. (0.5 б)
--

pythagoreanTriplets :: [(Integer, Integer, Integer)]
pythagoreanTriplets = do
    c <- [1..]
    a <- [1..c]
    b <- [1..a]
    if a^2 + b^2 == c^2
       then return (a, b, c)
       else fail ""

-- 5. Сделайте (Parser token) монадой. (0.5 б)
--

newtype Parser token a
  = Parser
      { runParser :: [token] -> (a, [token])
      }

instance Functor (Parser token) where
    fmap = liftM

instance Applicative (Parser token) where
    pure x = Parser{runParser = \tokens -> (x, tokens)}
    (<*>) = ap

instance Monad (Parser token) where
    Parser{runParser = x} >>= f =
      Parser{runParser = \tokens ->
        let (a, tokens') = x tokens
            Parser f' = f a
        in f' tokens'
      }

-- 6. Задайте тип данных 'HashMap'.
--
--    Сделайте 'HashMap' представителем класса 'KeyValueStorage'.
--    Коллизии в 'HashMap' должны разрешаться по методу цепочек. (2 б)
--
--    Для хранения цепочек нужно использовать списки.
--    В задании запрещается использовать какие-либо контейнеры, кроме списков.
--
--    Константная асимптотика операций не принципиальна.
--

class Hashable a where
    hash :: a -> Int

data (Hashable k, Eq k) => HashMap k v = HashTable{table :: [[(k, v)]], size :: Int}

class KeyValueStorage kvs where
    -- Ассоциированные типы. Они нам уже встречались в домашке про классы типов
    -- Тип ключа в хранилище
    type Key kvs
    -- Тип значения в хранилище
    type Value kvs

    -- | Достаёт значение по ключу.
    --   Если ключ отсутствует в хранилище, то возвращает Nothing.
    --
    get :: kvs -> Key kvs -> Maybe (Value kvs)

    -- | Кладёт значение по заданному ключу.
    --   Если по ключу в @kvs@ уже лежит значение, то возвращает Nothing.
    --
    put :: kvs -> Key kvs -> Value kvs -> Maybe kvs

    -- | Изменяет значение по заданному ключу.
    --   Если ключ отсутствует в хранилище, то возвращает Nothing.
    --
    update :: kvs -> Key kvs -> (Value kvs -> Value kvs) -> Maybe kvs

    -- | Удалаяет заданный ключ из хранилища.
    --   Если ключ отсутствует в хранилище, то возвращает Nothing.
    --
    remove :: kvs -> Key kvs -> Maybe kvs

keyIndex :: (Hashable k, Eq k) => HashMap k v -> k -> Int
keyIndex HashTable{size} key = hash key `mod` size

getBucket :: (Hashable k, Eq k) => HashMap k v -> k -> [(k, v)]
getBucket hm@HashTable{table} key = table !! keyIndex hm key

instance (Hashable k, Eq k, Eq v) => KeyValueStorage (HashMap k v) where
    type Key (HashMap k v) = k
    type Value (HashMap k v) = v

    get hm@HashTable{table, size} key = lookup key $ getBucket hm key

    put hm@HashTable{table, size} key value =
        case get hm key of
          Just _  -> Nothing
          Nothing -> Just HashTable{table = table', size}
            where
              bucket' = (key, value) : getBucket hm key
              (table1, _:table2) = splitAt (keyIndex hm key) table
              table' = table1 ++ (bucket' : table2)

    remove hm@HashTable{table, size} key = do
        value <- get hm key
        let bucket = getBucket hm key
        elemInd <- elemIndex (key, value) bucket
        let (row1, _:row2) = splitAt elemInd bucket
            row' = row1 ++ row2
            (table1, _:table2) = splitAt (keyIndex hm key) table
            table' = table1 ++ (row' : table2)
        return HashTable{table = table', size}

    update hm key f = do
        value <- get hm key
        hm' <- remove hm key
        hm'' <- put hm key (f value)
        return hm''

-- 7. Реализуйте с помощью do-нотации и рекурсии функцию @transaction@,
--    последовательно исполняющую над заданной мапой из 'Int' в 'Int'
--    операции из переданного списка 'KVSAction'. (1 б)
--
--    Если какая-то операция завершилась неудачей, то вся транзакция
--    должна завершиться неудачей и хранилище не должно никак измениться.
--

-- | Описание соответствующих действий приведено в комментариях.
--
data KVSAction
  = GetAndModify { gamKey :: Int, gamValue :: Int } -- если по заданному ключу @gamKey@ лежит
                                                    -- значение @gamValue@, то кладёт по ключу
                                                    -- (gamKey + 1) значение (gamValue - 1)
  | ModifyAndRemove { marKey :: Int, marValue :: Int } -- увеличивает значение по ключу @marKey@ на 1.
                                                       -- Если значение стало равно @marValue@,
                                                       -- то удаляет из хранилища ключ (@marKey@ - 1)
  | RemoveMany { rmKeys :: [Int], rmVal :: Int } -- удаляет из хранилища все ключи из списка @rmKeys@.
                                                 -- добавляет по ключу (length @rmKeys@) значение @rmVal@

instance Hashable Int where
    hash x = (x * 2654435761) `mod` (2^32)

transaction :: HashMap Int Int
            -> [KVSAction]
            -> Maybe (HashMap Int Int)
transaction hm [] = Just hm
transaction hm (tx:txs) = do
  hm' <-
    case tx of
      GetAndModify{gamKey, gamValue} -> do
        realValue <- get hm gamKey
        if realValue == gamValue
           then put hm (gamKey + 1) (gamValue - 1)
           else pure hm
      ModifyAndRemove{marKey, marValue} -> do
        update hm marKey (+1)
        newValue <- get hm marKey
        if newValue == marValue
           then remove hm (marKey - 1)
           else pure hm
      RemoveMany{rmKeys, rmVal} -> do
        hm'' <- go hm rmKeys
        put hm'' (length rmKeys) rmVal
          where
            go :: HashMap Int Int -> [Int] -> Maybe (HashMap Int Int)
            go hmm [] = Just hmm
            go hmm (key:keys) = do
              hmm' <- remove hmm key
              go hmm keys
  transaction hm' txs


-- 8. Задайте тип данных (ReturnableCalculation a) и сделайте
--    'ReturnableCalculation' монадой.
--    Также реализуйте функцию 'realReturn'.
--
--    Монада для 'ReturnableCalculation' должна быть определена таким образом,
--    чтобы 'realReturn', написанный в do-нотации или в цепочке bind'ов,
--    действительно возвращал заданное значение, а последующие вычисления
--    не имели бы никакого значения.
--    Пример использования 'realReturn' представлен ниже.
--
--    (2 б)
--

-- | Пример использования 'realReturn'.
--   Должно вернуться 42, завёрнутое в 'ReturnableCalculation'.
--
returnExample :: ReturnableCalculation Int
returnExample = do
    let a = 40
        b = 2

    realReturn $ a + b

    let a = 0

    if a == 0
      then pure 200
      else realReturn 0

data ReturnableCalculation a = YourImplementation2

instance Functor ReturnableCalculation where
    fmap = undefined

instance Applicative ReturnableCalculation where
    pure = undefined
    (<*>) = undefined

instance Monad ReturnableCalculation where
    (>>=) = undefined

realReturn :: a -> ReturnableCalculation a
realReturn = undefined