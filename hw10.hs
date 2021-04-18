{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

-- 1. Перепешите код без do-нотации, используя bind (>>=), then (>>) и обычные let'ы.

-- | 1.1. (0.25 б)
--
fromDo11 :: Maybe Int -> Maybe String -> Maybe (Int, String)
fromDo11 aM bM = do
    a <- fmap (+ 10) aM

    -- в одном 'let'-выражении внутри do-нотации можно писать несколько связываний.
    -- в обычных 'let'-выражениях это тоже работает
    let aL = [a, a, a]
        a  = a + length aL

    return a
    
    bM
    [a, b, c] <- Just aL

    b <- fmap (<> "abcd") bM

    pure (c, b)

-- | 1.2. (0.25 б)
--
fromDo12 :: [Int] -> Maybe Char -> [(Char, Int)]
fromDo12 isL cM = do
    curI  <- isL
    nextI <- tail isL

    -- в do-нотации можно использовать конструкцию if-then-else.
    -- исполнение кода пойдёт по одной из веток в зависимости от условия
    if nextI > curI 
        then do
            let a = curI + nextI

            nextNextI <- tail $ tail isL
            Just ch   <- [cM]

            -- в do-нотации можно использовать паттерн-матчинг.
            -- аналогично if-then-else код исполняется по одной из веток
            -- в зависимости от того, какая ветка сматчилась
            case (curI, nextI, nextNextI) of
              (0, 0, 0) -> pure (ch, a)
              _         -> fail ""
        else pure ('0', 0)

-- 2. Сделайте (Either e) монадой (не забудьте про 'MonadFail'). (0.25 б)
--

-- 3. Сделайте (WithData d) монадой (не забудьте про 'MonadFail'). 
--    Опишите словами, какой эффект получился у созданной вами монады.
--    Без этого описания задание не засчитывается. (0.5 б)
--

newtype WithData d a 
  = WithData 
      { runWithData :: d -> a 
      }

-- 4. С помощью монады списка создайте список, содержащий в себе все пифагоровы тройки. 
--    В списке не должно быть дублей. 
--    Дублирования нужно убрать за счёт дополнительного условия в do-нотации. (0.5 б)
--

-- 5. Сделайте (Parser token) монадой. (0.5 б)
--

newtype Parser token a 
  = Parser 
      { runParser :: [token] -> (a, [token])
      }

-- 6. Задайте тип данных 'HashMap'.
--
--    Сделайте 'HashMap' представителем класса 'KeyValueStorage'.
--    Коллизии в 'HashMap' должны разрешаться по методу цепочек. (2 б)
--
--    Для хранения цепочек нужно использовать списки. 
--    В задании запрещается использовать какие-либо контейнеры, кроме списков.
--
--    Константная ассимптотика операций не принципиальна.
-- 

data HashMap k v = YourImplementation

class KeyValueStorage kvs where
    -- Ассоциированные типы. Они нам уже устречались в домашке про классы типов
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

transaction :: HashMap Int Int
            -> KVSAction
            -> Maybe kvs
transaction = undefined

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