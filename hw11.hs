import Data.Set             (Set)
import Control.Monad.Reader (Reader)
import Control.Monad.Writer (Writer)
import Control.Monad.State  (State, execState)
import Data.Map.Strict      (Map)

-- 1. Реализуйте функцию @hasFreeVars@, проверяющую, есть ли в терме свободные переменные.
--    Функция должна быть реализована с помощью 'Reader'.
--    В качестве первого параметра 'Reader' поддерживайте множество видимых в данном 
--    месте терма переменных. (0.5 б)

data Term a = Var a | App (Term a) (Term a) | Lam a (Term a)

hasFreeVars :: Eq a => Term a -> Bool
hasFreeVars = undefined

-- 2. Реализуйте функцию @getBoundedVars@, возвращающую множество связанных переменных терма.
--    Функция должна быть реализована с помощью 'Writer'.
--    В качестве первого параметра 'Writer' поддерживайте множество связанных переменных терма. (0.5 б)

getBoundedVars :: Eq a => Term a -> Set a
getBoundedVars = undefined

-- 3. 'State' моднее, чем 'Reader'. Докажите это, реализовав аналоги 
--    ask, asks и local для 'State'. (0.5 б)
--

-- 4. 'State' моднее, чем 'Writer'. Докажите это, реализовав аналоги 
--    tell, listen, listens, censor для 'State'. (1 б)

-- | Ваш аналог tell.
--
tellState :: w -> State w a
tellState = undefined

-- | Ваш аналог censor.
--
censorState :: (w -> w) -> State w a -> State w a
censorState = undefined

-- | Этот тест должен возвращать "42".
--   Честный 'Writer' тоже вернул бы "42".
--
testWriterState :: String
testWriterState = flip execState mempty $ do
    tellState "4"
    censorState (pure . last) $ tellState "298" >> tellState "2"

-- 5. Время для императивного программирования!

-- | 5.1. Реализуйте функцию `whileM_`, исполняющую заданное вычисление,
--        пока первое вычисление возвращает 'True'. (0.1 б)
--
whileM_ :: m Bool -> m a -> m ()
whileM_ = undefined

-- | 5.2. Реализуйте функцию `forM_`, являющуюуся аналогом цикла for.
--        Перед входом в цикл вычисляется @init@, в итерациях цикла вычисляется @body@, 
--        в конце каждой итерации цикла вычисляется @nextIter@, итерации идут, 
--        пока выполняется условие @cond@. (0.1 б)
--
forM_ :: (m (), m Bool, m ()) -> m a -> m ()
forM_ (init, cond, nextIter) body = undefined

type Context = Map String Int

-- 5.3. Реализуйте функции @setVar@, @incVar@, @getVar@. (0.25 б)

-- | Задаёт значение переменной. 
--   Если переменная есть в контексте, перезаписывает её значение.
--
setVar :: String -> Int -> State Context ()
setVar = undefined

-- | Увеличивает значение переменной. 
--   Если её нет в контексте, то кидает ошибку.
--
incVar :: String -> Int -> State Context ()
incVar = undefined

-- | Достаёт из контекста значение заданной переменной.
--   Если переменной нет в контексте, то кидает ошибку.
--
getVar :: String -> State Context Int
getVar = undefined

-- 5.4. Перепешите один в один (насколько это возможно) представленный ниже плюсовый код, 
--      используя монаду 'State' и функции, которые вы реализовали выше. 
--      Напишите 3 теста, проверяющие, что ваша реализация @fib@ работает корректно. (1 б)

{- 
int fib(int n) {
    int prev = 0;
    int cur = 1;

    for (int i = 0; i < n; i = i + 1) {
        int c = cur;
        cur = prev + cur;
        prev = cur;
    }

    return cur
} 
-}

fib :: Int -> State Context Int
fib = undefined

-- 6. Создайте свою монаду @StateWithError@, обладающую сразу двумя эффектами:
--      a) эффектом монады 'State' (изменяемое состояние);
--      b) эффектои монады 'Either' (возможность завершения вычисления ошибкой). (2 б)
--

data StateWithError = YourImplementation

