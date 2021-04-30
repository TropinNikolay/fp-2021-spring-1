{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Set             (Set, singleton)
import Control.Monad        (liftM, ap)
import Control.Applicative  (Applicative(..))
import Control.Exception    (assert)
import Control.Monad.Reader (Reader, local, ask, runReader, MonadReader)
import Control.Monad.Writer (Writer, tell, runWriter, MonadWriter, censor, listen, pass)
import Control.Monad.State  (State, runState, get, state, execState, evalState)
import Data.Map.Strict      (Map, insert)
import qualified Data.Map.Strict as M (lookup)
import Data.List            (elem, union)
import Data.Tuple           (snd)
import Data.Maybe           (fromJust)

-- 1. Реализуйте функцию @hasFreeVars@, проверяющую, есть ли в терме свободные переменные.
--    Функция должна быть реализована с помощью 'Reader'.
--    В качестве первого параметра 'Reader' поддерживайте множество видимых в данном 
--    месте терма переменных. (0.5 б)

data Term a = Var a | App (Term a) (Term a) | Lam a (Term a)

hasFreeVars :: forall a. Eq a => Term a -> Bool
hasFreeVars t = runReader (go t) []
  where
    go :: Term a -> Reader [a] Bool
    go (Var x) = do
      bounded <- ask
      pure $ not $ x `elem` bounded
    go (App t1 t2) = do
      ans1 <- go t1
      ans2 <- go t2
      pure $ ans1 || ans2
    go (Lam x t) = do
      local (`union` [x]) (go t)

-- 2. Реализуйте функцию @getBoundedVars@, возвращающую множество связанных переменных терма.
--    Функция должна быть реализована с помощью 'Writer'.
--    В качестве первого параметра 'Writer' поддерживайте множество связанных переменных терма. (0.5 б)

getBoundedVars :: forall a. (Eq a, Ord a) => Term a -> Set a
getBoundedVars t = snd $ runWriter $ go t
  where
    go :: Term a -> Writer (Set a) a
    go (Var x) = pure x
    go (App t1 t2) = do
      go t1
      go t2
    go (Lam x t) = do
      tell $ singleton x
      go t

-- 3. 'State' мощнее, чем 'Reader'. Докажите это, реализовав аналоги
--    ask, asks и local для 'State'. (0.5 б)

instance {-# OVERLAPS #-} MonadReader s (State s) where
  ask = get
  -- asks реализован для State, т.к. для State реализован MonadReader
  local f st = state $ \s -> runState st $ f s

-- 4. 'State' мощнее, чем 'Writer'. Докажите это, реализовав аналоги
--    tell, listen, listens, censor для 'State'. (1 б)

instance {-# OVERLAPS #-} Monoid s => MonadWriter s (State s) where
  tell x = state $ \s -> ((), s <> x)
  listen st = state $ \s -> let (a, s') = runState st s in ((a, s'), s')
  pass stf = state $ \s -> let ((a, f), s') = runState stf s
                               s'' = f s'
                           in (a, s <> s'')

-- listens, censor реализованы для State, т.к. для State реализован MonadWriter

-- | Этот тест должен возвращать "42".
--   Честный 'Writer' тоже вернул бы "42".
--
testWriterState :: String
testWriterState = flip execState mempty $ do
    tell "4"
    censor (pure . last) $ tell "298" >> tell "2"

-- 5. Время для императивного программирования!

-- | 5.1. Реализуйте функцию `whileM_`, исполняющую заданное вычисление,
--        пока первое вычисление возвращает 'True'. (0.1 б)
--
whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ m a = do
  x <- m
  if x then do
    a
    whileM_ m a
  else pure ()

-- | 5.2. Реализуйте функцию `forM_`, являющуюся аналогом цикла for.
--        Перед входом в цикл вычисляется @init@, в итерациях цикла вычисляется @body@, 
--        в конце каждой итерации цикла вычисляется @nextIter@, итерации идут, 
--        пока выполняется условие @cond@. (0.1 б)
--
forM_ :: Monad m => (m (), m Bool, m ()) -> m a -> m ()
forM_ (init, cond, nextIter) body = do
  init
  whileM_ cond $ body >> nextIter

type Context = Map String Int

-- 5.3. Реализуйте функции @setVar@, @incVar@, @getVar@. (0.25 б)

-- | Задаёт значение переменной. 
--   Если переменная есть в контексте, перезаписывает её значение.
--
setVar :: String -> Int -> State Context ()
setVar name value = state $ \s -> ((), insert name value s)

-- | Увеличивает значение переменной. 
--   Если её нет в контексте, то кидает ошибку.
--
incVar :: String -> Int -> State Context ()
incVar name value = state $ \s -> let val = fromJust $ M.lookup name s
                                      s' = insert name (val + value) s
                                  in ((), s')

-- | Достаёт из контекста значение заданной переменной.
--   Если переменной нет в контексте, то кидает ошибку.
--
getVar :: String -> State Context Int
getVar name = state $ \s -> let val = fromJust $ M.lookup name s in (val, s)

-- 5.4. Перепишите один в один (насколько это возможно) представленный ниже плюсовый код,
--      используя монаду 'State' и функции, которые вы реализовали выше. 
--      Напишите 3 теста, проверяющие, что ваша реализация @fib@ работает корректно. (1 б)

{- 
int fib(int n) {
    int prev = 0;
    int cur = 1;

    for (int i = 0; i < n; i = i + 1) {
        int c = cur;
        cur = prev + cur;
        prev = c;
    }

    return cur
} 
-}

fib :: Int -> State Context Int
fib n = do
  setVar "prev" 0
  setVar "cur" 1

  forM_ (setVar "i" 0, getVar "i" >>= pure . (< n), incVar "i" 1) $ do
    cur <- getVar "cur"
    setVar "c" cur
    prev <- getVar "prev"
    setVar "cur" $ prev + cur
    getVar "c" >>= setVar "prev"

  getVar "cur"

testFib :: IO ()
testFib = (assert (fib' 0 == 1) . assert (fib' 1 == 1) . assert (fib' 5 == 8)) $ print "fib works"
  where
    fib' = flip evalState mempty . fib

-- 6. Создайте свою монаду @StateWithError@, обладающую сразу двумя эффектами:
--      a) эффектом монады 'State' (изменяемое состояние);
--      b) эффектои монады 'Either' (возможность завершения вычисления ошибкой). (2 б)
--

newtype StateWithError s e a = StateWE { runStateWE :: s -> Either e (a, s) }

instance Functor (StateWithError s e) where
  fmap = liftM

instance Applicative (StateWithError s e) where
  pure x = StateWE $ \s -> Right (x, s)
  (<*>) = ap

instance Monad (StateWithError s e) where
  StateWE x >>= f = StateWE $ \s -> do
    (a, s') <- x s
    let StateWE x' = f a
    x' s'
