import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor      (first)
import Control.Monad.Identity (Identity)
import Debug.Trace

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance Applicative m => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure

    MaybeT fMM <*> MaybeT aMM = MaybeT $ helper <$> fMM <*> aMM
      where
        helper fM aM = 
          case (fM, aM) of
            (Nothing, _)      -> Nothing
            (Just f, Nothing) -> Nothing
            (Just f, Just a)  -> pure $ f a

instance Monad m => Monad (MaybeT m) where
    return = pure

    MaybeT aMM >>= f = MaybeT $ do 
        aM <- aMM

        case aM of
          Just a  -> runMaybeT $ f a
          Nothing -> pure Nothing

instance MonadTrans MaybeT where
    -- lift :: Monad m => m a -> MaybeT m a
    lift = MaybeT . fmap pure

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap f (StateT aS) = StateT $ \s -> fmap (first f) $ aS s

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a, s)

    StateT fS <*> StateT aS = StateT $ \s0 -> do
        (f, s1) <- fS s0
        (a, s2) <- aS s1
        pure (f a, s2)

instance Monad m => Monad (StateT s m) where
    return = pure

    StateT fA >>= fToB = StateT $ \s -> do
        (a, s1) <- fA s
        runStateT (fToB a) s1

instance MonadTrans (StateT s) where
    -- lift :: Monad m => m a -> StateT m a
    lift aM = StateT $ \s -> fmap (flip (,) s) aM

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = StateT $ \s -> pure ((), f s)

-- Если трансформировать 'Maybe' до 'State', то тип у вычисляемого 
-- значения получится такой: StateT s Maybe a = s -> Maybe (a, s).
-- Получается, что если в ходе вычислений где-то всплывёт Nothing,
-- то вся цепочка вычислений прервётся. 
--
-- Что видно на примере:
-- > runStateT example ""
--   Nothing
-- 
example :: StateT String Maybe ()
example = do
    modify (<> "aaa")
    lift Nothing
    modify (<> "bbb")

-- Если трансформировать 'State' до 'Maybe', то тип получится
-- другой: MaybeT (State s) a = s -> (Maybe a, s).
-- И здесь возникновение значения Nothing, хотя и прерывает вычисление 
-- (см. определение 'MaybeT'), не мешает сохранению его эффекта.
-- Так как эффект у 'State' — изменяемое состояние, то мы всегда сможем
-- получить доступ к состоянию, которое было на момент, когда вычисление прервалось.
--
-- Это видно на примере:
-- > runStateT (runMaybeT example') ""
--   "aaa"
--
example' :: MaybeT (StateT String Identity) ()
example' = do
    lift $ modify (<> "aaa")
    (MaybeT $ StateT $ \s -> pure (Nothing, s))
    lift $ modify (<> "bbb")
