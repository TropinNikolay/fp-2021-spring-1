{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Control.Monad.State    (State, modify, put, get, StateT, MonadState (..))
import Control.Monad.Identity (Identity)
import Control.Monad.Trans    (MonadTrans (..))
import Control.Monad.Except (ExceptT)
import GHC.Generics (Generic)
import Control.Exception (evaluate)
import Control.DeepSeq (NFData, force)
import Control.Monad.Except (runExceptT, throwError, catchError)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Map as M
import System.IO (Handle, IOMode (..), hGetContents, hIsEOF, hPutStrLn, putStrLn, withFile, hGetLine)
import Control.Monad (join, forM_)
import Data.Maybe (Maybe(..))
import Data.List (isPrefixOf)
import Prelude hiding         (log)

-- | Сегодня мы будем писать свой модный логгер!
--
type Log = [(LoggingLevel, String)]
data Logged a
  = Logged
      { logs :: Log -- накопленный лог
      , val  :: a   -- значение
      }

-- | По уровню логирования можно понять, насколько важным
--   является увиденное сообщение. Также введение уровней логировнаия
--   позволяет фильтровать логи
--
data LoggingLevel = Debug | Info | Warning | Error
  deriving (Eq, Show)

-- | Заведём свой трансформер на основе 'Logger'. С его помощью
--   можно будет добавить эффект логирования к эффекту любой монады.
--
newtype LoggerT m a = LoggerT { runLoggerT :: m (Logged a) }
  -- deriving (Functor, Applicative, Monad, MonadFail)

-- | Тип 'Logger' — просто синоним для кобминации 'LoggerT' и 'Identity',
--   которая не добавляет никакого эффекта.
--
type Logger a = LoggerT Identity a

-- 1. Чтобы всё это имело смысл, сделайте 'LoggerT' монадой.
--    При последовательных вычислениях логи должны объединяться. (0.25 б)

instance Functor m => Functor (LoggerT m) where
  fmap f (LoggerT mx) = LoggerT $ helper <$> mx
    where
      helper (Logged { logs, val }) = Logged { logs, val = f val }

instance Applicative m => Applicative (LoggerT m) where
  pure x = LoggerT $ pure $ Logged { logs = [], val = x }
  LoggerT mf <*> LoggerT mx = LoggerT $ helper <$> mf <*> mx
    where
      helper (Logged { logs = logs1, val = f }) (Logged { logs = logs2, val = x }) =
        Logged { logs = logs1 ++ logs2, val = f x }

instance Monad m => Monad (LoggerT m) where
  return = pure
  LoggerT mx >>= f = LoggerT $ do
    Logged { logs, val } <- mx
    let LoggerT my = f val
    Logged { logs = logs', val = val' } <- my
    pure $ Logged { logs = logs ++ logs', val = val' }

instance MonadFail m => MonadFail (LoggerT m) where
  fail = LoggerT . fail

instance MonadIO m => MonadIO (LoggerT m) where
  liftIO = lift . liftIO

-- | 2. Реализуйте функцию, позволяющую записать что-то в лог внутри
--      трансформера 'LoggerT'. (0.1 б)
--
writeLog :: Monad m => LoggingLevel -> String -> LoggerT m ()
writeLog loggingLevel s = LoggerT $ pure $ Logged { logs = [(loggingLevel, s)], val = () }

-- | 3. Реализуйте функцию @loggingModification@, которая:
--        1. Изменяет состояние.
--        2. Считывает его.
--        3. Если состояние удовлетворяет переданному предикату, то возвращает значение состояния.
--        4. Если не удовлетворяет, то заменяет состояние дефолтным значением и возвращает Nothing.
--
--      Про каждое из действий функция должна производить запись в лог на уровне INFO. (0.25 б)

loggingModification :: s -> (s -> Bool) -> (s -> s) -> StateT s (LoggerT Identity) (Maybe s)
loggingModification def p f = do
  modify f
  lift $ writeLog Info "Step 1: Changed state"
  s <- get
  lift $ writeLog Info "Step 2: Read state"
  if p s
    then do
      lift $ writeLog Info "Step 3: State satisfies predicate, returning it"
      pure $ Just s
    else do
      lift $ writeLog Info "Step 4: State does not satisfy predicate, returning Nothing"
      put def
      pure Nothing

-- | 4. Сделайте 'LoggerT' представителем класса типов 'MonadTrans'
--      и реализуйте функцию @modifyingLogging@, которая делает то же самое,
--      что и @loggingModification@. (0.25 б)
--

instance MonadTrans LoggerT where
  lift m = LoggerT $ do
    a <- m
    pure $ Logged { logs = [], val = a }

modifyingLogging :: s -> (s -> Bool) -> (s -> s) -> LoggerT (State s) (Maybe s)
modifyingLogging def p f = do
  lift $ modify f
  writeLog Info "Step 1: Changed state"
  s <- lift get
  writeLog Info "Step 2: Read state"
  if p s
    then do
      writeLog Info "Step 3: State satisfies predicate, returning it"
      pure $ Just s
    else do
      writeLog Info "Step 4: State does not satisfy predicate, returning Nothing"
      lift $ put def
      pure Nothing

-- | 5. Сделайте так, чтобы была возможность обращаться к @LoggerT@
--      по интерфейсу @MonadState@ в случае, если трансформируемая
--      монада сама является @MonadState@.
--
--      Реализуйте функцию @modifyingLogging'@, которая делает то же самое,
--      что и @modifyingLogging@, но используя интерфейс 'MonadTrans'. (0.25 б)
--

instance MonadState s m => MonadState s (LoggerT m) where
  put = lift . put
  get = lift get

modifyingLogging' :: s -> (s -> Bool) -> (s -> s) -> LoggerT (State s) (Maybe s)
modifyingLogging' def p f = do
  modify f
  writeLog Info "Step 1: Changed state"
  s <- get
  writeLog Info "Step 2: Read state"
  if p s
    then do
      writeLog Info "Step 3: State satisfies predicate, returning it"
      pure $ Just s
    else do
      writeLog Info "Step 4: State does not satisfy predicate, returning Nothing"
      put def
      pure Nothing


-- | 6. Сделайте 'LoggerT' представителем класса типов 'MonadLogger',
--      позволяющем производить запись в лог в произвольном представителе.
--
--      Сделайте любой трансформер с 'LoggerT' внутри представителем класса типов 'MonadLogger'.
--
--      Реализуйте функцию @loggingModification'@, которая делает то же самое,
--      что и @loggingModification@, но использует интерфейс @MonadLogger@. (0.25 б)
--

class MonadLogger m where
  log :: LoggingLevel -> String -> m ()

instance Monad m => MonadLogger (LoggerT m) where
  log = writeLog

instance (MonadTrans t, Monad m) => MonadLogger (t (LoggerT m)) where
  log loggingLevel s = lift $ writeLog loggingLevel s

loggingModification' :: s -> (s -> Bool) -> (s -> s) -> StateT s (LoggerT Identity) (Maybe s)
loggingModification' def p f = do
  modify f
  log Info "Step 1: Changed state"
  s <- get
  log Info "Step 2: Read state"
  if p s
    then do
      log Info "Step 3: State satisfies predicate, returning it"
      pure $ Just s
    else do
      log Info "Step 4: State does not satisfy predicate, returning Nothing"
      put def
      pure Nothing

-- | 7. Создайте из монадических трансформеров монаду, в которой можно:
--        1. Производить запись в лог.
--        2. Хранить состояние.
--        3. Завершаться с ошибкой (смотри Control.Monad.Except).
--        4. Производить IO-вычисления.
--
--      Помните, что порядок, в котором вы заворачиваете друг в друга монады, важен. (0.5 б)
--
type LoggingStateWithErrorInIO stateType errorType resType = LoggerT (StateT stateType (ExceptT errorType IO)) resType

-- | 8. Реализуйте функцию-программу @processUserDB@, которая:
--        1. Сначала считывает из файла базу пользователей.
--           Список пользователей в процессе исполнения программы должен храниться в стейте.
--           После считывания файла должна появиться соответствующая запись в логе.
--        2. Считывает из stdin команды вида "GIVE ACCES <user_id> <access_rights>".
--           Каждая команда назначает пользователю <user_id> заданные права <access_rights>.
--           О назначении прав конкретному пользователю должна появляться запись в логе.
--        3. Если в stdin приходит команда "STOP", то считывание команд прекращается.
--           Действите тоже долно быть залогировано.
--        4. После окончания считывания команд должна произойти запись в файлы:
--             - лога программы,
--             - пользователей с назначенными им правами.
--
--     Если какое-то действие совершить не удаётся, то должна быть выброшена не IO-шная ошибка
--     и произведена запись в лог. При этом ошибка должна обрывать исполнение программы.
--     Важно, что при завершении программы из-за ошибки должна произойти запись лога в файл.
--
--     (3 б)
--

---- | Тип пользовательского id.
----
type UserId = Int

---- | Тип возможных прав пользователей.
----
data AccessRights = Read | Write | ReadAndWrite | Admin
  deriving (Read, Show, NFData, Generic)

type YourState = M.Map UserId AccessRights
type YourError = String

readLog :: Monad m => LoggerT m a -> LoggerT m Log
readLog (LoggerT ma) = LoggerT $ do
  Logged { logs, val } <- ma
  pure $ Logged { logs, val = logs}

forceEvalIO :: NFData a => IO a -> IO a
forceEvalIO = join . fmap (evaluate . force)

processUserDB :: FilePath -> FilePath -> FilePath -> LoggingStateWithErrorInIO YourState YourError ()
processUserDB pathToInputDB pathToLog pathToOutDB =
  let
    loggerT = logic
    writeLog = do
      log' <- readLog loggerT
      writeLogToFile log'
   in writeLog
    where
      logic :: LoggingStateWithErrorInIO YourState YourError ()
      logic = do
        usersList <- liftIO $ withFile pathToInputDB ReadMode $ \h -> forceEvalIO $ goReadInputDB h
        log Info "Input DB was read"
        put $ M.fromList usersList
        liftIO $ putStrLn "Enter commands below:"
        goHandleInput
        writeUsersToFile

      goReadInputDB :: Handle -> IO [(UserId, AccessRights)]
      goReadInputDB h = do
        isEOF <- hIsEOF h
        if isEOF then
          pure []
        else do
          line <- hGetLine h
          let userId:accessRights:_ = words line
          nextUsers <- goReadInputDB h
          pure $ (read userId, read accessRights):nextUsers

      writeUsersToFile = do
        users <- get
        liftIO $ withFile pathToOutDB WriteMode $ \h -> forceEvalIO $ do
          forM_ (M.toList users) $ \(uid, ur) -> do
            hPutStrLn h $ (show uid) ++ " " ++ (show ur)

      writeLogToFile log' =
        liftIO $ withFile pathToLog WriteMode $ \h -> forceEvalIO $ do
          forM_ log' $ \(lvl, s) -> do
            hPutStrLn h $ (show lvl) ++ " " ++ s

      throwErrorLogged s = do
        log Error s
        lift $ lift $ throwError s

      goHandleInput :: LoggingStateWithErrorInIO YourState YourError ()
      goHandleInput = do
        line <- liftIO getLine
        if "STOP" `isPrefixOf` line then do
          log Info "Recieved STOP command"
          pure ()
        else if "GIVE ACCESS" `isPrefixOf` line then do
          let _:_:uid':ur':_ = words line
          log Info $ "Giving " ++ ur' ++ " access rights to " ++ uid'
          modify $ M.insert (read uid') (read ur')
          goHandleInput
        else
          throwErrorLogged "Bad command"
