{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}


import qualified Control.Exception             as E
import           Control.Monad.Except          (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (MonadState (..), State,
                                                StateT (..), get, modify, put)
import           Control.Monad.Trans           (MonadTrans (..))
import qualified Data.Bifunctor                as BF
import           Data.Function                 (fix)
import           Data.Functor                  (($>))
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Prelude                       hiding (log)
import           System.IO
import           System.Process                (readProcess)
import           Text.ParserCombinators.Parsec as P hiding (State)
import           Text.Printf                   (printf)
import           Text.Read                     (readEither)

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
  deriving (Read, Show)

type YourState = M.Map UserId AccessRights

type YourError = String

type App a = LoggingStateWithErrorInIO YourState YourError a

data Cmd
  = Stop
  | GiveAccess UserId AccessRights
  deriving (Show)


processUserDB :: FilePath -> FilePath -> FilePath -> App ()
processUserDB pathToInputDB pathToLog pathToOutDB = saveLog pathToLog core
  where
    core = do
      inputStr <- safeReadFile pathToInputDB
      inputDB <- handleEither $ traverse (readEither @(UserId, AccessRights)) $ lines inputStr
      log Info "Successfully read the input file"
      put $ M.fromList inputDB
      liftIO $ hSetBuffering stdin NoBuffering
      fix $ \loop -> do
        liftIO $ putStrLn "Type a command (\"GIVE ACCESS <user_id> <access_right>\" or \"STOP\"):"
        cmd <- getCmd
        case cmd of
          Stop -> log Info "Stopped reading commands"
          GiveAccess uid access -> do
            modify $ M.insert uid access
            log Info $ printf "Granted user %d access rights %s" uid (show access)
            loop
      st <- get
      liftIO $ writeFile pathToOutDB $ showState st


showState :: M.Map UserId AccessRights -> String
showState m = unlines $ show <$> M.toList m


showLog :: Log -> String
showLog l = unlines $ (\(level, msg) -> printf "%s: %s" (show level) msg) <$> l


saveLog :: FilePath -> App () -> App ()
saveLog pathToLog app = do
  log <- logs <$> lift (runLoggerT app)
  liftIO $ writeFile pathToLog $ showLog log


parseCmd :: Parser Cmd
parseCmd = parseStop <|> parseGiveAccess where
    parseStop = string "STOP" $> Stop
    parseGiveAccess =  do
      string "GIVE ACCESS"
      spaces
      userID <- many1 digit
      spaces
      accessStr <- many1 alphaNum
      pure $ GiveAccess (read userID) (read accessStr)


getCmd :: App Cmd
getCmd = do
  liftIO $ putStr "> "
  line <- liftIO getLine
  handleEither $ parse parseCmd "" line


safeReadFile' :: FilePath -> IO (Either E.IOException String)
safeReadFile' = E.try . readFile


safeReadFile :: FilePath -> App String
safeReadFile filePath =
  handleEither =<< liftIO (comply <$> safeReadFile' filePath)


comply :: Show e => Either e a -> Either YourError a
comply = BF.first show


handleEither :: Show e => Either e a -> App a
handleEither = either handleError return
  where handleError e = lift (throwError . show $ e)


execApp :: App a -> IO ()
execApp p = either print (const $ return ()) =<< runExceptT (runStateT (runLoggerT p) M.empty)


run :: FilePath -> FilePath -> FilePath -> IO ()
run pathToInputDB pathToLog pathToOutDB = execApp $ processUserDB pathToInputDB pathToLog pathToOutDB


trim :: String -> String
trim = T.unpack . T.strip . T.pack


testRun :: IO ()
testRun = do
  pref <- trim <$> readProcess "sh" ["-c", "pwd"] ""
  run (pref ++ "/input.txt") (pref ++ "/log.txt") (pref ++ "/output.txt")
