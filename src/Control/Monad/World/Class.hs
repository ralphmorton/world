
module Control.Monad.World.Class (
    MonadConcurrent(..),
    MonadWorld(..),
    TerminalException(..),
    FileException(..),
    TimeException(..)
) where

import Prelude hiding (getLine, putStrLn, readFile, writeFile)

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Exception (Exception, SomeException, handle)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as R
import Control.Monad.State (StateT)
import Control.Monad.Trans
import qualified Data.Time as Time
import qualified System.IO as IO
import qualified System.Random as Random

--
-- Class
--

class Monad m => MonadConcurrent m where
    mapConcurrently :: (Read b, Show b) => (a -> m b) -> [a] -> m [b] -- TODO: do some stuff with Monoid1 or similar to get traversable

instance MonadConcurrent IO where
    mapConcurrently = Async.mapConcurrently

instance (Read e, Show e, MonadConcurrent m) => MonadConcurrent (ExceptT e m) where
    mapConcurrently f ax = do
        results <- lift $ mapConcurrently (runExceptT . f) ax
        case sequence results of
            Left e -> throwError e
            Right bx -> pure bx

instance MonadConcurrent m => MonadConcurrent (ReaderT r m) where
    mapConcurrently f ax = do
        r <- R.ask
        lift $ mapConcurrently (flip runReaderT r . f) ax

class Monad m => MonadWorld m where
    -- System.Concurrent
    threadDelay :: Int -> m ()

    -- System.IO
    getLine :: m (Either TerminalException String)
    putStrLn :: String -> m (Either TerminalException ())
    readFile :: IO.FilePath -> m (Either FileException String)
    writeFile :: IO.FilePath -> String -> m (Either FileException ())

    -- Data.Time
    getCurrentTime :: m (Either TimeException Time.UTCTime)

    -- System.Random
    randomRIO :: (Read a, Show a, Random.Random a) => (a, a) -> m a

instance MonadWorld IO where
    -- System.Concurrent
    threadDelay = Concurrent.threadDelay

    -- System.IO
    getLine = failSome (TerminalException . show) IO.getLine
    putStrLn = failSome (TerminalException . show) . IO.putStrLn
    readFile = failSome (FileException . show) . IO.readFile
    writeFile fp = failSome (FileException . show) . IO.writeFile fp

    -- Data.Time
    getCurrentTime = failSome (TimeException . show) Time.getCurrentTime

    -- System.Random
    randomRIO = Random.randomRIO

failSome :: (SomeException -> e) -> IO a -> IO (Either e a)
failSome f = handle (pure . Left . f) . fmap Right

instance MonadWorld m => MonadWorld (ExceptT e m) where
    -- System.Concurrent
    threadDelay = lift . threadDelay

    -- System.IO
    getLine = lift getLine
    putStrLn = lift . putStrLn
    readFile = lift . readFile
    writeFile fp = lift . writeFile fp

    -- Data.Time
    getCurrentTime = lift getCurrentTime

    -- System.Random
    randomRIO = lift . randomRIO

instance MonadWorld m => MonadWorld (ReaderT r m) where
    -- System.Concurrent
    threadDelay = lift . threadDelay

    -- System.IO
    getLine = lift getLine
    putStrLn = lift . putStrLn
    readFile = lift . readFile
    writeFile fp = lift . writeFile fp

    -- Data.Time
    getCurrentTime = lift getCurrentTime
    
    -- System.Random
    randomRIO = lift . randomRIO

instance MonadWorld m => MonadWorld (StateT r m) where
    -- System.Concurrent
    threadDelay = lift . threadDelay

    -- System.IO
    getLine = lift getLine
    putStrLn = lift . putStrLn
    readFile = lift . readFile
    writeFile fp = lift . writeFile fp

    -- Data.Time
    getCurrentTime = lift getCurrentTime
    
    -- System.Random
    randomRIO = lift . randomRIO

--
-- Exception types
--

data TerminalException
    = TerminalException String
    deriving (Eq, Read, Show)

instance Exception TerminalException

data FileException
    = FileException String
    deriving (Eq, Read, Show)

instance Exception FileException

data TimeException
    = TimeException String
    deriving (Eq, Read, Show)

instance Exception TimeException
