
module Control.Monad.World.Class (
    MonadConcurrent(..),
    TimeException(..),
    MonadTime(..),
    TerminalException(..),
    MonadTerminal(..),
    FileException(..),
    MonadFile(..),
    MonadRandom(..)
) where

import Prelude hiding (getLine, putStr, putStrLn, readFile, writeFile)

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Exception (Exception, SomeException, handle)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as R
import Control.Monad.State (StateT)
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time as Time
import qualified System.IO as IO
import qualified System.Random as Random

--
-- Concurrent
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

--
-- Time
--

data TimeException
    = TimeException String
    deriving (Eq, Read, Show)

instance Exception TimeException

class Monad m => MonadTime m where
    threadDelay :: Int -> m ()
    getCurrentTime :: m (Either TimeException Time.UTCTime)

instance MonadTime IO where
    threadDelay = Concurrent.threadDelay
    getCurrentTime = failSome (TimeException . show) Time.getCurrentTime

instance MonadTime m => MonadTime (ExceptT e m) where
    threadDelay = lift . threadDelay
    getCurrentTime = lift getCurrentTime

instance MonadTime m => MonadTime (ReaderT r m) where
    threadDelay = lift . threadDelay
    getCurrentTime = lift getCurrentTime

instance MonadTime m => MonadTime (StateT r m) where
    threadDelay = lift . threadDelay
    getCurrentTime = lift getCurrentTime

--
-- Terminal
--

data TerminalException
    = TerminalException String
    deriving (Eq, Read, Show)

instance Exception TerminalException

class Monad m => MonadTerminal m where
    getLine :: m (Either TerminalException String)
    putStr :: String -> m (Either TerminalException ())
    putStrLn :: String -> m (Either TerminalException ())

instance MonadTerminal IO where
    getLine = failSome (TerminalException . show) IO.getLine
    putStr = failSome (TerminalException . show) . IO.putStr
    putStrLn = failSome (TerminalException . show) . IO.putStrLn

instance MonadTerminal m => MonadTerminal (ExceptT e m) where
    getLine = lift getLine
    putStr = lift . putStr
    putStrLn = lift . putStrLn

instance MonadTerminal m => MonadTerminal (ReaderT r m) where
    getLine = lift getLine
    putStr = lift . putStr
    putStrLn = lift . putStrLn

instance MonadTerminal m => MonadTerminal (StateT r m) where
    getLine = lift getLine
    putStr = lift . putStr
    putStrLn = lift . putStrLn

--
-- File
--

data FileException
    = FileException String
    deriving (Eq, Read, Show)

instance Exception FileException

class Monad m => MonadFile m where
    readFile :: IO.FilePath -> m (Either FileException String)
    writeFile :: IO.FilePath -> String -> m (Either FileException ())
    readFileT :: IO.FilePath -> m (Either FileException T.Text)
    writeFileT :: IO.FilePath -> T.Text -> m (Either FileException ())
    readFileBS :: IO.FilePath -> m (Either FileException B.ByteString)
    writeFileBS :: IO.FilePath -> B.ByteString -> m (Either FileException ())
    readFileLBS :: IO.FilePath -> m (Either FileException BL.ByteString)
    writeFileLBS :: IO.FilePath -> BL.ByteString -> m (Either FileException ())

instance MonadFile IO where
    readFile = failSome (FileException . show) . IO.readFile
    writeFile fp = failSome (FileException . show) . IO.writeFile fp
    readFileT = failSome (FileException . show) . TIO.readFile
    writeFileT fp = failSome (FileException . show) . TIO.writeFile fp
    readFileBS = failSome (FileException . show) . B.readFile
    writeFileBS fp = failSome (FileException . show) . B.writeFile fp
    readFileLBS = failSome (FileException . show) . BL.readFile
    writeFileLBS fp = failSome (FileException . show) . BL.writeFile fp

instance MonadFile m => MonadFile (ExceptT e m) where
    readFile = lift . readFile
    writeFile fp = lift . writeFile fp
    readFileT = lift . readFileT
    writeFileT fp = lift . writeFileT fp
    readFileBS = lift . readFileBS
    writeFileBS fp = lift . writeFileBS fp
    readFileLBS = lift . readFileLBS
    writeFileLBS fp = lift . writeFileLBS fp

instance MonadFile m => MonadFile (ReaderT r m) where
    readFile = lift . readFile
    writeFile fp = lift . writeFile fp
    readFileT = lift . readFileT
    writeFileT fp = lift . writeFileT fp
    readFileBS = lift . readFileBS
    writeFileBS fp = lift . writeFileBS fp
    readFileLBS = lift . readFileLBS
    writeFileLBS fp = lift . writeFileLBS fp

instance MonadFile m => MonadFile (StateT r m) where
    readFile = lift . readFile
    writeFile fp = lift . writeFile fp
    readFileT = lift . readFileT
    writeFileT fp = lift . writeFileT fp
    readFileBS = lift . readFileBS
    writeFileBS fp = lift . writeFileBS fp
    readFileLBS = lift . readFileLBS
    writeFileLBS fp = lift . writeFileLBS fp

--
-- Random
--

class Monad m => MonadRandom m where
    randomR :: (Read a, Show a, Random.Random a) => (a, a) -> m a

instance MonadRandom IO where
    randomR = Random.randomRIO

instance MonadRandom m => MonadRandom (ExceptT e m) where
    randomR = lift . randomR

instance MonadRandom m => MonadRandom (ReaderT r m) where
    randomR = lift . randomR

instance MonadRandom m => MonadRandom (StateT r m) where
    randomR = lift . randomR

--
-- Util
--


failSome :: (SomeException -> e) -> IO a -> IO (Either e a)
failSome f = handle (pure . Left . f) . fmap Right