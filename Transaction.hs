module Transaction (withFileTransaction) where
import System.Directory (createDirectory, removeDirectory)
import Control.Concurrent (threadDelay)
import System.IO.Error (isAlreadyExistsError)
import Control.Exception (finally)
import Control.Monad (when)

tryLock file = catch (createDirectory file) (\e -> when (isAlreadyExistsError e) $ putStrLn "Waiting" >> threadDelay 1000 >> tryLock file)

withFileTransaction ::  FilePath -> IO b -> IO b
withFileTransaction file f = do
  tryLock file
  f `finally` removeDirectory file
