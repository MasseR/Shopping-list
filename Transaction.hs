-- |File locking based transaction
module Transaction (withFileTransaction) where
import System.Directory (createDirectory, removeDirectory)
import Control.Concurrent (threadDelay)
import System.IO.Error (isAlreadyExistsError)
import Control.Exception (finally)
import Control.Monad (when)

tryLock file = catch (createDirectory file) (\e -> when (isAlreadyExistsError e) $ putStrLn "Waiting" >> threadDelay 1000 >> tryLock file)

-- |Make a file lock and run the command, removing the lock after execution
withFileTransaction ::
    FilePath -- ^ Path to lock
  -> IO b -- ^ Command to run
  -> IO b -- ^ Return value from the command
withFileTransaction file f = do
  tryLock file
  f `finally` removeDirectory file
