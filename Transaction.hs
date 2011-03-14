module Transaction (withFileTransaction) where
import System.Directory (createDirectory, removeDirectory)
import Control.Concurrent (threadDelay)
import System.IO.Error (isAlreadyExistsError)

tryLock file = catch (createDirectory file) (\e -> if isAlreadyExistsError e then putStrLn "Waiting" >> threadDelay 1000 >> tryLock file else return ())

withFileTransaction ::  FilePath -> IO b -> IO b
withFileTransaction file f = do
  tryLock file
  a <- f
  removeDirectory file
  return a
