import Data.IORef
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (replicateM_)

main :: IO ()
main = do
  let val = "Hoge"
  ioref_val <- newIORef val　-- String を IORef String へ
  (readIORef ioref_val) >>= putStrLn -- readIORef で IORef String を IO String へ
  -- Hoge が出力される
  writeFuga ioref_val -- 書き換える
  (readIORef ioref_val) >>= putStrLn
  -- Fuga が出力される
  modifyIORef ioref_val (\str -> str ++ "Foo")
  (readIORef ioref_val) >>= putStrLn
  -- FugaFoo が出力される
  putStrLn val
  -- Hoge が出力される

  atomicModifyIORef ioref_val (\v -> (v ++ "Atom", undefined))
  (readIORef ioref_val) >>= putStrLn
  -- FugaFooAtom が出力される

  ioref_num <- newIORef (0::Int)
  (readIORef ioref_num) >>= print
  forkIO $ replicateM_ 10 $ atomicModifyIORef ioref_num testFunc
  forkIO $ replicateM_ 10 $ atomicModifyIORef ioref_num testFunc
  threadDelay 1000000
  (readIORef ioref_num) >>= print

  where
    testFunc v = (v+1,undefined)

-- IORefを受け取って中身を書き換える
writeFuga :: IORef String -> IO ()
writeFuga ref = writeIORef ref "Fuga"
