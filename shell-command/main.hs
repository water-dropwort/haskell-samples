import System.Process
import System.Exit

main :: IO ()
main = do
  let cmd = "ls --help"
  (exit_code, std_out, std_err) <- readCreateProcessWithExitCode (shell cmd) ""
  putStrLn "=== Exit Code ==="
  case exit_code of
    ExitSuccess -> putStrLn "ExitSuccess"
    ExitFailure ec -> print ec
  putStrLn "=== Standard Output ==="
  putStrLn std_out
  putStrLn "=== Standard Error ==="
  putStrLn std_err
