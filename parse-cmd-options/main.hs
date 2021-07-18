import System.Console.GetOpt
import Data.Maybe

data Flag = FilePath String | FlagA | FlagB String
          deriving Show

parseOptions :: [String] -> ([Flag], [String], [String])
parseOptions args = getOpt Permute optDescrs args

optDescrs :: [OptDescr Flag]
optDescrs = [ Option ['a'] ["flagA"]    (NoArg FlagA)            "flag A"
            , Option ['f'] ["filePath"] (ReqArg FilePath "DIR")  "file path"
            , Option ['b'] ["flagB"]    (OptArg flagB "FILE")    "flag B"]

flagB = FlagB . fromMaybe "default"

main :: IO ()
main = do
  doParse ["-f", "/home/hoge", "-a", "-b"]
  doParse ["-f", "/home/hoge", "-a", "-b", "aaa"]
  doParse ["-f", "-a", "--flagB=000"]
  doParse ["abs", "-f", "-a", "--flagB=000"]

doParse :: [String] -> IO ()
doParse args = do
  let (options, nonOpts, errMsgs) = parseOptions args
  putStrLn "===Options==="
  print options
  putStrLn "===Non-Options==="
  print nonOpts
  putStrLn "===Error Messages==="
  print errMsgs
