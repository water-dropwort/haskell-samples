module Parameter (
                 Parameter,
                 parseArgs,
                 hostName,
                 servName,
                 handler,
                 protocolType,
                 Handler,
                 ProtocolType(..),
                 HandlerType(..) ) where

import           Network.Socket

type Handler = Socket -> IO ()
data ProtocolType = TCP | UDP deriving (Show, Eq)
data HandlerType = Echo | Disp deriving (Show, Eq)

data Parameter = Parameter{
  hostName::HostName,
  servName::ServiceName,
  handler::HandlerType,
  protocolType::ProtocolType } deriving Show

updateHostName h p = Parameter{hostName=h, servName=servName p, handler=handler p, protocolType=protocolType p}
updateServName s p = Parameter{hostName=hostName p, servName=s, handler=handler p, protocolType=protocolType p}
updateHandler h p = Parameter{hostName=hostName p, servName=servName p, handler=h, protocolType=protocolType p}
updateProtocolType t p = Parameter{hostName=hostName p, servName=servName p,handler=handler p, protocolType=t}

parseArgs :: [String] -> Either String Parameter
parseArgs = foldr parseArg (Right defParam)
  where
    defParam = Parameter{
      hostName = "127.0.0.1",
      servName = "8888",
      handler = Echo,
      protocolType = TCP}

    parseArg :: String -> Either String Parameter -> Either String Parameter
    parseArg _ (Left err) = Left err
    parseArg [] _ = Left "Parse err : empty."
    parseArg arg (Right old) = case toCmdAndValue arg of
      (_ ,[]) -> Left $ "Parse err : invalid format. Arg = " ++ arg
      ("-HostName", host) -> Right $ updateHostName host old
      ("-ServName", serv) -> Right $ updateServName serv old
      ("-Handler", handler) -> trySetHandler handler old
      ("-Protocol", proto) -> trySetProtocolType proto old
      (_, _) -> Left $ "Parse err : undefined command. Arg = " ++ arg

    toCmdAndValue arg = (takeWhile (/= '=') arg, safetail (dropWhile (/= '=') arg))
      where
        safetail xs = if null xs then [] else tail xs

    trySetHandler han para
      | han == "Echo" = Right $ updateHandler Echo para
      | han == "Disp" = Right $ updateHandler Disp para
      | otherwise = Left $ "Parse err : undefined handler type. Input = " ++ han

    trySetProtocolType t para
      | t == "TCP" = Right $ updateProtocolType TCP para
      | t == "UDP" = Right $ updateProtocolType UDP para
      | otherwise = Left $ "Parse err : undefined protocol type. Input = " ++ t
