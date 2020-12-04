module Main(main) where

import           Control.Concurrent        (forkFinally)
import qualified Control.Exception         as E
import           Control.Monad             (forever, unless, void)
import qualified Data.ByteString.Char8     as C
import           Data.Either
import           Data.Maybe
import           Network.Socket
import           Network.Socket.ByteString (recv, recvFrom, sendAllTo)
import           Parameter
import           System.Environment        (getArgs)

main :: IO ()
main = do
  parameter <- parseArgs <$> getArgs
  either putStrLn runServer parameter

runServer :: Parameter -> IO ()
runServer p = withSocketsDo $ do
  print p
  addr <- head <$> getAddrInfo (Just $ defaultHints{addrFlags=[AI_PASSIVE]}) (Just $ hostName p) (Just $ servName p)
  E.bracket (open addr $ protocolType p) close (if protocolType p == TCP then loopTcp else loopUdp)
  where
    open addr t = do
      sock <- socket (addrFamily addr) (if t == TCP then Stream else Datagram) defaultProtocol
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      if t == TCP then
        do
          bind sock $ addrAddress addr
          listen sock 5
      else
        bind sock $ addrAddress addr
      return sock

    loopTcp sock = forever $ do
      (conn, peer) <- accept sock
      void $ forkFinally ((getHandler $ handler p) conn) (const $ gracefulClose conn 5000)

    loopUdp sock = forever $ (getHandler $ handler p) sock


echoHandler :: Handler
echoHandler s = do
  (msg, sender) <- recvFrom s 1024
  unless (C.null msg) $ do
     sendAllTo s msg sender
     echoHandler s


dispHandler :: Handler
dispHandler s = do
  msg <- recv s 1024
  unless (C.null msg) $ do
    print msg
    dispHandler s


getHandler :: HandlerType -> Handler
getHandler Echo = echoHandler
getHandler Disp = dispHandler
