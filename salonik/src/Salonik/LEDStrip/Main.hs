module Salonik.LEDStrip.Main where

import           Control.Concurrent        (threadDelay)
import           Data.List                 (cycle)
import qualified Data.Serialize            as S
import           Network.Socket            (PortNumber, SockAddr (SockAddrInet),
                                            setSocketOption, socketToHandle)
import qualified Network.Socket            as Socket
import           Salonik.LEDStrip.Protocol
import qualified Salonik.Log               as L
import           System.IO                 (Handle, IOMode (WriteMode))

main :: IO ()
main = do
  L.debug "LEDStrip main()"
  hndl <- udpHandle "192.168.1.27" 8585
  prettyPattern hndl
  hClose hndl

sendMsg :: (S.Serialize a, Show a) => Handle -> a -> IO ()
sendMsg hndl a = do
  L.debug $ "Sending: " ++ tshow a
  hPut hndl . S.runPut . S.put $ a

udpHandle :: String -> PortNumber -> IO Handle
udpHandle host port = do
  socket <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
  setSocketOption socket Socket.ReusePort 1
  address <- Socket.inet_addr host
  Socket.connect socket (SockAddrInet port address)
  socketToHandle socket WriteMode

prettyPattern :: Handle -> IO ()
prettyPattern h = do
  let frameTime = 1000 * 1000
  sendMsg h SetMsg {queueLength = 10, frameTime = fromIntegral frameTime}
  forM_ @[_] [0 ..] $ \frameNumber -> do
    threadDelay frameTime
    let black = RGB 0 0 0
        white = RGB 255 255 255
        leds =
          take 300 . cycle $
          case frameNumber `mod` 2 of
            0 -> [black, white]
            _ -> [white, black]
    sendMsg h DataMsg {stripNumber = 0, frameNumber, leds}
