module Salonik.LEDStrip.Client.Main where

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
  L.debug "LEDStrip.Client main()"
  hndl <- udpHandle "192.168.1.27" 8585
  runPattern (lgbtFlag 300) () hndl
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

--
-- TODO: Use <https://hackage.haskell.org/package/colour-2.3.4/docs/Data-Colour-SRGB.html>.
--
runPattern :: forall state. (Word32 -> state -> (state, [RGB])) -> state -> Handle -> IO ()
runPattern pattrn firstState h = do
  let frameTime = 500 * 1000
  sendMsg h SetMsg {queueLength = 10, frameTime = fromIntegral frameTime}
  let loop :: Word32 -> state -> IO ()
      loop frameNumber state = do
        let (nextState, leds) = pattrn frameNumber state
        sendMsg h DataMsg {stripNumber = 0, frameNumber, leds}
        threadDelay frameTime
        loop (frameNumber + 1) nextState
  loop 0 firstState

blackWhite :: Int -> Word32 -> () -> ((), [RGB])
blackWhite numLeds frameNumber _ =
  let black = RGB 0 0 0
      white = RGB 255 255 255
      leds =
        take numLeds . cycle $
        case frameNumber `mod` 2 of
          0 -> [black, white]
          _ -> [white, black]
   in ((), leds)

rotateList :: Int -> [a] -> [a]
rotateList i xs =
  let (ys, zs) = splitAt i xs
   in zs ++ ys

lgbtFlag :: Int -> Word32 -> () -> ((), [RGB])
lgbtFlag =
  let electricRed = RGB 231 0 0
      darkOrange = RGB 255 140 0
      canaryYellow = RGB 255 239 0
      laSalleGreen = RGB 0 129 31
      blue = RGB 0 68 255
      patriarch = RGB 118 0 137
   in movingColorStripes [electricRed, darkOrange, canaryYellow, laSalleGreen, blue, patriarch]

movingColorStripes :: [RGB] -> Int -> Word32 -> () -> ((), [RGB])
movingColorStripes colors numLeds frameNumber _ =
  let numInStripe = numLeds `div` length colors
      almost = concatMap @[_] (replicate numInStripe) colors
      final = almost ++ replicate (numLeds - length almost) (lastEx colors)
   in ((), rotateList (fromIntegral frameNumber `mod` numLeds) final)
