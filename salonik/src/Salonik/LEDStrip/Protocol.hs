module Salonik.LEDStrip.Protocol where

import qualified Data.Serialize as S

data RGB = RGB Word8 Word8 Word8 deriving (Eq, Show)

instance S.Serialize RGB where
  put (RGB r g b) = do
    S.putWord8 r
    S.putWord8 g
    S.putWord8 b
  get = RGB <$> S.getWord8 <*> S.getWord8 <*> S.getWord8

-- UDP packet DATA:
-- [1 byte] packet type [0: DATA, 1:SET]
-- [1 byte] strip number
-- [4 bytes] frame number
-- [2 bytes] N = length
-- [3 bytes x N] rgb led data
data DataMsg = DataMsg
  { stripNumber :: Word8 -- 0-7
  , frameNumber :: Word32
  , leds :: [RGB]
  } deriving (Eq, Show)

instance S.Serialize DataMsg where
  put DataMsg{..} = do
    S.putWord8 0
    S.putWord8 stripNumber
    S.putWord32le frameNumber
    S.putWord16le (fromIntegral . length $ leds)
    mapM_ S.put leds
  get = do
    0 <- S.getWord8
    stripNumber <- S.getWord8
    frameNumber <- S.getWord32le
    len <- S.getWord16le
    leds <- replicateM (fromIntegral len) S.get
    pure DataMsg{..}

-- UDP packet SET:
-- [1 byte] packet type [0: DATA, 1:SET]
-- [1 byte] queue length
-- [4 byte] frame time
data SetMsg = SetMsg
  { queueLength :: Word8 -- 1-64
  , frameTime :: Word32 -- [µs]
  }

instance S.Serialize SetMsg where
  put SetMsg {..} = do
    S.putWord8 1
    S.putWord8 queueLength
    S.putWord32le frameTime
  get = do
    1 <- S.getWord8
    queueLength <- S.getWord8
    frameTime <- S.getWord32le
    pure SetMsg{..}

-- UDP packet ACK:
-- [1 byte] strip number
-- [4 bytes] frame number
-- [1 byte] frames left in buffer (0 = buffer is starved)
-- [4 byte] minimal micros last frame
data AckMsg = AckMsg
  { stripNumber :: Word8 -- 0-7
  , frameNumber :: Word32
  , framesRemainingInBuffer :: Word8 -- 0 = buffer starvation
  , minimalMicrosLastFrame :: Word32 -- [µs]
  }

instance S.Serialize AckMsg where
  put AckMsg{..} = do
    S.putWord8 stripNumber
    S.putWord32le frameNumber
    S.putWord8 framesRemainingInBuffer
    S.putWord32le minimalMicrosLastFrame
  get = do
    stripNumber <- S.getWord8
    frameNumber <- S.getWord32le
    framesRemainingInBuffer <- S.getWord8
    minimalMicrosLastFrame <- S.getWord32le
    pure AckMsg{..}
