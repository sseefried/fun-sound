{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BS
import           Data.Word
import           Data.Binary.Put

type Time      = Double
type Amplitude = Double

type Sound = Time -> Amplitude



sampleRate :: Word32
sampleRate = 44100

numChannels :: Word16
numChannels = 1

bitsPerSample :: Word16
bitsPerSample = 16

numSamples :: Word32
numSamples = sampleRate * fromIntegral (numChannels * 2) -- 2 seconds

testPut :: Put
testPut = do
  putByteString "RIFF"
  putWord32le $ 36 + subChunk2Size
  putByteString "WAVE"
  -- SubChunk1
  putByteString "fmt "
  putWord32le 16 -- Subchunk1Size. 16 = PCM
  putWord16le 1  -- AudioForamt.   1 = PCM AudioFormat
  putWord16le numChannels  -- NumChannels.   1 = mono, 2 = stereo
  putWord32le sampleRate
  putWord32le $ sampleRate * (fromIntegral (numChannels * bitsPerSample `div` 8)) -- ByteRate
  putWord16le $ numChannels * (bitsPerSample `div` 8) -- BlockAlign
  putWord16le $ bitsPerSample -- BitsPerSample
  -- SubChunk2
  putByteString "data"
  putWord32le $ subChunk2Size
--  toStereoSample 2 (sinWave 400)
  toStereoSample 2 (sinWave 425 `addSound` sinWave 400 `addSound` sinWave 450)
  where
    subChunk2Size = numSamples * fromIntegral (numChannels * bitsPerSample `div` 8)

sinWave :: Double -> Sound
sinWave freq t = sin (2*pi*freq * t)

addSound :: Sound -> Sound -> Sound
addSound s s' t = (s t + s' t) /2

toStereoSample :: Double -> Sound -> Put
toStereoSample duration s = do
  let step = 1/(fromIntegral sampleRate * fromIntegral numChannels)
  sequence_ $ map sampleAt [0, step .. duration - step]
  where
    sampleAt :: Time -> Put
    sampleAt t = do
      putWord16le $ (floor $ s t * (2^15 -1))   -- Left channel


main :: IO ()
main = BS.writeFile "test.wav" $ runPut testPut