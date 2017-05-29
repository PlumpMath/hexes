
module Main where

-- base
import Control.Monad (when)
import System.Exit (die)
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- transformers
import Control.Monad.IO.Class
-- JuicyPixels
import Codec.Picture

-- hexes
import Hexes

-- | Converts any pixel that is a fully opaque green into a transparent black
-- pixel instead. Any other kind of pixel is unaffected.
greenToAlpha :: PixelRGBA8 -> PixelRGBA8
greenToAlpha (PixelRGBA8 0 255 0 255) = PixelRGBA8 0 0 0 0
greenToAlpha p = p

main = do
    bytes <- B.readFile "font-data/FixedSysExcelsior.png"
    let eStrImg = decodeImage bytes
    baseImage <- pixelMap greenToAlpha <$> either die (return . convertRGBA8) eStrImg
    runHexes 24 80 baseImage $ do
        mainLoop 0.0

mainLoop :: Double -> Hexes ()
mainLoop lastTime = do
    enterLoop <- not <$> windowShouldClose
    when enterLoop $ do
        pollEvents
        newTime <- maybe 0 id <$> getTime
        let deltaTime = newTime - lastTime
        --liftIO $ print $ (newTime - lastTime) * 1000
        refresh
        mainLoop newTime
