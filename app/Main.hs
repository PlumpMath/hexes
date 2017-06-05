{-# LANGUAGE TemplateHaskell #-}

module Main where

-- base
import Control.Monad (when)
import System.Exit (die)
import Data.Char
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- transformers
import Control.Monad.IO.Class
-- JuicyPixels
import Codec.Picture
-- linear
import Linear
-- embed-file
import Data.FileEmbed

-- hexes
import Hexes

-- | Converts any pixel that is a fully opaque green into a transparent black
-- pixel instead. Any other kind of pixel is unaffected.
greenToAlpha :: PixelRGBA8 -> PixelRGBA8
greenToAlpha (PixelRGBA8 0 255 0 255) = PixelRGBA8 0 0 0 0
greenToAlpha p = p

imageBytes :: ByteString
imageBytes = $(embedFile "font-data/FixedSysExcelsior.png")

main :: IO ()
main = do
    let eStrImg = decodeImage imageBytes
    baseImage <- pixelMap greenToAlpha <$> either die (return . convertRGBA8) eStrImg
    runHexes 24 80 baseImage $ do
        setCharCallback $ Just $ \char -> do
            let o = (fromIntegral $ ord char)
            setTileIDXY o 0 0
            setTileIDXY o 1 1
            setTileIDXY o 2 2
            setTileIDRC (o+1) 0 0
            setTileIDRC (o+1) 1 1
            setTileIDRC (o+1) 2 2
        setGridTileID (fromIntegral $ ord ' ')
        mainLoop 0.0

mainLoop :: Double -> Hexes ()
mainLoop lastTime = do
    enterLoop <- not <$> windowShouldClose
    when enterLoop $ do
        pollEvents
        newTime <- maybe 0 id <$> getTime
        let deltaTime = newTime - lastTime
            bgColor = abs $ realToFrac $ cos newTime
            fgColor = abs $ realToFrac $ sin newTime
        --liftIO $ print $ (newTime - lastTime) * 1000
        --setGridBackground (V3 bgColor bgColor bgColor)
        --setGridForeground (V4 fgColor (1-fgColor) fgColor fgColor)
        refresh
        mainLoop newTime
