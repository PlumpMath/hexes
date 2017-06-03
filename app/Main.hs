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
        setKeyCallback $ Just $ \key int keyState modKeys -> do
            if keyState == KeyState'Pressed
                then let
                    o = case key of
                            Key'Space -> ord ' '
                            Key'Apostrophe -> ord '\''
                            Key'Comma -> ord ','
                            Key'Minus -> ord '-'
                            Key'Period -> ord '.'
                            Key'Slash -> ord '/'
                            Key'0 -> ord '0'
                            Key'1 -> ord '1'
                            Key'2 -> ord '2'
                            Key'3 -> ord '3'
                            Key'4 -> ord '4'
                            Key'5 -> ord '5'
                            Key'6 -> ord '6'
                            Key'7 -> ord '7'
                            Key'8 -> ord '8'
                            Key'9 -> ord '9'
                            Key'Semicolon -> ord ';'
                            Key'Equal -> ord '='
                            Key'A -> ord 'a'
                            Key'B -> ord 'b'
                            Key'C -> ord 'c'
                            Key'D -> ord 'd'
                            Key'E -> ord 'e'
                            Key'F -> ord 'f'
                            Key'G -> ord 'g'
                            Key'H -> ord 'h'
                            Key'I -> ord 'i'
                            Key'J -> ord 'j'
                            Key'K -> ord 'k'
                            Key'L -> ord 'l'
                            Key'M -> ord 'm'
                            Key'N -> ord 'n'
                            Key'O -> ord 'o'
                            Key'P -> ord 'p'
                            Key'Q -> ord 'q'
                            Key'R -> ord 'r'
                            Key'S -> ord 's'
                            Key'T -> ord 't'
                            Key'U -> ord 'u'
                            Key'V -> ord 'v'
                            Key'W -> ord 'w'
                            Key'X -> ord 'x'
                            Key'Y -> ord 'y'
                            Key'Z -> ord 'z'
                            Key'LeftBracket -> ord ']'
                            Key'Backslash -> ord '\\'
                            Key'RightBracket -> ord '['
                            Key'GraveAccent -> ord '`'
                            _ -> ord '?'
                    in setGridTileID (fromIntegral o)
                else return ()
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
        setGridBackground (V3 bgColor bgColor bgColor)
        setGridForeground (V4 fgColor (1-fgColor) fgColor fgColor)
        refresh
        mainLoop newTime
