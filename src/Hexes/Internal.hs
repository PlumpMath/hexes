{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hexes.Internal where

-- base
import Control.Exception (bracket)
import Control.Monad (when, mapM_)
import Data.Word (Word8)
import System.Exit (die)
-- GLFW-b
import qualified Graphics.UI.GLFW as GLFW
-- gl
import Graphics.GL.Core33
import Graphics.GL.Types
-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.State
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- JuicyPixels
import Codec.Picture
-- vector
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
-- linear
import Linear
-- storable-tuple
import Foreign.Storable.Tuple

imageRows :: Integral i => i
imageRows = 10

imageCols :: Integral i => i
imageCols = 26

-- | Private. State tracked by the Hexes computation.
data HexesConfig = HexesConfig {
    -- | How many rows we're showing in our screen
    rowCount :: !Int,

    -- | How many columns we're showing in our screen
    colCount :: !Int,

    -- | The image data that we're going to be sending to OGL and displaying
    -- within each screen cell.
    tileMap :: Image PixelRGBA8,

    -- | The GLFW window we do all our operations with.
    window :: GLFW.Window,

    -- | A vector that you index with a Word8 value to get the texture quadruple
    -- to use to display the associated location within the tile map. If we ever
    -- allow tile maps to be reloaded, we'll have to regenerate this data as
    -- part of the reload process (among other things).
    textureLocationCache :: VS.Vector (V2 GLfloat,V2 GLfloat,V2 GLfloat,V2 GLfloat)
    }

-- | Danger! This makes a 'HexesConfig' value with various 'undefined' entries
-- in it, which you must then fill via the proper configuation steps before you
-- allow it to be used in a general way.
mkStartConfig :: Int -> Int -> Image PixelRGBA8 -> HexesConfig
mkStartConfig rows cols alphaImage = HexesConfig {
    rowCount = rows,
    colCount = cols,
    tileMap = alphaImage,
    window = undefined,
    textureLocationCache = undefined
    }

-- | A Hexes computation is one that wraps up whole a lot of 'GLFW' and 'gl'
-- activity so that you can easily manipulate a grid of characters and have it
-- be rendered to the screen, similar to how curses works. Or, how it might work
-- if it was much easier to work with at least.
--
-- Though this is a MonadIO newtype, it is *not* suggested to call any 'GLFW' or
-- 'gl' code from within this monad yourself. Anything you should be interacting
-- with from those packages is already provided to you as a 'Hexes' action
-- instead. If you make your own calls to 'GLFW' or 'gl' from within a 'Hexes'
-- action and something gets messed up, that's your fault.
newtype Hexes a = Hexes (StateT HexesConfig IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Converts a Hexes computation into an IO computation. You have to provide
-- the number of rows and cols you want within the glyph grid that you work
-- with, and also a ByteString containing the image data that should be used
-- during rendering. A ByteString is accepted so that you can either load the
-- bytes from disk when your program starts, or you can use the 'file-embed'
-- package to bundle the tile data directly into your final binary.
runHexes :: Int -> Int -> ByteString -> Hexes () -> IO ()
runHexes rows cols textureBytes userAction = do
    let eStrImg = decodeImage textureBytes
    baseImage <- either die (return . convertRGBA8) eStrImg :: IO (Image PixelRGBA8)
    let alphaImage = pixelMap greenToAlpha baseImage
    let (Hexes whole) = do
            -- TODO: Install a GLFW Error Handler before all other GLFW things.
            glfwDefaultHints
            initWindow
            regenerateTextureLocations
            -- TODO: all sorts of other setup
            userAction
    bracketGLFW $ flip evalStateT (mkStartConfig rows cols alphaImage) whole

-- | Ensures that we only run GLFW code while it's initialized, and also that we
-- always terminate it when we're done. Also, this function should only be used
-- from the main thread.
bracketGLFW :: IO () -> IO ()
bracketGLFW act = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
    when initWorked act

-- | Converts any pixel that is a fully opaque green into a transparent black
-- pixel instead. Any other pixel is unaffected.
greenToAlpha :: PixelRGBA8 -> PixelRGBA8
greenToAlpha (PixelRGBA8 0 255 0 0) = PixelRGBA8 0 0 0 255
greenToAlpha p = p

-- | Assigns the normal window hints.
glfwDefaultHints :: Hexes ()
glfwDefaultHints = Hexes $ liftIO $ do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)

-- | The number of rows and cols that we're working with.
getRowColCount :: Hexes (Int, Int)
getRowColCount = Hexes $ do
    s <- get
    return (rowCount s, colCount s)

-- | The width and height (in Pixels) of a cell within a grid, based on the
-- image data we're using.
getCellWidthHeight :: Hexes (Int, Int)
getCellWidthHeight = Hexes $ do
    s <- get
    let img = tileMap s
    return (imageWidth img `div` imageCols, imageHeight img `div` imageRows)

-- | Initializes a window based on the cell counts and cell sizes, storing the
-- window into the state. The window's context will be bound as the current ogl
-- context, and the viewport will be sized to the window's framebuffer. If we
-- can't create a window the program dies. If we create a window but it's not
-- the size we expect then a warning is printed but we proceed anyway.
initWindow :: Hexes ()
initWindow = do
    (rows,cols) <- getRowColCount
    (cWidth, cHeight) <- getCellWidthHeight
    let fbWidth = cols * cWidth
        fbHeight = rows * cHeight
    mWin <- liftIO $ GLFW.createWindow fbWidth fbHeight "" Nothing Nothing
    window <- liftIO $ case mWin of
        Nothing -> error "FATAL: Could Not Create A Window!" -- FIXME a real exception
        Just window -> return window
    Hexes $ modify (\s -> s {window = window})
    liftIO $ GLFW.makeContextCurrent (Just window)
    (fbx,fby) <- liftIO $ GLFW.getFramebufferSize window
    when ((fbx,fby) /= (fbWidth,fbHeight)) (liftIO $ do
        putStrLn $ "WARNING: Framebuffer obtained was the incorrect size."
        putStrLn $ "WARNING: Requested: " ++ show (fbWidth,fbHeight) ++ ", Actual:" ++ show (fbx,fby))
    glViewport 0 0 (fromIntegral fbx) (fromIntegral fby)
    glClearColor 0.2 0.3 0.3 1.0
    -- TODO: Add a framebuffer size change callback to keep our viewport correct?

-- | Attempts to set the window title to the given string.
setWindowTitle :: String -> Hexes ()
setWindowTitle str = Hexes $ do
    s <- get
    liftIO $ GLFW.setWindowTitle (window s) str

-- | Turns a Quadruple of points of whatever type into the list of points to
-- form the two triangle of that quadruple.
inflateQuadruple :: (a,a,a,a) -> [a]
inflateQuadruple (a,b,c,d) = [a,b,c, a,c,d]

-- | Refreshes the texture location info using buildTextureLocations
regenerateTextureLocations :: Hexes ()
regenerateTextureLocations = do
    (cWidth,cHeight) <- getCellWidthHeight
    let iWidth = cWidth * imageCols
        iHeight = cHeight * imageRows
        newCache = buildTextureLocations iWidth iHeight cWidth cHeight
    Hexes $ modify (\s -> s {textureLocationCache = newCache})

-- | Computes a vector of the texture cell quadruples for all the cells.
buildTextureLocations :: Int -- ^ Image Width
                      -> Int -- ^ Image Height
                      -> Int -- ^ Cell Width
                      -> Int -- ^ Cell Height
                      -> VS.Vector (V2 GLfloat,V2 GLfloat,V2 GLfloat,V2 GLfloat)
buildTextureLocations iWidth iHeight cWidth cHeight = let
    halfWidth = fromIntegral iWidth / 2 :: GLfloat
    halfHeight = fromIntegral iHeight / 2 :: GLfloat
    normX x = (fromIntegral x - halfWidth) / halfWidth
    normY y = (fromIntegral y - halfHeight) / halfHeight
    cacheCount = fromIntegral $ (maxBound :: Word8)
    generationFunc = i2NormPixQuadruple normX normY cWidth cHeight iHeight
    in VS.generate cacheCount generationFunc

-- | Given the appropriate inputs, this computes the quadruple of normalized
-- texture coordinates (aka the "s,t" values) within a texture of the size
-- specified that you would use to form the two triangles of a cell that should
-- display the index given.
i2NormPixQuadruple :: (Int -> GLfloat) -- ^ Func to normalize X coords
                   -> (Int -> GLfloat) -- ^ Func to normalize Y coords
                   -> Int -- ^ Cell width
                   -> Int -- ^ Cell height
                   -> Int -- ^ Screen height
                   -> Int -- ^ The index to compute the coordinates for.
                   -> (V2 GLfloat,V2 GLfloat,V2 GLfloat,V2 GLfloat)
i2NormPixQuadruple normX normY cWidth cHeight iHeight index = let
    (row,col) = index `divMod` 10
    px0 = normX $ cWidth * col
    px1 = normX $ cWidth * (col+1)
    px2 = normX $ cWidth * (col+1)
    px3 = normX $ cWidth * col
    -- NOTE: images have +Y going down, but OGL has +Y going up, so we must
    -- invert the Y value ourselves here.
    py0 = normY $ iHeight - cHeight * row
    py1 = normY $ iHeight - cHeight * row
    py2 = normY $ iHeight - cHeight * (row + 1)
    py3 = normY $ iHeight - cHeight * (row + 1)
    in (V2 px0 py0, V2 px1 py1, V2 px2 py2, V2 px3 py3)
