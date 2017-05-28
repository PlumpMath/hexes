{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This is the "internal" module for the Hexes project. If you're not familiar
-- with the concept of internal modules, that means that absolutely everything
-- in the package is exported from here, and then the "clean" version of the API
-- intended for end users is re-exported from the "Hexes" module. Things that
-- are here but __not__ in the public API portion are not intended for general
-- use, and using them could make your program crash or whatever if you don't
-- hold up to the assumed invariants of the system.
--
-- Here be Dragons, etc.
module Hexes.InternalOld where

-- base
import Control.Applicative (liftA2)
import Control.Exception (bracket, assert)
import Control.Monad (when, mapM_)
import Data.Char (ord)
import Data.IORef
import Data.Word (Word8)
import System.Exit (die)
import Foreign
import Foreign.C.String
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
-- linear
import Linear
-- storable-tuple
import Foreign.Storable.Tuple
-- hexes internal sub-modules
import Hexes.Internal.Shader

-- | The number of rows we expect to find in a proper image file. This is a
-- constant 26, but we're naming it for clarity.
imageRows :: Integral i => i
imageRows = 26

-- | The number of columns we expect to find in a proper image file. This is a
-- constant 10, but we're naming it for clarity.
imageCols :: Integral i => i
imageCols = 10

-- | The maximum numeric value that can fix into a cell. This is a constant 255,
-- but we're naming it for clarity.
cellMaxValue :: Integral i => i
cellMaxValue = fromIntegral (maxBound :: Word8)

-- | Private. State tracked by the Hexes computation.
data HexesState = HexesState {
    -- | How many rows we're showing in our screen
    rowCount :: !Int,

    -- | How many columns we're showing in our screen
    colCount :: !Int,

    -- | The image data that we're going to be sending to OGL as a texture. We
    -- will only need to send this to OGL once unless we later decide to allow
    -- the texture to be reloaded.
    tileMap :: Image PixelRGBA8,

    -- | The GLFW window we do all our operations with.
    window :: GLFW.Window,

    -- | A vector that you index into with a cell's screen index, (@row *
    -- colCount + col@), to get the vertex point location data of the cell. If
    -- we ever allow the window to be resized, this must be recalculated upon
    -- resize.
    screenLocationCache :: VS.Vector ScreenQuadruple,

    -- | A vector that you index into with a cell's Word8 value to get the
    -- vertex texture location data of the cell. If we ever allow tile maps to
    -- be reloaded, this must be recalculated upon texture reload.
    textureLocationCache :: VS.Vector TextureQuadruple,

    -- | An IO vector that holds the Word8 contents of the cell grid.
    cellData :: VS.Vector Word8,

    -- TODO: If we store just the seperate data values (screenLocationCache,
    -- textureLocationCache, cellData) and then rebuild all of that to the VBO
    -- each frame that seems like it would be slower than keeping the
    -- mostly-built version and then editing that when we get updates. Keeping a
    -- cache of the data to put out doesn't save us on VBO pushes, but it saves
    -- us on building temporary lists, allocating those into Ptr values, then
    -- cleaning up those Ptr values.

    -- | The shader program. Probably never changes during the program's life.
    theShaderProgram :: ShaderProgram,

    -- | The texture holding our image data. Currently it can't be reloaded or
    -- refreshed in any way within a single Hexes computation.
    theTexture :: TextureID,

    -- | Our Vertex Array Object.
    theVertexArray :: VertexArrayID,

    -- | Our Vertex Buffer Object. We need to re-push this data before a draw
    -- call if any of our cells have changed since the last call. For now we'll
    -- simply re-push /every single/ call, but we might be able to be smarter
    -- about it in the future and avoid some pushes.
    theVertexBuffer :: VertexBufferID
    }

-- | Newtype for a quadruple of texture location data, @(s,t)@ coordinates.
newtype TextureQuadruple = TextureQuadruple (V2 GLfloat,V2 GLfloat,V2 GLfloat,V2 GLfloat)
    deriving (Show, VS.Storable) -- no Eq, these are floats.

-- | Newtype for a quadruple of screen location data, @(x,y)@ coordinates.
newtype ScreenQuadruple = ScreenQuadruple (V2 GLfloat,V2 GLfloat,V2 GLfloat,V2 GLfloat)
    deriving (Show, VS.Storable) -- no Eq, these are floats.

-- | A shader program id for use with 'glUseProgram' and similar.
newtype ShaderProgram = ShaderProgram GLuint
    deriving (Eq, Ord, Show)

-- | A texture ID for use with 'glBindTexture' and similar.
newtype TextureID = TextureID GLuint
    deriving (Eq, Ord, Show)

-- | Vertex Array Object names, from 'glGenVertexArrays' and all that.
newtype VertexArrayID = VertexArrayID GLuint
    deriving (Eq, Ord, Show)

-- | Vertex Buffer Object names, from 'glGenBuffers' and all that.
newtype VertexBufferID = VertexBufferID GLuint
    deriving (Eq, Ord, Show)

-- | Our data per vertex, arranged as we'll put it into our Vertex Buffer
-- Object. The ordering is xy, st, background (rgb), foreground (rgba).
newtype VertexData = VertexData (V2 GLfloat)--,V2 GLfloat,V3 GLfloat,V4 GLfloat)
    deriving (Show, Storable)

-- | Danger! This makes a 'HexesState' value with various 'undefined' entries in
-- it, which you must then fill via the proper configuation steps before you
-- allow it to be used in a general way.
mkStartConfig :: Int -> Int -> Image PixelRGBA8 -> HexesState
mkStartConfig rows cols alphaImage = HexesState {
    rowCount = rows,
    colCount = cols,
    tileMap = alphaImage,
    window = undefined,
    textureLocationCache = undefined,
    screenLocationCache = undefined,
    cellData = undefined,
    theShaderProgram = ShaderProgram 0,
    theTexture = TextureID 0,
    theVertexArray = VertexArrayID 0,
    theVertexBuffer = VertexBufferID 0
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
newtype Hexes a = Hexes (StateT HexesState IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Converts a Hexes computation into an IO computation. You have to provide
-- the number of rows and cols you want within the glyph grid that you work
-- with, and also a ByteString containing the image data that should be used
-- during rendering. A ByteString is accepted so that you can either load the
-- bytes from disk when your program starts, or you can use the 'Data.FileEmbed'
-- module from the 'file-embed' package to bundle the tile data directly into
-- your final binary if you want. Because that's probably want you'll want.
runHexes :: Int -> Int -> ByteString -> Hexes () -> IO ()
runHexes rows cols textureBytes userAction = do
    -- TODO: Have the user decode their bytestring and provide whatever
    -- pre-processing that they want on their end, then they pass in an
    -- ImageRGBA8 that's exactly what they want. Offer up the 'greenToAlpha'
    -- function for general use, but don't make it automatic here.
    let eStrImg = decodeImage textureBytes
    baseImage <- either die (return . convertRGBA8) eStrImg :: IO (Image PixelRGBA8)
    let alphaImage = pixelMap greenToAlpha baseImage
    -- TODO: this should be a separated "hexesSetup" func, probably
    let (Hexes whole) = do
            -- TODO: We should probably install a GLFW Error Handler before
            -- doing anything else with GLFW.
            glfwDefaultHints
            initWindow
            regenerateTextureLocations
            regenerateScreenLocations
            -- Assign an initial grid
            let spaceWord8 = (fromIntegral $ ord 'X') -- TODO: set this back to space instead of X
                newGrid = VS.generate (rows*cols) fromIntegral
            Hexes $ modify (\s -> s {cellData = newGrid})
            -- Dummy keyboard callback for testing
            let callback window key scanCode keyState modKeys = do
                    print key
                    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
                        (GLFW.setWindowShouldClose window True)
            win <- Hexes $ window <$> get
            liftIO $ GLFW.setKeyCallback win (Just callback)
            -- ready our shader program
            eErrProg <- programFromSources vertexShaderSource fragmentShaderSource
            case eErrProg of
                Left e -> liftIO $ putStrLn e
                Right p -> do
                    Hexes $ modify (\s -> s {theShaderProgram = ShaderProgram p})
                    glUseProgram p
            
            -- TODO: stop unwrapping these newtypes (below) and write safer gl
            -- funcs that only accept the correct form of thing.
            
            -- Create our texture object and push the texture data to the GPU
            [(TextureID texID)] <- safeGenTextures 1
            glActiveTexture GL_TEXTURE0
            glBindTexture GL_TEXTURE_2D texID
            let iWidth = fromIntegral $ imageWidth alphaImage
                iHeight = fromIntegral $ imageHeight alphaImage
                iData = imageData alphaImage -- don't fromIntegral here or the solver won't solve
            liftIO $ VS.unsafeWith iData $ \dataP ->
                glTexImage2D GL_TEXTURE_2D 0 GL_RGBA iWidth iHeight 0 GL_RGBA GL_UNSIGNED_BYTE (castPtr dataP)
            glGenerateMipmap GL_TEXTURE_2D
            Hexes $ modify (\s -> s {theTexture = TextureID texID})
            (ShaderProgram p) <- Hexes $ theShaderProgram <$> get
            imageMapLoc <- liftIO $ withCString "imageMap" $ glGetUniformLocation p
            glUniform1i imageMapLoc 0
            -- create our vertex array object
            [(VertexArrayID vao)] <- safeGenVertexArrays 1
            Hexes $ modify (\s -> s {theVertexArray = VertexArrayID vao})
            glBindVertexArray vao
            -- create our vertex buffer object
            [(VertexBufferID vbo)] <- safeGenVertexBuffers 1
            Hexes $ modify (\s -> s {theVertexBuffer = VertexBufferID vao})
            glBindBuffer GL_ARRAY_BUFFER vbo
            -- assign attribute pointer information for our bound VBO
            let strideSize = fromIntegral $ sizeOf (undefined::VertexData)
                offsetPtr x = castPtr $ nullPtr `plusPtr` (sizeOf (0.0::GLfloat) * x)
            glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE strideSize (offsetPtr 0)
            --glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE strideSize (offsetPtr 2)
            --glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE strideSize (offsetPtr 4)
            --glVertexAttribPointer 3 4 GL_FLOAT GL_FALSE strideSize (offsetPtr 7)
            glEnableVertexAttribArray 0
            userAction
            hexesCleanup
    bracketGLFW $ flip evalStateT (mkStartConfig rows cols alphaImage) whole

-- TODO: An action to set a key input callback

-- TODO: An action to write data into a cell

-- TODO: A way to set the colors of a cell

-- | Causes the GLFW window system to poll for events. This is a foreign call,
-- so it's uninterruptable until it completes. However, @pollEvents@ does not
-- directly execute your callbacks while within the foreign call. Instead it
-- only schedules each of your callbacks to be executed within the normal GHC
-- runtime once this call returns.
pollEvents :: Hexes ()
pollEvents = liftIO $ GLFW.pollEvents

-- | Refreshes the screen's display based on the current grid configuration.
refresh :: Hexes ()
refresh = do
    glClear GL_COLOR_BUFFER_BIT
    verticies <- buildData
    let verticiesByteCount = fromIntegral $ sizeOf (undefined::VertexData) * (VS.length verticies)
    --liftIO $ print "first 6"
    --liftIO $ print (VS.take 6 verticies)
    win <- Hexes $ window <$> get
    liftIO $ withArray (VS.toList verticies) $ \verticiesP -> do
        glBufferData GL_ARRAY_BUFFER verticiesByteCount (castPtr verticiesP) GL_DYNAMIC_DRAW
        glDrawArrays GL_TRIANGLES 0 (fromIntegral $ VS.length verticies)
        liftIO $ GLFW.swapBuffers win

buildData :: Hexes (VS.Vector VertexData)
buildData = do
    (rows,cols) <- getRowColCount
    let cellCount = rows*cols
    screenLocations <- Hexes $ screenLocationCache <$> get
    return $ VS.generate (cellCount*6) (\subIndex ->
        let (cell,vertex) = subIndex `divMod` 6
            (ScreenQuadruple sq) = VS.unsafeIndex screenLocations cell
            screenLocList = inflateQuadruple sq
        in VertexData $ screenLocList !! vertex)

-- | Ensures that we only run GLFW code while it's initialized, and also that we
-- always terminate it when we're done. Also, this function should only be used
-- from the main thread.
bracketGLFW :: IO () -> IO ()
bracketGLFW act = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
    when initWorked act

-- | This is planned to clean up any GL associated data so that we can cleanly
-- exit from Hexes.
hexesCleanup :: Hexes ()
hexesCleanup = return () -- TODO: actually do this.

-- | Safely generates the number of texture ID values requested. This just
-- simplifies the process of calling 'glGenTextures' with a properly sized
-- temporary buffer, no magic.
safeGenTextures :: MonadIO m => Int -> m [TextureID]
safeGenTextures n = do
    liftIO $ allocaArray n $ \arrP -> do
        glGenTextures (fromIntegral n) arrP
        (fmap.fmap) TextureID (peekArray n arrP)

-- | Safely generates the number of vertex array ID values requested. This just
-- simplifies the process of calling 'glGenVertexArrays' with a properly sized
-- temporary buffer, no magic.
safeGenVertexArrays :: MonadIO m => Int -> m [VertexArrayID]
safeGenVertexArrays n = do
    liftIO $ allocaArray n $ \arrP -> do
        glGenVertexArrays (fromIntegral n) arrP
        (fmap.fmap) VertexArrayID (peekArray n arrP)

-- | Safely generates the number of vertex buffer ID values requested. This just
-- simplifies the process of calling 'glGenBuffers' with a properly sized
-- temporary buffer, no magic.
safeGenVertexBuffers :: MonadIO m => Int -> m [VertexBufferID]
safeGenVertexBuffers n = do
    liftIO $ allocaArray n $ \arrP -> do
        glGenBuffers (fromIntegral n) arrP
        (fmap.fmap) VertexBufferID (peekArray n arrP)

-- | Converts any pixel that is a fully opaque green into a transparent black
-- pixel instead. Any other kind of pixel is unaffected.
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

-- | The number of rows and cols that we're working with. Has no XY variant
-- since the Row count and Y count are both identical.
getRowColCount :: Hexes (Int, Int)
getRowColCount = Hexes $ liftA2 (,) rowCount colCount <$> get

-- | The width and height (in Pixels) of a cell within a grid, based on the
-- image data we're using and the 'imageCols' and 'imageRows' top level
-- definitions.
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
        -- TODO: Implement a real exception type that we can use with throwIO
        Nothing -> error "FATAL: Could not create a window!"
        Just window -> return window
    Hexes $ modify (\s -> s {window = window})
    liftIO $ GLFW.makeContextCurrent (Just window)
    (fbx,fby) <- liftIO $ GLFW.getFramebufferSize window
    when ((fbx,fby) /= (fbWidth,fbHeight)) (liftIO $ do
        putStrLn $ "WARNING: Framebuffer obtained was the incorrect size."
        putStrLn $ "WARNING: Requested: " ++ show (fbWidth,fbHeight) ++ ", Actual:" ++ show (fbx,fby))
    glViewport 0 0 (fromIntegral fbx) (fromIntegral fby)
    glClearColor 0.2 0.3 0.3 1.0
    -- TODO: Maybe add a framebuffer size change callback to keep our viewport correct?

-- | Assigns the "window should close" value of the Hexes window.
setWindowShouldClose :: Bool -> Hexes ()
setWindowShouldClose b = Hexes $ do
    win <- window <$> get
    liftIO $ GLFW.setWindowShouldClose win b

-- | Obtains the "window should close" value of the Hexes window.
windowShouldClose :: Hexes Bool
windowShouldClose = Hexes $ do
    win <- window <$> get
    liftIO $ GLFW.windowShouldClose win

-- | Attempts to set the window title to the given string.
setWindowTitle :: String -> Hexes ()
setWindowTitle str = Hexes $ do
    win <- window <$> get
    liftIO $ GLFW.setWindowTitle win str

-- | Turns a Quadruple of points of whatever type into the list of points to
-- form the two triangles of that quadruple.
-- 
-- >>> inflateQuadruple (0,1,2,3)
-- [0,1,2,0,2,3]
inflateQuadruple :: (a,a,a,a) -> [a]
inflateQuadruple (a,b,c,d) = [a,b,c, a,c,d]

-- | Refreshes the texture location cache.
regenerateTextureLocations :: Hexes ()
regenerateTextureLocations = do
    (cWidth,cHeight) <- getCellWidthHeight
    -- Remember that imageCols and imageRows are top level definitions.
    let iWidth = cWidth * imageCols
        iHeight = cHeight * imageRows
        newCache = buildTextureLocations iWidth iHeight cWidth cHeight
    Hexes $ modify (\s -> s {textureLocationCache = newCache})

-- | Computes a vector of the texture quadruples for all possible tiles in an
-- image with the specified characteristics. The output quadruples are in terms
-- of OGL @s,t@ [-1,1] texture coordiante space.
buildTextureLocations :: Int -- ^ Image Width
                      -> Int -- ^ Image Height
                      -> Int -- ^ Cell Width
                      -> Int -- ^ Cell Height
                      -> VS.Vector TextureQuadruple
buildTextureLocations iWidth iHeight cWidth cHeight = let
    normX x = (fromIntegral x / fromIntegral iWidth) * 2 - 1
    normY y = (fromIntegral y / fromIntegral iHeight) * 2 - 1
    cacheCount = cellMaxValue
    generationFunc = i2NormPixQuadruple normX normY cWidth cHeight iHeight
    in VS.generate cacheCount generationFunc

-- | Given the appropriate inputs, this computes the bounds for the tile index
-- specified within an image's pixel space (0 to N in each of @x,y@), and then
-- converts that into the quadruple you would use to pick out that portion of
-- the texture within OGL's texture space (-1 to 1 in each of @s,t@).
i2NormPixQuadruple :: (Int -> GLfloat) -- ^ Func to normalize X coords
                   -> (Int -> GLfloat) -- ^ Func to normalize Y coords
                   -> Int -- ^ Cell width
                   -> Int -- ^ Cell height
                   -> Int -- ^ Screen height
                   -> Int -- ^ The index to compute the coordinates for.
                   -> TextureQuadruple
i2NormPixQuadruple normX normY cWidth cHeight iHeight index = let
    (row,col) = index `divMod` imageCols -- imageCols is top level
    px0 = normX $ cWidth * col
    px1 = normX $ cWidth * (col+1)
    px2 = normX $ cWidth * (col+1)
    px3 = normX $ cWidth * col
    -- NOTE: images have +Y going down, but OGL has +Y going up, so we must
    -- invert the Y value here in addition to the normalization.
    py0 = normY $ iHeight - cHeight * row
    py1 = normY $ iHeight - cHeight * row
    py2 = normY $ iHeight - cHeight * (row + 1)
    py3 = normY $ iHeight - cHeight * (row + 1)
    in TextureQuadruple (V2 px0 py0, V2 px1 py1, V2 px2 py2, V2 px3 py3)

-- | Refreshes the screen location cache.
regenerateScreenLocations :: Hexes ()
regenerateScreenLocations = do
    (rowCount,colCount) <- getRowColCount
    (cWidth,cHeight) <- getCellWidthHeight
    let newCache = buildScreenLocations rowCount colCount cWidth cHeight
    Hexes $ modify (\s -> s {screenLocationCache = newCache})

-- | Computes a vector of the location quadruples for all possible cells in a
-- screen space with the specified characteristics. The output quadruples are in
-- terms of OGL's @x,y@ [-1,1] location coordinate space.
buildScreenLocations :: Int -> Int -> Int -> Int -> VS.Vector ScreenQuadruple
buildScreenLocations rowCount colCount cWidth cHeight = let
    screenWidth = rowCount * cHeight
    screenHeight = colCount * cWidth
    normX x = (fromIntegral x / fromIntegral screenWidth) * 2 - 1
    normY y = (fromIntegral y / fromIntegral screenHeight) * 2 - 1
    cacheCount = rowCount * colCount
    generationFunc = i2NormScreenQuadruple normX normY cWidth cHeight screenHeight colCount
    in VS.generate cacheCount generationFunc

-- | Given the appropriate inputs, the computes the bounds for the cell index
-- specified within the pixel's screen space (0 to N in each of @x,y@), and then
-- converts that into the quadruple you would use to pick out of that portion of
-- the screen space within OGL's NDC space (-1 to 1 in each of @x,y@).
i2NormScreenQuadruple :: (Int -> GLfloat) -- ^ Func to normalize X coords
                      -> (Int -> GLfloat) -- ^ Func to normalize Y coords
                      -> Int -- ^ Cell width
                      -> Int -- ^ Cell height
                      -> Int -- ^ Screen Height
                      -> Int -- ^ Column count
                      -> Int -- ^ The index to compute the coordinates for.
                      -> ScreenQuadruple
i2NormScreenQuadruple normX normY cWidth cHeight sHeight colCount index = let
    (row,col) = index `divMod` colCount
    px0 = normX $ cWidth * col
    px1 = normX $ cWidth * (col+1)
    px2 = normX $ cWidth * (col+1)
    px3 = normX $ cWidth * col
    -- NOTE: rows have +Y going down, but OGL has +Y going up, so we must
    -- invert the Y value here in addition to the normalization.
    py0 = normY $ sHeight - cHeight * row
    py1 = normY $ sHeight - cHeight * row
    py2 = normY $ sHeight - cHeight * (row + 1)
    py3 = normY $ sHeight - cHeight * (row + 1)
    in ScreenQuadruple (V2 px0 py0, V2 px1 py1, V2 px2 py2, V2 px3 py3)

-- TODO: i2NormScreenQuadruple and i2NormPixQuadruple are almost 100% identical.
-- Fix this and merge them already.

-- TODO: buildScreenLocations and buildTextureLocations are pretty similar.
-- Consider merging them, though since they're each wrapping the normalized
-- quadruple builder func in a slightly different way perhaps they can stay as
-- they are.

-- | Sets the entire grid to be the value specified converted into a 'Word8'.
setGridToIntegral :: Integral i => i -> Hexes ()
setGridToIntegral i = do
    (rows,cols) <- getRowColCount
    let newGrid = VS.replicate (rows*cols) (fromIntegral i)
    Hexes $ modify (\s -> s {cellData = newGrid})

-- | As per 'setGridToIntegral', but uses the 'ord' value of the character
-- specified as the number.
setGridToChar :: Char -> Hexes ()
setGridToChar c = setGridToIntegral (ord c)
