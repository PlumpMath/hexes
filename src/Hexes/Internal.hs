
-- | This is the "internal" module for the Hexes project. If you're not familiar
-- with the concept of internal modules, that means that absolutely everything
-- in the package is exported from here, and then the "clean" version of the API
-- intended for end users is re-exported from the "Hexes" module. Things that
-- are here but __not__ in the public API portion are not intended for general
-- use, and using them could make your program crash or whatever if you don't
-- hold up to the assumed invariants of the system.
--
-- Here be Dragons, etc.
module Hexes.Internal where

import Hexes.Internal.Types
import Hexes.Internal.GLFW
import Hexes.Internal.Shader

-- base
import Control.Concurrent.MVar
import Data.Char (ord)
import Foreign
import Foreign.C.String
-- GLFW-b
import qualified Graphics.UI.GLFW as GLFW
-- gl
import Graphics.GL.Core33
import Graphics.GL.Types
-- JuicyPixels
import Codec.Picture
-- vector
import qualified Data.Vector.Storable as VS
-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
-- Linear
import Linear

-- | You specify the number of rows, number of cols, the Image to use as the
-- tilemap, and a Hexes action, and this will turn it into an IO action. This
-- uses opengl, and so it should only be called from the main thread. Also, you
-- should not use liftIO to call runHexes inside of a Hexes block. That would
-- probably end in a bad time.
runHexes :: Int -> Int -> Image PixelRGBA8 -> Hexes () -> IO ()
runHexes rows cols img userAction = bracketGLFW $ do
    GLFW.setErrorCallback (Just $ \error msg -> print error >> putStrLn msg)
    hexesStateMVar <- newMVar (mkState rows cols img)
    flip runReaderT hexesStateMVar $ unwrapHexes $ do
        -- If it's not entirely clear: this is a Hexes do-block, and also GLFW
        -- is safely enabled during this block.

        -- Creates our window and stores it into the HexesState. This also
        -- initializes our gl context and glViewport to match the window. The
        -- Bool we get back is if that was all successful or not, but right now
        -- there's really no error handling.
        _success <- createWindow

        -- set clear color
        glClearColor 0.2 0.3 0.3 1.0

        -- arrange shader program
        eErrP <- liftIO $ programFromSources vertexShaderSource fragmentShaderSource
        shaderProgram <- liftIO $ case eErrP of
            Left e -> putStrLn e >> return 0
            Right p -> return p
        setShaderProgram shaderProgram
        glUseProgram shaderProgram

        -- Setup verticies
        (cWidth, cHeight) <- getCellWidthHeight
        let cellIndexes = [0..(rows*cols)-1]
            bangOrd = (fromIntegral $ ord 'a')
            builder = \i -> mkCellPair cWidth cHeight cols
                (fromIntegral i) (V3 0.5 0.5 0.5) (V4 1 1 1 1) i
        setVerticies $ VS.fromList $ map builder cellIndexes
        
        -- vertex array object
        [vao] <- safeGenVertexArrays 1
        glBindVertexArray vao
        setTheVAO vao
        
        -- vertex buffer object
        [vbo] <- safeGenBuffers 1
        glBindBuffer GL_ARRAY_BUFFER vbo
        setTheVBO vbo

        -- Assign the attributes of the vertex data we're using
        let stride = fromIntegral $ sizeOf (0.0::GLfloat) * 11
            offset x = castPtr $ nullPtr `plusPtr` (sizeOf (0.0::GLfloat) * x)
        glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE stride nullPtr
        glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE stride (offset 2)
        glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE stride (offset 4)
        glVertexAttribPointer 3 4 GL_FLOAT GL_FALSE stride (offset 7)
        glEnableVertexAttribArray 0
        glEnableVertexAttribArray 1
        glEnableVertexAttribArray 2
        glEnableVertexAttribArray 3

        -- assign our resolution uniform
        windowResolutionLoc <- liftIO $ withCString "windowResolution" $
                glGetUniformLocation shaderProgram
        let fbWidth = fromIntegral $ cols*cWidth
            fbHeight = fromIntegral $ rows*cHeight
        glUniform2f windowResolutionLoc fbWidth fbHeight

        -- assign out tilemap uniform
        [tilemapTexture] <- safeGenTextures 1
        glActiveTexture GL_TEXTURE0
        glBindTexture GL_TEXTURE_2D tilemapTexture
        -- we should never hit the border anyway... we hope.
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER
        -- this makes things blocky instead of blurry
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
        let iWidth = fromIntegral $ imageWidth img
            iHeight = fromIntegral $ imageHeight img
            iData = imageData img
        liftIO $ VS.unsafeWith iData $ \dataP ->
            glTexImage2D GL_TEXTURE_2D 0 GL_RGBA iWidth iHeight 0 GL_RGBA GL_UNSIGNED_BYTE (castPtr dataP)
        glGenerateMipmap GL_TEXTURE_2D
        tilemapLoc <- liftIO $ withCString "tilemap" $ glGetUniformLocation shaderProgram
        glUniform1i tilemapLoc 0

        -- assign our tilePixels uniform
        tilemapResolutionLoc <- liftIO $ withCString "tilemapResolution" $
                glGetUniformLocation shaderProgram
        glUniform2f tilemapResolutionLoc (fromIntegral iWidth) (fromIntegral iHeight)

        -- Set to not using a vertex array until we're ready again.
        --glBindVertexArray 0
        
        -- Finally, we are ready to enter the user's action.
        userAction
        -- TODO: Perform a cleanup after the user's action before we exit Hexes
        -- so that we don't leak any resources and we could safely perform as
        -- many Hexes actions within a program as we like.

-- | Refreshes the window with the current state of the Hexes computation. This
-- uses opengl, and so it should only be called from the main thread.
refresh :: Hexes ()
refresh = do
    -- clear any old data
    glClear GL_COLOR_BUFFER_BIT

    -- get out our stuff
    verticies <- getVerticies
    --vao <- getTheVAO

    -- Bind our VAO
    --glBindVertexArray vao

    -- push the current vertex data.
    liftIO $ VS.unsafeWith verticies $ \verticiesP ->
        let verticiesBytes = fromIntegral $ vLen * sizeOf(undefined::CellPair)
            vLen = VS.length verticies
        in glBufferData GL_ARRAY_BUFFER verticiesBytes (castPtr verticiesP) GL_DYNAMIC_DRAW

    -- Draw the elements. 66 = 11 GLfloats per vertex, * 3 verts per triangle *
    -- 2 triangles per cellpair
    glDrawArrays GL_TRIANGLES 0 126720
    
    -- Clear the VAO selection for paranoia purposes.
    --glBindVertexArray 0

    -- End by swapping the buffers.
    swapBuffers

setGridBackground :: V3 GLfloat -> Hexes ()
setGridBackground bg = do
    v <- getVerticies
    setVerticies (VS.map (setCellPairBackground bg) v)

setGridForeground :: V4 GLfloat -> Hexes ()
setGridForeground fg = do
    v <- getVerticies
    setVerticies (VS.map (setCellPairForeground fg) v)

-- TODO: The following should maybe (?) be collapsed into a single "work"
-- function that takes the gen operation as a paramater in three different
-- aliases, since they're so similar. Also, they can all trivially be adjusted
-- to (Integral i), so that should be considered.

-- | Generates a buffer and automatically handles the pointer fiddling.
safeGenBuffers :: MonadIO m => Int -> m [GLuint]
safeGenBuffers n = do
    liftIO $ allocaArray n $ \arrP -> do
        glGenBuffers (fromIntegral n) arrP
        (peekArray n arrP)

-- | Generates a vertex array and automatically handles the pointer fiddling.
safeGenVertexArrays :: MonadIO m => Int -> m [GLuint]
safeGenVertexArrays n = do
    liftIO $ allocaArray n $ \arrP -> do
        glGenVertexArrays (fromIntegral n) arrP
        (peekArray n arrP)

-- | Generates a texture object and automatically handles the pointer fiddling.
safeGenTextures :: MonadIO m => Int -> m [GLuint]
safeGenTextures n = do
    liftIO $ allocaArray n $ \arrP -> do
        glGenTextures (fromIntegral n) arrP
        (peekArray n arrP)
