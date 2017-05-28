
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

-- TODO is "would be nice if", FIXME is "this won't turn on unless"

import Hexes.Internal.Types
import Hexes.Internal.GLFW
import Hexes.Internal.Shader

-- base
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
import Control.Monad.Trans.State

-- | You specify the number of rows, number of cols, the Image to use as the
-- tilemap, and a Hexes action, and this will turn it into an IO action.
runHexes :: Int -> Int -> Image PixelRGBA8 -> Hexes () -> IO ()
runHexes rows cols img userAction = bracketGLFW $
    flip evalStateT (mkState rows cols img) $ unwrapHexes $ do
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
        let cellIndexes = [0..(rows*cols)-1]
        (cWidth, cHeight) <- getCellWidthHeight
        setVerticies $ concatMap (\i ->
            let (row,col) = i `divMod` cols
            -- TODO: We should wrap this all up so that our verticies list is
            -- just a [VertexData] or some such blob that's easier to deal with.
            in map fromIntegral [
                -- X coordinate   Y coordinate         S  T   BG:RGB FG:RGBA
                cWidth * col,     cHeight * row,       24,48, 1,0,0, 0,1,0,1, -- a
                cWidth * (col+1), cHeight * row,       32,48, 1,0,0, 0,1,0,1, -- b
                cWidth * (col+1), cHeight * (row + 1), 32,64, 1,0,0, 0,1,0,1, -- c
                --
                cWidth * (col+1), cHeight * (row + 1), 32,64, 0,1,1, 0,0,0,0, -- c
                cWidth * col,     cHeight * (row + 1), 24,64, 0,1,1, 0,0,0,0, -- d
                cWidth * col,     cHeight * row,       24,48, 0,1,1, 0,0,0,0  -- a
            ]) cellIndexes
        
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
        glBindVertexArray 0
        
        -- Finally, we are ready to enter the user's action.
        userAction
        -- TODO: Perform a cleanup after the user's action before we exit Hexes
        -- so that we don't leak any resources and we could safely perform as
        -- many Hexes actions within a program as we like.

-- | Refreshes the window with the current state of the Hexes computation.
refresh :: Hexes ()
refresh = do
    -- clear any old data
    glClear GL_COLOR_BUFFER_BIT

    -- get out our stuff
    verticies <- getVerticies
    vao <- getTheVAO
    vbo <- getTheVBO

    -- Bind our VAO
    glBindVertexArray vao

    -- push the current vertex data.
    liftIO $ withArrayLen verticies $ \vLen verticiesP ->
        let verticiesBytes = fromIntegral $ vLen * sizeOf(0.0::GLfloat)
        in glBufferData GL_ARRAY_BUFFER verticiesBytes (castPtr verticiesP) GL_DYNAMIC_DRAW

    -- Draw the elements
    glDrawArrays GL_TRIANGLES 0 (fromIntegral $ length verticies)
    
    -- Clear the VAO selection for paranoia purposes.
    glBindVertexArray 0

    -- End by swapping the buffers.
    swapBuffers

safeGenBuffers :: MonadIO m => Int -> m [GLuint]
safeGenBuffers n = do
    liftIO $ allocaArray n $ \arrP -> do
        glGenBuffers (fromIntegral n) arrP
        (peekArray n arrP)

safeGenVertexArrays :: MonadIO m => Int -> m [GLuint]
safeGenVertexArrays n = do
    liftIO $ allocaArray n $ \arrP -> do
        glGenVertexArrays (fromIntegral n) arrP
        (peekArray n arrP)

safeGenTextures :: MonadIO m => Int -> m [GLuint]
safeGenTextures n = do
    liftIO $ allocaArray n $ \arrP -> do
        glGenTextures (fromIntegral n) arrP
        (peekArray n arrP)
