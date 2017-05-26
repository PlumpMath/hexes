{-# LANGUAGE QuasiQuotes #-}

module Main where

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- transformers
import Control.Monad.IO.Class

-- hexes
import Hexes

main = do
    bytes <- B.readFile "font-data/FixedSysExcelsior.png"
    runHexes 24 80 bytes $ do
        liftIO $ putStrLn "Hello From Hexes!"

{-
main :: IO ()
main = bracketGLFW $ do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    let rows = 24
    let cols = 80
    let cellWidth = 8 -- this should be figured from the texture loaded
    let cellHeight = 16 -- this should be figured from the texture loaded
    let viewportWidth = (cols*cellWidth)
    let viewportHeight = (rows*cellHeight)
    window <- windowOrDie viewportWidth viewportHeight "Demo Program"

    -- pressing escape will set the program to close.
    let callback window key scanCode keyState modKeys = when
            (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
                (GLFW.setWindowShouldClose window True)
    GLFW.setKeyCallback window (Just callback)

    -- Set our clear color.
    glClearColor 0.2 0.3 0.3 1.0
    
    -- ready our shader program
    eErrProg <- programFromSources vertexShaderSource fragmentShaderSource
    prog <- case eErrProg of
        Left e -> putStrLn e >> return 0
        Right p -> return p
    glUseProgram prog

    -- ready our texture
    eErrDI <- readImage "font-data/FixedSysExcelsior.png"
    dyImage <- case eErrDI of
        Left e -> do
            putStrLn e
            return $ ImageRGB8 $ generateImage (\x y ->
                let x' = fromIntegral x in PixelRGB8 x' x' x') (cellWidth*cols) (cellHeight*rows)
        Right di -> return di
    let ipixelrgb8 = convertRGB8 dyImage
        iWidth = fromIntegral $ imageWidth ipixelrgb8
        iHeight = fromIntegral $ imageHeight ipixelrgb8
        iData = imageData ipixelrgb8
    textureP <- malloc
    glGenTextures 1 textureP
    texture <- peek textureP
    --glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D texture
    VS.unsafeWith iData $ \dataP ->
        glTexImage2D GL_TEXTURE_2D 0 GL_RGB iWidth iHeight 0 GL_RGB GL_UNSIGNED_BYTE (castPtr dataP)
    glGenerateMipmap GL_TEXTURE_2D
    glBindTexture GL_TEXTURE_2D 0

    -- some characters
    let charData = M.empty

    -- Turns a char into the 4 texture locations (s,t) of that character.
    -- Textures should always be 10 cols * 13 rows.
    let texQuad :: Char -> [V2 GLfloat]
        texQuad char = let
            charOrd = mod (ord char) 130
            halfTW = fromIntegral (cellWidth * 10) / 2
            halfTH = fromIntegral (cellHeight * 13) / 2
            (charRow,charCol) = charOrd `divMod` 10
            in undefined

    -- setup our vertex data
    let pixelQuads = computeQuads (rows,cols) (cellWidth,cellHeight)
    let ndcQuads = map (toNDC viewportWidth viewportHeight) pixelQuads
    let v2s = concatMap (ndcToAllVerts charData undefined) ndcQuads
    let verticies = concatMap (\(V2 x y) -> [x,y]) v2s :: [GLfloat]
    let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length verticies)
    verticesP <- newArray verticies

    -- setup a vertex array object
    vaoP <- malloc
    glGenVertexArrays 1 vaoP
    vao <- peek vaoP
    glBindVertexArray vao
    
    -- setup a vertex buffer object and send it data
    vboP <- malloc
    glGenBuffers 1 vboP
    vbo <- peek vboP
    glBindBuffer GL_ARRAY_BUFFER vbo
    glBufferData GL_ARRAY_BUFFER verticesSize (castPtr verticesP) GL_STATIC_DRAW

    -- assign the attribute pointer information
    let twoFloats = fromIntegral $ sizeOf (0.0::GLfloat) * 2
    glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE twoFloats nullPtr
    glEnableVertexAttribArray 0

    -- unbind our vertex array object to prevent accidental changes in
    -- between our draw calls.
    glBindVertexArray 0

    let loop = do
            shouldContinue <- not <$> GLFW.windowShouldClose window
            when shouldContinue $ do
                -- event poll
                GLFW.pollEvents
                -- draw
                glClear GL_COLOR_BUFFER_BIT

                glBindVertexArray vao
                glDrawArrays GL_TRIANGLES 0 (fromIntegral $ length verticies)
                glBindVertexArray 0

                GLFW.swapBuffers window
                -- go again
                loop
    loop

-}