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

-- | Quad data for a single cell in the grid, with coordinates in pixel space.
-- Assumes that 0,0 is the bottom left.
data PixelQuad = PixelQuad {
    -- Triangles will always use indexes (0,1,2) and (0,2,3)
    pixelPoints :: [V2 Int],
    pixelRowCol :: (Int,Int)
    } deriving Show

-- | This doesn't give fully compact results, but it is good enough for now, and
-- we won't have so many verticies that it'll make a huge difference that we
-- compact it down anyway. Even though 24row*80col would be 2015 vertexes if
-- fully compacted, this method is still only 7680 for the same size, which is
-- so small that the GPU won't care.
computeQuads :: (Int, Int) -> (Int, Int) -> [PixelQuad]
computeQuads (rows,cols) (cellWidth,cellHeight) = do
    let screenWidth = cols*cellWidth
    let screenHeight = rows*cellHeight
    row <- [0 .. (rows-1)]
    col <- [0 .. (cols-1)]
    let px0 = cellWidth * col
    let px1 = cellWidth * (col+1)
    let px2 = cellWidth * (col+1)
    let px3 = cellWidth * col
    let py0 = screenHeight - cellHeight * row
    let py1 = screenHeight - cellHeight * row
    let py2 = screenHeight - cellHeight * (row + 1)
    let py3 = screenHeight - cellHeight * (row + 1)
    return $ PixelQuad [V2 px0 py0,
                        V2 px1 py1,
                        V2 px2 py2,
                        V2 px3 py3] (row,col)

-- | Quad data for a single cell in the grid, with coordinates in NDC space.
-- Assumes that 0,0 is the center of the screen, and ranges +/- 1.
data NDCQuad = NDCQuad {
    -- Triangles will always use indexes (0,1,2) and (0,2,3)
    ndcPoints :: [V2 GLfloat],
    ndcRowCol :: (Int,Int)
    }

toNDC :: Int -> Int -> PixelQuad -> NDCQuad
toNDC viewportWidth viewportHeight (PixelQuad
                                    [(V2 ax ay),
                                    (V2 bx by),
                                    (V2 cx cy),
                                    (V2 dx dy)]
                                    (row,col)) = let
    halfWidth = fromIntegral viewportWidth / 2 :: GLfloat
    halfHeight = fromIntegral viewportHeight / 2 :: GLfloat
    normalX x = (fromIntegral x - halfWidth) / halfWidth
    normalY y = (fromIntegral y - halfHeight) / halfHeight
    in (NDCQuad
        [(V2 (normalX ax) (normalY ay)),
        (V2 (normalX bx) (normalY by)),
        (V2 (normalX cx) (normalY cy)),
        (V2 (normalX dx) (normalY dy))]
        (row,col))

ndcToAllVerts :: (Map (Int,Int) Char) -> (Char -> V2 GLfloat) -> NDCQuad -> [V2 GLfloat]
ndcToAllVerts charData texCoords (NDCQuad [(V2 ax ay),
                        (V2 bx by),
                        (V2 cx cy),
                        (V2 dx dy)] (row,col)) = let
    maybeChar = M.lookup (row,col) charData
    charOrd = maybe 0 ord maybeChar
    in [
    V2 ax ay,
    V2 bx by,
    V2 cx cy,
    V2 ax ay,
    V2 cx cy,
    V2 dx dy]

-- | Converts PixelQuad data into the list of six vertexes needed to form the
-- two triangles, normalizes the coordinates to fit into the screen space, and
-- concats it all together.
normalizeQuads :: (Int, Int) -> (Int, Int) -> [PixelQuad] -> [GLfloat]
normalizeQuads (rows,cols) (cellWidth,cellHeight) quads = let
    screenWidth = cols*cellWidth
    screenHeight = rows*cellHeight
    in concatMap (\(PixelQuad [(V2 ax ay),
                               (V2 bx by),
                               (V2 cx cy),
                               (V2 dx dy)]) -> [
                                   normalX ax, normalY ay, 0,
                                   normalX bx, normalY by, 0,
                                   normalX cx, normalY cy, 0,
                                   normalX ax, normalY ay, 0,
                                   normalX cx, normalY cy, 0,
                                   normalX dx, normalY dy, 0
            ]) quads
-}