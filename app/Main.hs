{-# LANGUAGE QuasiQuotes #-}

module Main where

-- base
import Control.Monad (when)
import Foreign
-- GLFW-b
import qualified Graphics.UI.GLFW as GLFW
-- gl
import Graphics.GL.Core33
import Graphics.GL.Types
-- raw-strings-qq
import Text.RawString.QQ
-- linear
import Linear
-- hexes
import Hexes

vertexShaderSource :: String
vertexShaderSource = [r|
    #version 330 core
    layout (location = 0) in vec3 position;
    void main()
    {
        gl_Position = vec4(position.x, position.y, position.z, 1.0);
    }
    |]

fragmentShaderSource :: String
fragmentShaderSource = [r|
    #version 330 core
    out vec4 color;
    void main()
    {
        color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
    }
    |]

main :: IO ()
main = bracketGLFW $ do
    glfwNormalHints
    let rows = 24
    let cols = 80
    let cellWidth = 8
    let cellHeight = 14
    window <- windowOrDie (cols*cellWidth) (rows*cellHeight) "Demo Program"

    -- pressing escape will set the program to close.
    let callback window key scanCode keyState modKeys = when
            (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
                (GLFW.setWindowShouldClose window True)
    GLFW.setKeyCallback window (Just callback)

    -- calibrate the viewport
    (fbx,fby) <- GLFW.getFramebufferSize window
    putStrLn $ "Framebuffer Size: " ++ show (fbx,fby)
    glViewport 0 0 (fromIntegral fbx) (fromIntegral fby)
    glClearColor 0.2 0.3 0.3 1.0
    
    -- ready our shader program
    eErrProg <- programFromSources vertexShaderSource fragmentShaderSource
    prog <- case eErrProg of
        Left e -> putStrLn e >> return 0
        Right p -> return p
    glUseProgram prog

    -- setup our vertex data
    let quadList = computeQuads (rows,cols) (cellWidth,cellHeight)
    let verticies = normalizeQuads (rows,cols) (cellWidth,cellHeight) quadList :: [GLfloat]
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
    let threeFloats = fromIntegral $ sizeOf (0.0::GLfloat) * 3
    glVertexAttribPointer 0 3 GL_FLOAT GL_TRUE threeFloats nullPtr
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

-- | The data to build a Quad, with coordinates in pixel space. Assumes that 0,0
-- is the bottom left. When converting to 3d just give z the same value for all
-- points.
data PixelQuad = PixelQuad {
    quadPoints :: [V2 Int]
    -- Triangles will always use indexes (0,1,2) and (0,2,3)
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
                        V2 px3 py3]

-- | Converts PixelQuad data into the list of six vertexes needed to form the
-- two triangles, normalizes the coordinates to fit into the screen space, and
-- concats it all together.
normalizeQuads :: (Int, Int) -> (Int, Int) -> [PixelQuad] -> [GLfloat]
normalizeQuads (rows,cols) (cellWidth,cellHeight) quads = let
    screenWidth = cols*cellWidth
    screenHeight = rows*cellHeight
    halfWidth = fromIntegral screenWidth / 2 :: GLfloat
    halfHeight = fromIntegral screenHeight / 2 :: GLfloat
    normalX x = (fromIntegral x - halfWidth) / halfWidth
    normalY y = (fromIntegral y - halfHeight) / halfHeight
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
