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

    -- setup our verticies
    let verticies = computeVerticies (rows,cols) (cellWidth,cellHeight)
    let verticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length verticies) * 3
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
                glDrawArrays GL_POINTS 0 (fromIntegral (length verticies) * 3)
                glBindVertexArray 0

                GLFW.swapBuffers window
                -- go again
                loop
    loop

computeVerticies :: (Int, Int) -> (Int, Int) -> [V3 GLfloat]
computeVerticies (rows,cols) (cellWidth,cellHeight) = do
    let screenWidth = fromIntegral $ cols*cellWidth
    let halfWidth = screenWidth / 2
    let screenHeight = fromIntegral $ rows*cellHeight
    let halfHeight = screenHeight / 2
    row <- [0 .. rows]
    col <- [0 .. cols]
    let px = fromIntegral $ (col*cellWidth)
    let pyOriginTop = fromIntegral $ (row*cellHeight)
    let pyOriginBottom = screenHeight - pyOriginTop
    let normpx = (px - halfWidth) / halfWidth
    let normpyB = (pyOriginBottom - halfHeight) / halfHeight
    return $ V3 normpx normpyB 0
