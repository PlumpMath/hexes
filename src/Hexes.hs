
module Hexes (
    Hexes(),
    runHexes,
    getRowColCount,
    getCellWidthHeight,
    setWindowTitle
    ) where

import Hexes.Internal

{-


vertexShaderSource :: String
vertexShaderSource = [r|
    #version 330 core
    layout (location = 0) in vec2 position;
    void main()
    {
        gl_Position = vec4(position.x, position.y, 1.0, 1.0);
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
    
-- | Makes a GLFW Window... or dies trying. Also sets it to have the current
-- OpenGL context and adjusts the viewport to use the entire window space.
--
-- The width and height are the desired width and height of the OGL framebuffer.
-- The window itself will be slightly larger than that.
windowOrDie :: Int -> Int -> String -> IO GLFW.Window
windowOrDie width height title = do
    maybeWindow <- GLFW.createWindow width height title Nothing Nothing
    case maybeWindow of
        Nothing -> die "Could not create a window!"
        Just window -> do
            GLFW.makeContextCurrent (Just window)
            (fbx,fby) <- GLFW.getFramebufferSize window
            if (fbx,fby) /= (width,height)
                then die "Framebuffer obtained was the incorrect size"
                else glViewport 0 0 (fromIntegral fbx) (fromIntegral fby)
            return window

-- | Given a shader type and a shader source, it gives you (Right id) of the
-- successfully compiled shader, or (Left err) with the error message. In the
-- error case, the shader id is deleted before the function returns to avoid
-- accidentally leaking shader objects.
loadShader :: MonadIO m => GLenum -> String -> ExceptT String m GLuint
loadShader shaderType source = do
    -- new shader object
    shaderID <- glCreateShader shaderType
    -- assign the source to the shader object
    liftIO $ withCAStringLen source $ \(strP, strLen) ->
        withArray [strP] $ \linesPtrsPtr ->
            withArray [fromIntegral strLen] $ \lengthsPtr ->
                glShaderSource shaderID 1 linesPtrsPtr lengthsPtr
    -- compile and check success
    glCompileShader shaderID
    success <- liftIO $ alloca $ \successP -> do
        glGetShaderiv shaderID GL_COMPILE_STATUS successP
        peek successP
    if success == GL_TRUE
        -- success: we're done
        then return shaderID
        -- failure: we get the log, delete the shader, and return the log.
        else do
            -- how many bytes the info log should be (including the '\0')
            logLen <- liftIO $ alloca $ \logLenP -> do
                glGetShaderiv shaderID GL_INFO_LOG_LENGTH logLenP
                peek logLenP
            -- space for the info log
            logBytes <- liftIO $ allocaBytes (fromIntegral logLen) $ \logP -> do
                -- space for the log reading result
                alloca $ \resultP -> do
                    -- Try to obtain the log bytes
                    glGetShaderInfoLog shaderID logLen resultP logP
                    -- this is how many bytes we actually got
                    result <- fromIntegral <$> peek resultP
                    peekArray result logP
            -- delete the shader object and return the log
            glDeleteShader shaderID
            let prefix = case shaderType of
                    GL_VERTEX_SHADER -> "Vertex"
                    GL_GEOMETRY_SHADER -> "Geometry"
                    GL_FRAGMENT_SHADER -> "Fragment"
                    _ -> "Unknown Type"
            throwE $ prefix ++ " Shader Error:" ++ map (toEnum.fromEnum) logBytes

-- | Given a vertex shader object and a fragment shader object, this will link
-- them into a new program, giving you (Right id). If there's a linking error
-- the error log is retrieved, the program deleted, and (Left err) is returned.
linkProgram :: MonadIO m => GLuint -> GLuint -> ExceptT String m GLuint
linkProgram vertexID fragmentID = do
    programID <- glCreateProgram
    glAttachShader programID vertexID
    glAttachShader programID fragmentID
    glLinkProgram programID
    success <- liftIO $ alloca $ \successP -> do
        glGetProgramiv programID GL_LINK_STATUS successP
        peek successP
    if success == GL_TRUE
        -- success: we're done
        then return programID
        -- failure: we get the log, delete the shader, and return the log.
        else do
            -- how many bytes the info log should be (including the '\0')
            logLen <- liftIO $ alloca $ \logLenP -> do
                glGetProgramiv programID GL_INFO_LOG_LENGTH logLenP
                peek logLenP
            -- space for the info log
            logBytes <- liftIO $ allocaBytes (fromIntegral logLen) $ \logP -> do
                -- space for the log reading result
                alloca $ \resultP -> do
                    -- Try to obtain the log bytes
                    glGetProgramInfoLog programID logLen resultP logP
                    -- this is how many bytes we actually got
                    result <- fromIntegral <$> peek resultP
                    peekArray result logP
            -- delete the program object and return the log
            glDeleteProgram programID
            throwE $ "Program Link Error: " ++ map (toEnum.fromEnum) logBytes

-- | Given the source for the vertex shader and the fragment shader, compiles
-- both and links them into a single program. If all of that is successful, the
-- intermediate shaders are deleted before the final value is returned.
programFromSources :: MonadIO m => String -> String -> m (Either String GLuint)
programFromSources vertexSource fragmentSource = liftIO $ runExceptT $ do
    v <- loadShader GL_VERTEX_SHADER vertexSource
    f <- loadShader GL_FRAGMENT_SHADER fragmentSource
    p <- linkProgram v f
    glDeleteShader v
    glDeleteShader f
    return p

-}
