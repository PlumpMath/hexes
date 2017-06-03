{-# LANGUAGE QuasiQuotes #-}

-- | This is its own submodule primarily because it uses QuasiQuotes and the
-- rest of the program doesn't need it. However, it's also nice to simply
-- separate out our shader handling concerns.
module Hexes.Internal.Shader where

-- gl
import Graphics.GL.Core33
import Graphics.GL.Types
-- foreign
import Foreign
import Foreign.C.String
-- transformers
import Control.Monad.IO.Class
-- raw-strings-qq
import Text.RawString.QQ

-- | Our vertex shader
vertexShaderSource :: String
vertexShaderSource = [r|
    #version 330 core
    layout (location = 0) in vec2 position;
    layout (location = 1) in vec2 texST;
    layout (location = 2) in vec3 background;
    layout (location = 3) in vec4 foreground;

    uniform vec2 windowResolution;

    out vec2 fragmentTexST;
    out vec3 fragmentBackground;
    out vec4 fragmentForeground;

    void main()
    {
        // Assign our position.
        float normX = (position.x / windowResolution.x)*2.0 - 1.0;
        float normY = ((windowResolution.y - position.y) / windowResolution.y)*2.0 - 1.0;
        gl_Position = vec4(normX, normY, 1.0, 1.0);

        // Pass along the extra info.
        fragmentTexST = texST;
        fragmentBackground = background;
        fragmentForeground = foreground;
    }
    |]

-- | This fragment shader makes all fragments be a fixed color.
fragmentShaderSource :: String
fragmentShaderSource = [r|
    #version 330 core
    in vec2 fragmentTexST;
    in vec3 fragmentBackground;
    in vec4 fragmentForeground;

    uniform sampler2D tilemap;
    uniform vec2 tilemapResolution;

    out vec4 color;

    void main()
    {
        // Convert our background color up into vec4.
        vec4 background = vec4(fragmentBackground, 1.0);

        // Figure our texture's contribution.
        float normS = fragmentTexST.s / tilemapResolution.x;
        float normT = fragmentTexST.t / tilemapResolution.y;
        vec2 tileNormalized = vec2(normS, normT);
        vec4 tilemapColor = texture(tilemap, tileNormalized);

        // here mix the tilemapColor and the foreground.
        vec4 overlay = mix(tilemapColor, fragmentForeground, fragmentForeground.a);

        // Then apply what we get over top of the background.
        vec4 final = mix(background, overlay, tilemapColor.a);

        color = final;
    }
    |]

-- | Given a shader type and a shader source, it gives you (Right id) of the
-- successfully compiled shader, or (Left err) with the error message. In the
-- error case, the shader id is deleted before the function returns to avoid
-- accidentally leaking shader objects.
loadShader :: MonadIO m => GLenum -> String -> m (Either String GLuint)
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
        then return $ Right $ shaderID
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
            return $ Left $ prefix ++ " Shader Error:" ++ map (toEnum.fromEnum) logBytes

-- | Given a vertex shader object and a fragment shader object, this will link
-- them into a new program, giving you (Right id). If there's a linking error
-- the error log is retrieved, the program deleted, and (Left err) is returned.
linkProgram :: MonadIO m => GLuint -> GLuint -> m (Either String GLuint)
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
        then return $ Right programID
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
            return $ Left $ "Program Link Error: " ++ map (toEnum.fromEnum) logBytes

-- | Given the source for the vertex shader and the fragment shader, compiles
-- both and links them into a single program. If all of that is successful, the
-- intermediate shaders are deleted before the final value is returned.
programFromSources :: MonadIO m => String -> String -> m (Either String GLuint)
programFromSources vertexSource fragmentSource = liftIO $ do
    eitherVertShader <- loadShader GL_VERTEX_SHADER vertexSource
    case eitherVertShader of
        Left e -> return $ Left e
        Right vertShader -> do
            eitherFragShader <- loadShader GL_FRAGMENT_SHADER fragmentSource
            case eitherFragShader of
                Left e -> do
                    glDeleteShader vertShader
                    return $ Left e
                Right fragShader -> do
                    eitherProgram <- linkProgram vertShader fragShader
                    glDeleteShader vertShader
                    glDeleteShader fragShader
                    return $ eitherProgram
