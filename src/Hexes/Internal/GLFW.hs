
-- | The module for hexes things that are mostly GLFW related.
module Hexes.Internal.GLFW where

import Hexes.Internal.Types

-- base
import Control.Exception (bracket)
import Control.Monad (when)
-- GLFW-b
import qualified Graphics.UI.GLFW as GLFW
-- gl
import Graphics.GL.Core33
import Graphics.GL.Types
-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.State

-- | Ensures that we only run GLFW code while it's initialized, and also that we
-- always terminate it when we're done. Also, this function should only be used
-- from the main thread.
bracketGLFW :: IO () -> IO ()
bracketGLFW act = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
    when initWorked act
    -- TODO: this should be IO a -> IO (Maybe a), possibly as MonadIO as well.

-- | Assigns the normal window hints that we want to use: Core OpenGL 3.3, with
-- a window that is not resizable by the user.
glfwDefaultHints :: Hexes ()
glfwDefaultHints = Hexes $ liftIO $ do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)

-- | Obtains the "window should close" value of the Hexes window.
windowShouldClose :: Hexes Bool
windowShouldClose = do
    win <- getWindow
    liftIO $ GLFW.windowShouldClose win

-- | Causes the GLFW window system to poll for events. This is a foreign call,
-- so it's uninterruptable until it completes. However, @pollEvents@ does not
-- directly execute your callbacks while within the foreign call. Instead it
-- only schedules each of your callbacks to be executed within the normal GHC
-- runtime once this call returns.
pollEvents :: Hexes ()
pollEvents = liftIO $ GLFW.pollEvents

-- TODO: Offer waitEvents and postEmptyEvent as well.

-- | Swaps the foreground and background framebuffer.
swapBuffers :: Hexes ()
swapBuffers = do
    win <- getWindow
    liftIO $ GLFW.swapBuffers win

-- | This sets that the window should close when Escape is pressed, and does
-- nothing otherwise. You can use it as a sort of "default" key callback to make
-- your program easy to quit, if you like.
--
-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
escapeCallback :: GLFW.KeyCallback
escapeCallback window key scanCode keyState modKeys = do
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)

-- | Creates our GLFW Window. If successful, also makes its context the current
-- context and adjusts the 'glViewport' to match the window's framebuffer size.
createWindow :: Hexes Bool
createWindow = do
    (rows,cols) <- getRowColCount
    (cWidth, cHeight) <- getCellWidthHeight
    let fbWidth = cols*cWidth
        fbHeight = rows*cHeight
    glfwDefaultHints
    maybeWindow <- liftIO $ GLFW.createWindow fbWidth fbHeight "Hexes" Nothing Nothing
    case maybeWindow of
        Nothing -> return False
        Just win -> do
            setWindow win
            liftIO $ GLFW.makeContextCurrent (Just win)
            (fbx,fby) <- liftIO $ GLFW.getFramebufferSize win
            when ((fbx,fby) /= (fbWidth,fbHeight)) (liftIO $ do
                putStrLn $ "WARNING: Framebuffer obtained was the incorrect size."
                putStrLn $ "WARNING: Requested: " ++ show (fbWidth,fbHeight) ++ ", Actual:" ++ show (fbx,fby))
            glViewport 0 0 (fromIntegral fbx) (fromIntegral fby)
            -- TODO: Don't set this auto-callback later on.
            liftIO $ GLFW.setKeyCallback win (Just escapeCallback)
            return True

-- | Obtains GLFW's timer value. This is in seconds, and is the amount of time
-- since GLFW was initialized. This can be safely used from any thread.
getTime :: Hexes (Maybe Double)
getTime = Hexes $ liftIO $ GLFW.getTime
