
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
import Control.Monad.Trans.Reader

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
    -- This line seems to help the library run on OSX, somehow.
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)

-- | Obtains the "window should close" value of the Hexes window.
--
-- __Thread Safety:__ Any thread, but access is not synchronized.
windowShouldClose :: Hexes Bool
windowShouldClose = do
    win <- getWindow
    liftIO $ GLFW.windowShouldClose win

-- | Assigns the "window should close" value of the Hexes window.
--
-- __Thread Safety:__ Any thread, but access is not synchronized.
setWindowShouldClose :: Bool -> Hexes ()
setWindowShouldClose b = do
    win <- getWindow
    liftIO $ GLFW.setWindowShouldClose win b

-- | Causes the GLFW window system to poll for events. Part of this process is a
-- foreign call, so it's uninterruptable during that portion. However,
-- @GLFW-b@ does not directly execute your callbacks while within the foreign
-- portion of the call. Instead, it only schedules each of your callbacks to be
-- executed, and they they are executed within the normal GHC runtime once the
-- foreign portion of the call returns.
--
-- __Thread Safety:__ Main thread only.
pollEvents :: Hexes ()
pollEvents = liftIO $ GLFW.pollEvents

-- TODO: Offer waitEvents and postEmptyEvent as well.

-- | Swaps the foreground and background framebuffer.
--
-- __Thread Safety:__ Any thread when used on its own, but the OpenGL drawing
-- that you probably do before this is main only.
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
--
-- __Thread Safety:__ Main thread only.
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
--
-- __Thread Safety:__ Any thread, but access is not synchronized.
getTime :: Hexes (Maybe Double)
getTime = Hexes $ liftIO $ GLFW.getTime

-- | A callback for whenever a key is pressed or released.
type HexesKeyCallback = GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> Hexes ()

-- | Assigns the callback to use for key presses. Only one callback can be set
-- at a time, and if you assign 'Nothing' then it will clear the current
-- callback selection.
--
-- __Thread Safety:__ Main thread only.
setKeyCallback :: Maybe HexesKeyCallback -> Hexes ()
setKeyCallback maybeCallback = do
    -- We need to intercept the window argument of the GLFW.KeyCallback so the
    -- user doesn't see it, and the callback that we give to GLFW also has to be
    -- an IO () values, so we need to pass down our MVar and run a
    -- sub-computation. All of the MVar's changes are synchronized so it's fine.
    win <- getWindow
    ref <- Hexes $ ask
    Hexes $ liftIO $ case maybeCallback of
        Nothing -> GLFW.setKeyCallback win Nothing
        Just keyCall -> GLFW.setKeyCallback win $ Just $
            \win key int keyState modKeys -> let
                hexesAct = keyCall key int keyState modKeys :: Hexes ()
                readerTthing = unwrapHexes hexesAct
                in runReaderT readerTthing ref

-- | A callback for when a character gets typed into the window.
type HexesCharCallback = Char -> Hexes ()

-- | Assigns the callback to use for typed characters. Only one callback can be
-- set at a time, and if you assign 'Nothing' then it will clear the current
-- callback selection.
--
-- __Thread Safety:__ Main thread only.
setCharCallback :: Maybe HexesCharCallback -> Hexes ()
setCharCallback maybeCallback = do
    win <- getWindow
    ref <- Hexes $ ask
    Hexes $ liftIO $ case maybeCallback of
        Nothing -> GLFW.setCharCallback win Nothing
        Just charCall -> GLFW.setCharCallback win $ Just $
            \win char -> let
                hexesAct = charCall char :: Hexes ()
                readerTthing = unwrapHexes hexesAct
                in runReaderT readerTthing ref

-- all these are TODO
-- setStickyKeysInputMode
-- setStickyMouseButtonsInputMode
-- getKey
-- getMouseButton
-- getCursorPos
-- setMouseButtonCallback
-- setCursorPosCallback
-- setCursorEnterCallback
-- setScrollCallback
-- setDropCallback
-- getJoystickAxes
-- getJoystickButtons
-- setTime
