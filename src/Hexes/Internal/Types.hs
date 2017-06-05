{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

-- | Module for the types defined by the Hexes package. Primarily for the Hexes
-- newtype. It is intended that, if this module has been completely filled, you
-- will never have to use wrap the Hexes constructor around the get, put, and
-- modify StateT operations. Because that's just annoying.
module Hexes.Internal.Types where

-- base
import Control.Applicative (liftA2)
import Control.Concurrent.MVar
import Data.Word
import Debug.Trace
import Foreign.Storable
-- GLFW-b
import qualified Graphics.UI.GLFW as GLFW
-- gl
import Graphics.GL.Core33
import Graphics.GL.Types
-- JuicyPixels
import Codec.Picture
-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
-- linear
import Linear hiding (trace)
-- vector
import qualified Data.Vector.Storable as VS
-- storable-tuple
import Foreign.Storable.Tuple
-- lens
import Control.Lens

-- | The number of rows we expect to find in a proper image file. This is a
-- constant 26, but we're naming it for clarity.
tileSetRowCount :: Integral i => i
tileSetRowCount = 26

-- | The number of columns we expect to find in a proper image file. This is a
-- constant 10, but we're naming it for clarity.
tileSetColCount :: Integral i => i
tileSetColCount = 10

-- | The state tracked within a Hexes computation.
data HexesData = HexesData {
    -- | How many rows we're using
    rowCount :: !Int,

    -- | How many cols we're using
    colCount :: !Int,
    
    -- | How wide (in Pixels) a cell is.
    cellWidth :: !Int,
    
    -- | How tall (in Pixels) a cell is.
    cellHeight :: !Int,
    
    -- | Our GLFW window.
    window :: GLFW.Window,

    -- TODO: newtype the crap out of all this GL stuff and provide safe wrappers
    -- all over the place.

    -- | Our gl shader program.
    shaderProgram :: GLuint,

    -- | The verticies of our program.
    verticies :: VS.Vector CellData,

    -- | Our Vertex Array Object.
    theVAO :: GLuint,

    -- | Our Vertex Buffer Object.
    theVBO :: GLuint
    }

-- TODO: Make HexesData storable and store it in a Ptr instead of an MVar maybe?
-- Probably faster, but not thread safe at all. Then again we might just want to
-- mark all the drawing stuff as just being main thread only anyway because GLFW
-- and OGL are already pretty finnicky on thread safety as it is.

-- | Danger! This just lets you fill in the record as you go. You still need to
-- fill in the whole record before you start using it.
mkState :: Int -> Int -> Image PixelRGBA8 -> HexesData
mkState rows cols img = let
    iWidth = imageWidth img
    iHeight = imageHeight img
    (cellWidth,widthSpare) = iWidth `divMod` tileSetColCount
    (cellHeight,heightSpare) = iHeight `divMod` tileSetRowCount
    in HexesData {
    rowCount = rows,
    colCount = cols,
    cellWidth = if widthSpare == 0
        then cellWidth
        else trace ("WARNING: Image has spare pixel width: "++show widthSpare) cellWidth,
    cellHeight = if heightSpare == 0
        then cellHeight
        else trace ("WARNING: Image has spare pixel height: "++show heightSpare) cellHeight,
    window = undefined,
    shaderProgram = 0,
    verticies=undefined,
    theVAO=0,
    theVBO=0
    }

-- | A Hexes computation is one that wraps up whole a lot of 'GLFW' and 'gl'
-- activity so that you can easily manipulate a grid of characters and have it
-- be rendered to the screen, similar to how curses works. Or, how it might work
-- if it was much easier to use at least.
--
-- Though this is a MonadIO newtype, it is *not* suggested to call any 'GLFW' or
-- 'gl' code from within this monad yourself. Anything you should be interacting
-- with from those packages is already provided to you as a 'Hexes' action
-- instead. If you make your own calls to 'GLFW' or 'gl' from within a 'Hexes'
-- action and something gets messed up, that's your fault.
--
-- Thread Safety: The Hexes data is kept in an MVar, so you can potentially use
-- Hexes in more than one thread at once. However, there's no way to batch up a
-- transaction, and also the 'refresh' call should only be made from the main
-- thread anyway.
newtype Hexes a = Hexes (ReaderT (MVar HexesData) IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Unwraps a Hexes back into its non-newtype form. Since Hexes is a newtype,
-- this actually compiles away into nothing. It's just for type juggling.
unwrapHexes :: Hexes a -> ReaderT (MVar HexesData) IO a
unwrapHexes (Hexes action) = action

-- | Looks into the HexesState for whatever you're after.
hexGets :: (HexesData -> a) -> Hexes a
hexGets projectFunc = Hexes $ do
    var <- ask
    liftIO $ withMVar var (\hexesData -> return $ projectFunc hexesData)

-- | Modifies the HexesState however you like.
hexModify :: (HexesData -> HexesData) -> Hexes ()
hexModify modifyFunc = Hexes $ do
    var <- ask
    liftIO $ modifyMVar_ var (\hexesData -> return $ modifyFunc hexesData)

-- | Obtains the 'GLFW.Window' for the current Hexes context.
getWindow :: Hexes GLFW.Window
getWindow = hexGets window

-- | Updates the 'GLFW.Window' for the current Hexes context.
setWindow :: GLFW.Window -> Hexes ()
setWindow newWindow = hexModify (\s -> s {window=newWindow})

-- | Gets the number of rows and cols that we're using.
getRowColCount :: Hexes (Int,Int)
getRowColCount = hexGets $ liftA2 (,) rowCount colCount

-- | Gets the width and height of the display cells that we're using.
getCellWidthHeight :: Hexes (Int,Int)
getCellWidthHeight = hexGets $ liftA2 (,) cellWidth cellHeight

-- | Obtains the shader program.
getShaderProgram :: Hexes GLuint
getShaderProgram = hexGets shaderProgram

-- | Assigns the shader program.
setShaderProgram :: GLuint -> Hexes ()
setShaderProgram newProgram = hexModify (\s -> s {shaderProgram=newProgram})

-- | Assigns the verticies to use.
setVerticies :: VS.Vector CellData -> Hexes ()
setVerticies verts = hexModify (\s -> s{verticies=verts})

-- | Gets the verticies we're using.
getVerticies :: Hexes (VS.Vector CellData)
getVerticies = hexGets verticies

-- | Assigns our Vertex Array Object
setTheVAO :: GLuint -> Hexes ()
setTheVAO vao = hexModify (\s -> s{theVAO=vao})

-- | Gets our Vertex Array Object
getTheVAO :: Hexes GLuint
getTheVAO = hexGets theVAO

-- | Sets out VBO ID.
setTheVBO :: GLuint -> Hexes ()
setTheVBO vbo = hexModify (\s -> s{theVBO=vbo})

-- | Obtains our VBO ID.
getTheVBO :: Hexes GLuint
getTheVBO = hexGets theVBO

-- | A single vertex entry that ogl will read.
newtype VertexEntry = VertexEntry { _vertexEntry :: (V2 GLfloat, V2 GLfloat, V3 GLfloat, V4 GLfloat) }
    deriving (Eq, Ord, Show, Storable)

-- | Converts a VertexEntry into the list of float data you need to push to the
-- GPU.
vertexEntryToList :: VertexEntry -> [GLfloat]
vertexEntryToList (VertexEntry ((V2 x y),(V2 s t),(V3 bgr bgb bgg),(V4 fgr fgg fgb fga))) =
    [x,y,s,t,bgr,bgb,bgg,fgr,fgg,fgb,fga]

-- | A series of three vertex entries, forming a triangle.
newtype CellTriangle = CellTriangle { _cellTriangle :: (VertexEntry,VertexEntry,VertexEntry) }
    deriving (Eq, Ord, Show, Storable)

-- | Converts a CellTriangle into the list of float data you need to push to the
-- GPU.
cellTriangleToList :: CellTriangle -> [GLfloat]
cellTriangleToList (CellTriangle (a,b,c)) =
    concatMap vertexEntryToList [a,b,c]

-- | The two triangles that form a cell on the screen.
newtype CellData = CellData { _cellData :: (CellTriangle,CellTriangle) }
    deriving (Eq, Ord, Show, Storable)

makeLenses ''VertexEntry
makeLenses ''CellTriangle
makeLenses ''CellData

-- TODO: Haddock for the lenses

-- | Converts a CellData into the list of float data you need to push to the
-- GPU.
cellDataToList :: CellData -> [GLfloat]
cellDataToList (CellData (upperRight,bottomLeft)) =
    concatMap cellTriangleToList [upperRight,bottomLeft]

-- | Given the correct info, forms a CellPair value.
mkCellData :: Int        -- ^ The width (in pixels) of one cell/tile
           -> Int        -- ^ The height (in pixels) of one cell/tile
           -> Int        -- ^ The number of columns in the grid
           -> Word8      -- ^ The cell's tileID
           -> V3 GLfloat -- ^ The cell's background color
           -> V4 GLfloat -- ^ The cell's foreground color
           -> Int        -- ^ Cell's index within the grid
           -> CellData
mkCellData wI hI cols word bg fg index = let
    (rowI,colI) = index `divMod` cols
    (tI,sI) = (fromIntegral word) `divMod` 10 -- TODO: Make this not a magic number
    w = fromIntegral wI :: GLfloat
    h = fromIntegral hI :: GLfloat
    row = fromIntegral rowI :: GLfloat
    col = fromIntegral colI :: GLfloat
    s = fromIntegral sI :: GLfloat
    t = fromIntegral tI :: GLfloat
    in CellData (
        CellTriangle (
                VertexEntry (V2 (w*col)     (h*row),    V2 (w*s)     (h*t)    ,bg,fg),
                VertexEntry (V2 (w*(col+1)) (h*row),    V2 (w*(s+1)) (h*t)    ,bg,fg),
                VertexEntry (V2 (w*(col+1)) (h*(row+1)),V2 (w*(s+1)) (h*(t+1)),bg,fg)
            ),
        CellTriangle (
                VertexEntry (V2 (w*(col+1)) (h*(row+1)),V2 (w*(s+1)) (h*(t+1)),bg,fg),
                VertexEntry (V2 (w*col)     (h*(row+1)),V2 (w*s)     (h*(t+1)),bg,fg),
                VertexEntry (V2 (w*col)     (h*row),    V2 (w*s)     (h*t)    ,bg,fg)
            )
        )

-- | Given tile width and height, and the new tile index to use, this will
-- convert an old CellData value to the new tile index, preserving all other
-- values within the CellData.
setCellDataTileID :: Int -> Int -> Word8 -> CellData -> CellData
-- TODO: find a way to extract the cell width and height from the xy pairs so
-- that we can write this whole function as just foo :: Word8 -> CellData ->
-- CellData and then expose it to the user. Maybe the user could perform their
-- own maps and such and get stream fusion going?
setCellDataTileID wI hI word (CellData (
        CellTriangle (
                VertexEntry (xy1, _, bg1, fg1),
                VertexEntry (xy2, _, bg2, fg2),
                VertexEntry (xy3, _, bg3, fg3)
            ),
        CellTriangle (
                VertexEntry (xy4, _, bg4, fg4),
                VertexEntry (xy5, _, bg5, fg5),
                VertexEntry (xy6, _, bg6, fg6)
            )
        )) = let
        (tI,sI) = (fromIntegral word) `divMod` 10 -- TODO: Make this not a magic number
        w = fromIntegral wI :: GLfloat
        h = fromIntegral hI :: GLfloat
        s = fromIntegral sI :: GLfloat
        t = fromIntegral tI :: GLfloat
        in CellData (
        CellTriangle (
                VertexEntry (xy1, V2 (w*s)     (h*t)    , bg1, fg1),
                VertexEntry (xy2, V2 (w*(s+1)) (h*t)    , bg2, fg2),
                VertexEntry (xy3, V2 (w*(s+1)) (h*(t+1)), bg3, fg3)
            ),
        CellTriangle (
                VertexEntry (xy4, V2 (w*(s+1)) (h*(t+1)), bg4, fg4),
                VertexEntry (xy5, V2 (w*s)     (h*(t+1)), bg5, fg5),
                VertexEntry (xy6, V2 (w*s)     (h*t)    , bg6, fg6)
            )
        )

-- | Updates a CellData value to have a new background color without making any
-- other changes to the CellData.
setCellDataBackground :: V3 GLfloat -> CellData -> CellData
setCellDataBackground newBG cell = let
    bgPath = cellData.each.cellTriangle.each.vertexEntry._3
    in cell & bgPath .~ newBG

-- | Updates a CellData value to have a new foreground color without making any
-- other changes to the CellData.
setCellDataForeground :: V4 GLfloat -> CellData -> CellData
setCellDataForeground newFG cell = let
    bgPath = cellData.each.cellTriangle.each.vertexEntry._4
    in cell & bgPath .~ newFG

{-
^. = get
.~ = set
%~ = modify

tileID = complicated, we have to set the value in each of 6 places.

bgPath = cellPair.each.cellTriangle.each.vertexEntry._3
fgPath = cellPair.each.cellTriangle.each.vertexEntry._4

examples:
setBG = cell & cellPair.each.cellTriangle.each.vertexEntry._3 .~ newRGB
setFG = cell & cellPair.each.cellTriangle.each.vertexEntry._4 .~ newRGBA
-}
