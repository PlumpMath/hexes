
-- | Hexes is best summarized as "Curses for Haskell, in Open GL".
--
-- This module allows you to easily work with a 2d grid of tile cells and have
-- them be rendered to the screen using OpenGL in a cross-platform way. It's
-- assumed that many of the tiles in your tileset will be pictures of various
-- character glyphs, so that your tile grid can be used to display a grid of
-- characters. That's not actually required though.
--
-- Hexes cells each have a 'Word8' which determines what tile to draw in that
-- cell. This means that there are 256 possible things to show in a single
-- tileset. Many functions let you update a cell using a 'Char' value, and in
-- that case the character's 'ord' is converted into a Word8 for you. The
-- "printable" ASCII range uses 32 through 127, so you'll have spare space in
-- your tileset even if you want to be able to print all the normal glyphs. You
-- can put special extension glyphs in there or leave the slots blank and not
-- worry about them.
--
-- In addition to a Word8 which determines which tile from the tileset to use,
-- each cell has a controlable background (RGB) and forground (RGBA) color. Your
-- background color is always treated as being fully opaque, because if the tile
-- and forground of that cell are both totally transparent then something must
-- still be drawn. The foreground can have any alpha value you like, and that
-- determines how much it's overlayed over the tile data before that's drawn
-- over the background. It works like this:
--
-- * If your foreground color is fully transparent, then your tile data is drawn
-- exactly, and you basically have a tile engine on your hands.
--
-- * If your foreground is fully opaque, then it completely replaces the normal
-- coloration of the tile data entirely. This lets you have your letters be any
-- color you like with only a single small tileset.
--
-- * In either case, the tile's alpha value is used to determine how much of the
-- tile+foreground mixture to draw overtop of the background color. So even with
-- a foreground Alpha of 1, if your tile has a tansparent alpha layer in that
-- spot you'll still see the background. This lets your tiles be colorized only
-- in their actual locations, without the forground color totally overtaking the
-- background color.
--
-- Because editing a 2d grid has two major conventions that you might want to
-- use, Hexes provides for both. When selecting a grid location there are
-- generally 'RC' and 'XY' variants:
--
-- * RC forms allow you to specify Row and then Column. Both values are 0
-- indexed, and __Rows increase in number as you go down the screen__. This is
-- how curses or a text editor does things.
--
-- * XY forms allow you to specify X and then Y. Both values are 0 indexed, and
-- __Y increases in number as you go up the screen__. This is how math does
-- things in Quadrant 1 of a Cartesian Grid.
--
-- * In both cases, Column and X increase as you move from right to left.
module Hexes (
    -- * Core
    Hexes(),
    runHexes,
    windowShouldClose,
    pollEvents,
    refresh,

    -- * Extra
    getRowColCount,
    getCellWidthHeight,
    getTime
    ) where

import Hexes.Internal
import Hexes.Internal.Types
import Hexes.Internal.GLFW
import Hexes.Internal.Shader


-- TODO: Callbacks to gather user input

-- TODO: Actions to write output to the grid.
