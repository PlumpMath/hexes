[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# Hexes

Hexes is a curses-inspired library for Haskell that lets you work with a 2d grid
of tiles out of a tileset you specify.

* Every cell has a Word8 tile ID, RGB background color, and RGBA foreground
  color. You specify how many rows and columns you want in your grid and a
  window is made of the appropriate sized based on the tileset you specify.

* The tilesets are loaded with
  [JuicyPixels](https://hackage.haskell.org/package/JuicyPixels), so many common
  file formats are supported. You load the image yourself, apply any
  pre-processing that you want (like maybe converting a specific color into an
  alpha channel), and then pass an `Image PixelRGBA8` value to the Hexes library
  when you're ready to go (the
  [convertRGBA8](https://hackage.haskell.org/package/JuicyPixels-3.2.8.1/docs/Codec-Picture.html#v:convertRGBA8)
  function is great help here). The library assumes that all tileset images are
  10 tiles wide and 26 tiles tall for the purposes of picking out individual
  tiles. The exact size of a tile is any size you want, Hexes will just figure
  it out based on the size of your image. You'll get a warning message if your
  image isn't an even number of tiles wide or tall (though it will still work
  with the extra bits just cut off).

* User input comes from [GLFW-b](https://hackage.haskell.org/package/GLFW-b), so
  you just set your callbacks and then use `pollEvents` at the start of each run
  through the main loop. Note that `pollEvents` is actually a foreign call, with
  all the troubles that implies, and so your callbacks are only _scheduled_ by
  it, then they are run by the normal GHC runtime once `pollEvents` returns,
  without the trouble of being locked inside a foreign call.

* Rendering is done by [gl](https://hackage.haskell.org/package/gl), but you
  don't touch any of that yourself. Instead you just set the data for each cell
  and call `refresh` when you're ready. Hexes handles all the rendering details
  for you.

## Status

Right now it's un a useable but barebones state. You can poll for keyboard
events, update the foreground, background, and tileID of any cell within the
grid, and refresh the grid. This makes it "usable" in a minimal sense, but
there's more work to be done for sure. Most of the work from here is not hard,
it's just boilerplate type stuff that's unexciting to do.

Developed with `stack`, but should theoretically also work with `cabal-install`.
To build and run the demo just run something like this:

```
stack build && stack exec demo
```
