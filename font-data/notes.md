
# Font Data Explanations

All font data files must be 10 columns across and 26 rows tall!

The loading process assumes this, and then sets the cell size of the drawing
space based on the font image that you specify.

The glyph grid allows any cell to be set with a Word8 value. This Word8 value is
converted into a row and col with the fomula `divMod x 10`. In otherwords, the
10s digit is the row, and the 1s digit is the col. The assumption is that glyphs
in the 32 to 127 range will be the printable character glyphs. Other locations
can be left blank if you intend only text, or you could place special drawing
characters there such as card suit symbols, line drawing chars, and so on.

For ease of imagemap creation, opaque pure green pixels are converted into
transparency pixels once the image is loaded. In other words, any pixel with
RGBA of (0,255,0,0) is converted to an RGBA of (0,0,0,255). All other pixels are
left alone. The image data is loaded with the JuicyPixels library and then
converted into an 8-bit per channel RGBA image, so you should have quite a
variety of successful input formats.

Remember that the 256th to 260th locations in the grid are impossible to access
from within the program, since a Word8 value only ranges from 0 to 255.

## Making Your Own Imagemaps

If you want to quickly make a suitable image in any monospace font of your font
of choice, just paste the following block of characters into the upper left
corner of your paint program, crop the image to the correct size, and save.
Unfortunately, fonts that aren't mono-spaced will require you to place all the
characters within identically sized cells by hand, since all cells must be of
identical size for the imagemap to be used. You don't actually need to use a
font at all, you can draw anything you like into a 10 wide and 26 tall grid as
long as all the grid cells are identically sized.

```
 ?????????
??????????
??????????
?? !"#$%&'
()*+,-./01
23456789:;
<=>?@ABCED
FGHIJKLMNO
PQRSTUVWXY
Z[\]^_`abc
defghijklm
nopqrstuvw
xyz{|}~???
??????????
??????????
??????????
??????????
??????????
??????????
??????????
??????????
??????????
??????????
??????????
??????????
??????????
```
