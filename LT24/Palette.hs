module LT24.Palette where

import CLaSH.Prelude

{-
 - Convert a 5-bit subpixel brightness to a 6-bit brightness
 -
 - Ranges from black to full brightness.
 -}

pal5bTo6b :: Unsigned 5
          -> Unsigned 6

pal5bTo6b 0 = 0
pal5bTo6b b = fromBV $ (<: H) $ toBV b

{-
 - Convert a 6-bit subpixel brightness to a 5-bit brightness
 -
 - Just discard the LSB.
 -}

pal6bTo5b :: Unsigned 6
          -> Unsigned 5

pal6bTo5b = resize . (`shiftR` 1)

{-
 - Convert a (5,6,5) color triplet to a 16bpp pixel color value
 -}
pal565To16bpp :: (Unsigned 5, Unsigned 6, Unsigned 5)
              -> Unsigned 16

pal565To16bpp (r,g,b) = fromBV $ toBV r <++> toBV g <++> toBV b

{-
 - Convert a (5,5,5) color triplet to a 16bpp pixel color value
 -}

pal555To16bpp (r,g,b) = pal565To16bpp (r, pal5bTo6b g, b)

