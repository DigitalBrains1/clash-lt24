{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-
 - Copyright (c) 2015, Peter Lebbing <peter@digitalbrains.com>
 - All rights reserved.
 - 
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 - 
 - 1. Redistributions of source code must retain the above copyright notice,
 - this list of conditions and the following disclaimer.
 - 
 - 2. Redistributions in binary form must reproduce the above copyright notice,
 - this list of conditions and the following disclaimer in the documentation
 - and/or other materials provided with the distribution.
 - 
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 - AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 - IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 - ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 - LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 - CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 - SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 - INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 - CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 - ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 - POSSIBILITY OF SUCH DAMAGE.
 -}

module LT24.Palette where

import CLaSH.Prelude

{-
 - Convert a 5-bit subpixel brightness to a 6-bit brightness
 -
 - Ranges from black to full brightness. Uses the same trick as the 16-bit RGB
 - interface from the Ilitek datasheet: put the MSB in the LSB. That way, the
 - dimmest 16 colours start from black, and the brightest 16 colours end in
 - full brightness. Brightness 15 becomes 30, brightness 16 becomes 33.
 -}

pal5bTo6b :: Unsigned 5
          -> Unsigned 6

pal5bTo6b 0 = 0
pal5bTo6b b5 = fromBV $ b5b <: vhead b5b
    where
        b5b = toBV b5

{-
 - Convert a 6-bit subpixel brightness to a 5-bit brightness
 -
 - Just discard the LSB.
 -}

pal6bTo5b :: Unsigned 6
          -> Unsigned 5

pal6bTo5b = resize . (`shiftR` 1)

{-
 - Convert a (5,6,5) colour triplet to a 16bpp pixel colour value
 -}
pal565To16bpp :: (Unsigned 5, Unsigned 6, Unsigned 5)
              -> Unsigned 16

pal565To16bpp (r,g,b) = fromBV $ toBV r <++> toBV g <++> toBV b

{-
 - Convert a (5,5,5) colour triplet to a 16bpp pixel colour value
 -}

pal555To16bpp (r,g,b) = pal565To16bpp (r, pal5bTo6b g, b)

