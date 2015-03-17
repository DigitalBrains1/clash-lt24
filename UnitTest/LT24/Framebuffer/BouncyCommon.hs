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

module UnitTest.LT24.Framebuffer.BouncyCommon where

import CLaSH.Prelude
import Control.Applicative
import Debug.Trace

import qualified LT24.LT24 as LT24
import LT24.Commands
import Toolbox.Misc
import qualified Toolbox.ClockScale as CS
import Toolbox.FClk

{-
 - Make a ball travel at 45° angles and bounce against the sides of the screen.
 -
 - `fb` is a generalised version of the two possible framebuffers,
 - LT24.Framebuffer and LT24.FramebufferRMW. The accompanying Haskell files
 - specify which one to actually use.
 -
 - A blue ball with a red center is drawn at coordinates (x,y). A window of
 - 64x48 pixels is projected on the full screen. The window is chosen in such a
 - way, that not only the ball can be drawn, but it also overlaps with the ball
 - drawn in the previous frame. This is accomplished by `juggleCoords`. If the
 - window consists of a black background around the ball, this means that the
 - black will overwrite the previously drawn ball, and the ball appears to
 - move.
 -
 - If, however, the FramebufferRMW version is used and the background is not
 - black but transparent, then the ball will "streak", leaving a blue trail
 - wherever it goes. This doesn't look as nice, but it demonstrates the
 - workings of the transparency that the RMW version offers.
 -
 - A 4-colour palette is used. Colours 0 and 1 are black, 2 is red and 3 is
 - blue. But for FramebufferRMW, 0 indicates "transparent". KEY1 on the
 - DE0-Nano switches the background between colour 0 and colour 1. When colour
 - 1 is used, the net effect is that the RMW version functions equally to the
 - non-RMW version.
 -
 - The sampled value of KEY1 is folded over time: `buttonF` is L if `button`
 - has been L since the last time `doUpdate` was True. The effect is that the
 - button is considered pressed if it has been pressed between two screen
 - updates.
 -
 - The signal `doUpdate` triggers several things that advance the system to
 - the next cycle of operation. It primarily triggers the CλaSH framebuffer
 - component to start transferring pixels to the display, but it also:
 -  - Resets the one-shot timer that determines when the next frame (with the
 -    ball in the next position) will be drawn.
 -  - Triggers `ballPos` to compute the next ball position.
 -  - Clears the register tracking button state.
 -}
bouncyBall fb i = o
    where
        o = ((combineOutput <$>) . pack)
              ( signal L, signal H, lcdOn, csx, resx, dcx, wrx, rdx, ltdout
              , oe)
        ltdin = (fromBV . vdrop d2) <$> i
        button = vhead <$> i

        buttonF = tfold (.&.) H (button, doUpdate)
        period = ($(CS.staticOneShotPeriod fClk 0.02) <^> 1) doUpdate
--        period = signal True
        doUpdateD = register False doUpdate
        (x, y) = (ballPos <^> (5, 7, BpDown, BpRight)) doUpdateD
        (xD, yD) = (delayCoords <^> (5, 7)) (x,y, doUpdateD)
        (wx, wy, rx, ry) = (unpack . (juggleCoords <$>) . pack) (x,y,xD,yD)
        (lt24AD, fbAddr, fbDin, fbWrEn, doUpdate, needAccess)
            = (drawBall <^> DbInitDisp 0)
                (wx, wy, rx, ry, accepted, period, buttonF)

        (action, din, accepted) = untilAccept (lt24AD, ready)

        pageStart = (resize . fromBV . toBV) <$> wy :: Signal (Unsigned 8)
        ( ready, fbDout, updateDone, pixelVal, dout, lcdOn, csx, resx, dcx,
          wrx, rdx, ltdout, oe)
--            = fb ( action, din, fbAddr, fbDin, fbWrEn, pageStart, doUpdate
--                 , pixelColour, ltdin)
            = fb ( action, din, fbAddr, fbDin, fbWrEn, pageStart
                 , not <$> needAccess, pixelColour, ltdin)
        -- Black, black, red, blue
        pixelColour = ($(v [ 0x1F :: Unsigned 16, 0xF800, 0, 0 ])!)
                     <$> pixelVal

{-
 - Because CλaSH components should have a single input and a single output
 - (otherwise they can't be simulated), but tuples in the topEntity produce an
 - unpractical type in the generated VHDL, all inputs and outputs for the
 - topEntity are combined into a bitvector. The VHDL wrapper then untangles the
 - vector.
 -}
combineOutput (gpioO, txd, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe)
    = ((gpioO :> txd :> lcdOn :> csx :> resx :> dcx :> wrx :> rdx :> Nil)
       <++> toBV ltdout) <: oe

{-
 - Alternative interface to LT24.lt24: `i` = Just "action" to request an
 - action, and wait for `accepted` to become True.
 -
 - Examples: `i` = Just (LT24.Command, cCASET)
 -           `i` = Just (LT24.Write, 0xCAFE)
 -
 - This way of passing actions can make it easier to read; the disadvantage is
 - you can't burst back-to-back, you always lose a clock cycle in which
 - `accepted` first becomes True.
 -}
untilAccept = untilAccept' <^> (LT24.NOP, 0, True)
untilAccept' (c, d, lastReady) (i, ready)
    = ((c', d', ready), (c', d', accepted))
    where
        sawAccept = lastReady && not ready
        (c', d') = case (i, sawAccept) of
                     (Just (ci,di), _    ) -> (ci      , di)
                     (Nothing     , False) -> (c       , d )
                     (Nothing     , True ) -> (LT24.NOP, d )
        accepted = c == LT24.NOP

data BallHDir = BpLeft | BpRight
data BallVDir = BpUp | BpDown

-- When input is True, choose next ball coordinates; change direction when
-- hitting a border
ballPos (x, y, v, h) False = ((x , y , v , h ),(x, y))
ballPos (x ,y, v, h) True  = ((x', y', v', h'),(x, y))
    where
        h' = case (h, x, xl-x) of
               (BpLeft , 0, _) -> BpRight
               (BpRight, _, 0) -> BpLeft
               _               -> h
        v' = case (v, y, yl-y) of
               (BpUp  , 0, _) -> BpDown
               (BpDown, _, 0) -> BpUp
               _              -> v

        x' = case h' of
               BpLeft  -> x - 1
               BpRight -> x + 1
        y' = case v' of
               BpUp   -> y - 1
               BpDown -> y + 1

        xl = 320 - bbox
        yl = 240 - bbox

-- Make previous coordinates available
delayCoords s (x, y, False) = (s     , s)
delayCoords s (x, y, True ) = ((x, y), s)

-- Decide a window position that overlaps with both the previous and the
-- current ball position
juggleCoords (x, y, xD, yD) = (wx, wy, rx, ry)
    where
        wx = min (320-64) $ min x xD
        wy = min (240-48) $ min y yD
        rx = x - wx
        ry = y - wy

data DbState = DbInitDisp (Unsigned 4) | DbWriteRam (Signed 14) (Signed 14)
             | DbDone
    deriving (Show, Eq)

data DbI = DbI
    { dbWx :: Signed 14
    , dbWy :: Signed 14
    , dbRx :: Signed 14
    , dbRy :: Signed 14
    , dbAccepted :: Bool
    , dbPeriod :: Bool
    , dbButtonF :: Bit
    }

data DbO = DbO
    { dbLt24AD :: Maybe (LT24.Action, Unsigned 16)
    , dbFbAddr :: Unsigned 12
    , dbFbDin :: Unsigned 2
    , dbFbWrEn :: Bool
    , dbDoUpdate :: Bool
    , dbNeedAccess :: Bool
    }

dbO = DbO
    { dbLt24AD = Nothing
    , dbFbAddr = 0
    , dbFbDin = 0
    , dbFbWrEn = False
    , dbDoUpdate = False
    , dbNeedAccess = False
    }

{-
 - Draw a ball in the 64x48 framebuffer with a top-left corner at (rx, ry). The
 - LT24 controller is told that it should draw this 64x48 framebuffer at
 - coordinates (wx, wy) (again top-left corner).
 -
 - Finally, request the framebuffer component (LT24.Framebuffer or
 - LT24.FramebufferRMW) to transfer the 64x48 framebuffer to the display.
 -
 - When the one-shot timer runs out, start from the top.
 -
 - The button chooses the background colour: 0 when not pressed, 1 when pressed.
 -}
drawBall s (wx, wy, rx, ry, accepted, period, buttonF)
    = (s', (lt24AD, fbAddr, fbDin, fbWrEn, doUpdate, needAccess))
    where
        i = DbI { dbWx = wx
                , dbWy = wy
                , dbRx = rx
                , dbRy = ry
                , dbAccepted = accepted
                , dbPeriod = period
                , dbButtonF = buttonF
                }
        (s', o) = drawBall' s i
        lt24AD = dbLt24AD o
        fbAddr = dbFbAddr o
        fbDin = dbFbDin o
        fbWrEn = dbFbWrEn o
        doUpdate = dbDoUpdate o
        needAccess = dbNeedAccess o

-- Waiting for `untilAccept`
drawBall' s (DbI { dbAccepted = False })
    = (s, dbO { dbNeedAccess = True })

-- Tell the display controller where to draw the 64x48 framebuffer
drawBall' s@(DbInitDisp n) i
    = ( s'
      , dbO { dbLt24AD = Just ad
            , dbNeedAccess = True
            })
    where
        s' | n == 9    = DbWriteRam 0 0
           | otherwise = DbInitDisp (n+1)
        ad = case n of
               0 -> (LT24.Command, cCASET)
               1 -> (LT24.Write  , xsH   )
               2 -> (LT24.Write  , xsL   )
               3 -> (LT24.Write  , xeH   )
               4 -> (LT24.Write  , xeL   )
               5 -> (LT24.Command, cPASET)
               6 -> (LT24.Write  , ysH   )
               7 -> (LT24.Write  , ysL   )
               8 -> (LT24.Write  , yeH   )
               9 -> (LT24.Write  , yeL   )
        xs = (resize . fromBV . toBV . dbWx) i :: Unsigned 9
        ys = (resize . fromBV . toBV . dbWy) i :: Unsigned 9
        xe = xs + 63
        ye = ys + 47
        xsL = resize (resize xs :: Unsigned 8)
        xsH = resize (xs `shiftR` 8)
        xeL = resize (resize xe :: Unsigned 8)
        xeH = resize (xe `shiftR` 8)
        ysL = resize (resize ys :: Unsigned 8)
        ysH = resize (ys `shiftR` 8)
        yeL = resize (resize ye :: Unsigned 8)
        yeH = resize (ye `shiftR` 8)

{-
 - Draw all 64x48 pixels; Pythagoras determines the colour (together with the
 - button)
 -
 - `toHub` refers to the distance from the current coordinate to the hub of the
 - ball that is to be drawn. (`dbRx`, `dbRy`) is the position of the top-left
 - corner of the bounding box of the ball, relative to the 64x48 framebuffer.
 -
 - When all pixels have been written, signal `doUpdate`.
 -}
drawBall' s@(DbWriteRam x y ) i
    = ( s'
      , dbO { dbFbAddr = fromBV $ yBV <++> xBV
            , dbFbDin  = d
            , dbFbWrEn = True
            , dbDoUpdate = s' == DbDone
            })
    where
        s' = case (x,y) of
               (63, 47) -> DbDone
               (63, _ ) -> DbWriteRam 0     (y+1)
               (_ , _ ) -> DbWriteRam (x+1) y

        xBV = vdropI (toBV x) :: Vec 6 Bit
        yBV = vdropI (toBV y) :: Vec 6 Bit

        toHubX   = outerRBall + dbRx i - x
        toHubY   = outerRBall + dbRy i - y
        toHubSq  = toHubX * toHubX + toHubY * toHubY

        d | toHubSq <= innerRBall * innerRBall = 2
          | toHubSq <= outerRBall * outerRBall = 3
          | dbButtonF i == L                   = 1
          | otherwise                          = 0

-- One-shot timer has finished, next cycle
drawBall' s@(DbDone) (DbI { dbPeriod = True }) = (DbInitDisp 0, dbO)

drawBall' s i = (s, dbO)

-- Radius of blue ball
outerRBall = 23
-- Radius of red center of the ball
innerRBall = 5
-- Bounding box of the ball
bbox = outerRBall * 2 + 1
