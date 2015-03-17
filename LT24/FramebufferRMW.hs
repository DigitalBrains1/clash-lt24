{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module LT24.FramebufferRMW
       (framebuffer
       ) where

import CLaSH.Prelude
import Control.Applicative
import Debug.Trace

import qualified LT24.LT24 as LT24
import LT24.Init (lt24WithInit)
import LT24.Commands
import LT24.Palette
import Toolbox.Blockram2p
--import Simul.Toolbox.Blockram2p_2_16
import Toolbox.Misc

{-
 - Framebuffer interface to LT24; 3 colours and transparency
 -
 - The great feature of this framebuffer is that you can keep pixels on the
 - display as they are. This also means that, even though you are limited to
 - drawing in three colours at the same time, you can use all 65K colours by
 - changing your palette on each invocation of `doUpdate`.
 -
 - To achieve the "transparency", this framebuffer interface will read out the
 - current pixel colours from the LT24 display, and on writeback, only replace
 - those pixels that have a non-zero value in the 64x48 framebuffer.
 -
 - To have a bit of temporary storage, the 8192-bits blockram is split in two
 - parts: 6144 bits store the 64x48 2bpp pixels, and the remaining 2048 bits
 - are used to store 128 16bpp pixels. The former area is called "framebuffer"
 - and the latter "scratchpad".
 -
 - In more detail, what happens is this:
 -   - Set LT24 display page address range to the next 2 pages (or rows, or
 -     y coordinates)
 -   - Read 128 pixels in 18bpp back from the display; convert each 18bpp pixel
 -     to its 16bpp direct colour equivalent. Take a look at the 2bpp pixel
 -     stored in the 64x48 framebuffer; if it is zero, store the old pixel
 -     colour in the scratchpad. If it is non-zero, store the new pixel colour
 -     in the scratchpad.
 -   - Write 128 pixels to the LT24 display from the scratchpad
 -   - Continue this process until all 24 blocks of 128 pixels are transferred
 -
 - So the decision to overwrite a pixel or not is taken while reading the old
 - pixels. What is stored in the scratchpad is subsequently copied to the
 - display verbatim.
 -
 - Because this implementation shares a lot of traits with LT24.Framebuffer,
 - only the differences in usage are documented.
 -
 - This CλaSH component takes another input:
 - `pageStart`: The y coordinate of the top-left corner of the 64x48
 -     framebuffer in the 320x240 display. Or equivalently, the top row.
 -
 - Because of this interleaved reading and writing process, this CλaSH
 - component now unfortunately has to know about the page range (the
 - y-coordinate range).  This necessarily means that you are limited to a 64x48
 - framebuffer, and can't decide to use a 16x192 one like you could with
 - LT24.Framebuffer. Also, the fact that the pixels are read in 18bpp format
 - (this is unavoidable) means that you have to use a direct-colour mapping,
 - but that is not a great loss, because surely 65K colours is a lot already.
 - (Note that it is impossible to have an M9K 2-port blockram with one port 2
 - bits wide and one port 18 bits wide, which is why I chose to use 16 bpp.)
 -
 - Hint for somebody wishing to implement a more elaborate read-modify-write
 - behaviour: say you wish to have the output of a camera module on the screen,
 - but want to draw stuff on that video feed. You could use the least
 - significant bit of green to discern whether the old pixel came from the
 - video or from your drawing. If it is from the video, replace it with the new
 - pixel from the camera, if it is from your drawing, keep it. That way you can
 - draw and remove pixels while a video is running underneath your drawing. It
 - is certainly possible to change this file a bit to accomodate this extra
 - logic. It has not been implemented, but I think it's a neat idea and wanted
 - to share it. The loss of resolution on the green channel is certainly very
 - minor.
 -}
framebuffer (actionDaisy, dInDaisy, fbAddr, fbDIn, fbWrEn, pageStart, doUpdate
            , pixelColour, ltdin)
    = ( readyDaisy, fbDout, updateDone, pixelVal, lt24DOut, lcdOn, csx, resx
      , dcx, wrx, rdx, ltdout, oe)
    where

        (fbDout, myRamDOut) = blockram2p d12 d9 d2 d16
                            (fbAddr, fbDIn, fbWrEn, myRamAddr, myRamDIn
                            , myRamWrEn)

        (lt24Ready, lt24DOut, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe) =
            lt24WithInit (lt24Action, lt24DIn, ltdin)

        (x,y, coordsDone) = (genCoords <^> (0, 0)) nextCoords
        myRamAddr  = ((ramAddr <$>) . pack) (x,y, addrMode)
        (pixel1, pixel2) = (unpack . (pixelLanes <$>) . pack) (x, myRamDOut)

        -- Fold over time with "or", in other words: Output True if `doUpdate`
        -- was at some time True since the last `clearDU`.
        doUpdateF = tfoldD (||) False (doUpdate, clearDU)
        ( readyDaisy, updateDone, clearDU, lt24Action, lt24DIn, nextCoords,
          pixelVal, addrMode, myRamDIn, myRamWrEn)
            = (fbFSM <^> FbFSMS { fbState = FbIdle
                                , fbPageStartS = 0
                                , fbPixel2Buf = 0
                                , fbR1 = 0
                                , fbG1 = 0
                                , fbR2 = 0
                                , fbMyActionS = LT24.NOP
                                , fbMyDInS = 0
                                })
                ( actionDaisy, dInDaisy, pageStart, doUpdateF, lt24Ready
                , lt24DOut , coordsDone, pixel1, pixel2, pixelColour, myRamDOut)

{- Step through all coordinates, stepping when `nextCoords` = True.
 - Assert `coordsDone` one clock cycle after last coordinate was produced.
 - Only the even x-coordinates are produced. Because of the way the LT24
 - display combines two pixels in three words when reading data in its most
 - efficient mode, all pixels are processed in pairs, and `nextCoords` needs to
 - advance by two pixels rather than one.
 -}
genCoords (x, y) nextCoords = ((x', y'), (x, y, coordsDone))
    where
        (x', y') | nextCoords = case (x, y) of
                                  (62, 47) -> (0  , 0  )
                                  (62, _ ) -> (0  , y+1)
                                  ( _, _ ) -> (x+2, y  )
                 | otherwise  = (x, y)
        coordsDone = (x, y) == (0, 0)

{- Generate a linear RAM address, either from the (x,y) coordinates or from an
 - index in the scratchpad part, depending on FbAddrMode.
 - fbFSM now reads from the memory with a 16-bit wide data port, but the (x,y)
 - coordinates address 2 bit wide pixels. So the memory address discards the 3
 - least significant bits from the x coordinate. Put differently, 8 consecutive
 - x coordinates map to a single RAM address.
 -}
ramAddr :: (Unsigned 6, Unsigned 6, FbAddrMode)
          -> Unsigned 9
ramAddr (x,y, FbFramebuffer ) = fromBV $ toBV y <++> vtakeI (toBV x)
ramAddr (_,_, FbScratchpad n) = resize n + 384

{- Pick two 2-bit lanes from the 16-bit memory word.
 - The lowest 3 bits of the x coordinate address an even-numbered pixel in the
 - 16-bit word (so actually the least significant bit is always 0). Return that
 - pixel and the odd-numbered pixel following it.
 -}
pixelLanes :: (Unsigned 6, Unsigned 16)
           -> (Unsigned 2, Unsigned 2)
pixelLanes (x, pixelW) = (pixel1, pixel2)
    where
        pixel1 = fromBV
               $ (vunconcatI $ toBV pixelW)!(resize x :: Unsigned 3)
        pixel2 = fromBV
               $ (vunconcatI $ toBV pixelW)!(resize x + 1 :: Unsigned 3)

data FbState = FbIdle | FbSendPA1 | FbSendPA2 | FbSendPA3 | FbSendPA4
             | FbSendPA5 | FbSendPA6 | FbSendPA7 | FbSendPA8 | FbSendPA9
             | FbSendPA10 | FbReadCommand | FbDiscardRead1 | FbDiscardRead2
             | FbDiscardRead3 | FbRead1 (Unsigned 7) | FbRead2 (Unsigned 7)
             | FbRead3 (Unsigned 7) | FbRead4 (Unsigned 7)
             | FbRead5 (Unsigned 7) | FbRead6 (Unsigned 7) | FbWriteCmd1
             | FbWriteCmd2  | FbWrite1 (Unsigned 7) | FbWrite2 (Unsigned 7)
             | FbFinish1 | FbFinish2
    deriving (Show, Eq)

data FbAddrMode = FbFramebuffer | FbScratchpad (Unsigned 7)

-- (S)tate for fbFSM
data FbFSMS = FbFSMS
    { fbState :: FbState
    , fbPageStartS :: Unsigned 8
    , fbPixel2Buf :: Unsigned 2
    , fbR1 :: Unsigned 5
    , fbG1 :: Unsigned 5
    , fbR2 :: Unsigned 5
    , fbMyActionS :: LT24.Action
    , fbMyDInS :: Unsigned 16
    }
    deriving (Show, Eq)

-- (I)nput for fbFSM
data FbFSMI = FbFSMI
    { fbState' :: FbState
    , fbActionDaisy :: LT24.Action
    , fbDInDaisy :: Unsigned 16
    , fbPageStartI :: Unsigned 8
    , fbDoUpdateF :: Bool
    , fbLt24Ready :: Bool
    , fbLt24DOut :: Unsigned 16
    , fbCoordsDone :: Bool
    , fbPixel1 :: Unsigned 2
    , fbPixel2 :: Unsigned 2
    , fbPixelColour :: Unsigned 16
    , fbMyRamDOut :: Unsigned 16
    , fbMyActionI :: LT24.Action
    , fbMyDInI :: Unsigned 16
    }

-- (O)utput for fbFSM1
data FbFSMO1 = FbFSMO1
    { fbReadyDaisy :: Bool
    , fbLt24Action :: LT24.Action
    , fbLt24DIn :: Unsigned 16
    }

-- (O)utput for fbFSM2
data FbFSMO2 = FbFSMO2
    { fbClearDU :: Bool
    , fbNextCoords :: Bool
    , fbPixelVal :: Unsigned 2
    , fbAddrMode :: FbAddrMode
    , fbMyRamDIn :: Unsigned 16
    , fbMyRamWrEn :: Bool
    }

-- Default values for outputs
fbFSMO2 = FbFSMO2
    { fbClearDU = False
    , fbNextCoords = False
    , fbPixelVal = 0
    , fbAddrMode = FbFramebuffer
    , fbMyRamDIn = 0
    , fbMyRamWrEn = False
    }

{- The finite state machine that does all the real work
 -
 - This function merely wraps fbFSM1 and fbFSM2, passing all the signals from
 - and to, and even between them.
 -
 - That is, the inputs fbState', fbMyActionI and fbMyDinI, which are an input
 - to fbFSM1, are actually passed from fbFSM2 (where they happen to be part of
 - the state).
 -
 - The output `updateDone` is computed here. All the other signals are only
 - "converted" from a CλaSH component-style tuple to a record format, and
 - vice-versa. That way, record syntax can be used for pattern matching,
 - supplying default values, and record update syntax for the state, to enhance
 - the readability of the functions fbFSM1 and fbFSM2.
 -}
fbFSM s
      (actionDaisy, dInDaisy, pageStart, doUpdateF, lt24Ready, lt24DOut
      , coordsDone , pixel1, pixel2, pixelColour, myRamDOut)
    = (s', ( readyDaisy, updateDone, clearDU, lt24Action, lt24DIn
           , nextCoords, pixelVal, addrMode, myRamDIn, myRamWrEn))
    where
--        s't | s' == s   = s'
--            | otherwise = trace (show s') s'
        s' = s2'
        readyDaisy = fbReadyDaisy o1
        updateDone = fbState s2' == FbIdle
        clearDU = fbClearDU o2
        lt24Action = fbLt24Action o1
        lt24DIn = fbLt24DIn o1
        nextCoords = fbNextCoords o2
        pixelVal = fbPixelVal o2
        addrMode =fbAddrMode o2
        myRamDIn = fbMyRamDIn o2
        myRamWrEn = fbMyRamWrEn o2

        i = FbFSMI { fbState' = fbState s2'
                   , fbActionDaisy = actionDaisy
                   , fbDInDaisy = dInDaisy
                   , fbPageStartI = pageStart
                   , fbDoUpdateF = doUpdateF
                   , fbLt24Ready = lt24Ready
                   , fbLt24DOut = lt24DOut
                   , fbCoordsDone = coordsDone
                   , fbPixel1 = pixel1
                   , fbPixel2 = pixel2
                   , fbPixelColour = pixelColour
                   , fbMyRamDOut = myRamDOut
                   , fbMyActionI = fbMyActionS s2'
                   , fbMyDInI = fbMyDInS s2'
                   }

        o1 = fbFSM1 i
        (s2', o2) = fbFSM2 s i

-- Daisy chain handler: when we're not using the LT24, let the caller access it
fbFSM1 i@(FbFSMI { fbState' = FbIdle })
    = FbFSMO1 { fbReadyDaisy = fbLt24Ready i
              , fbLt24Action = fbActionDaisy i
              , fbLt24DIn = fbDInDaisy i
              }
fbFSM1 i = FbFSMO1 { fbReadyDaisy = True
                   , fbLt24Action = fbMyActionI i
                   , fbLt24DIn = fbMyDInI i
                   }


{-
 - doUpdate handler
 -
 - Actions for the LT24.LT24 are queued as soon as the previous action is
 - accepted. This way, maximum throughput is achieved even on low clock
 - frequencies.
 -
 - The LT24 display transfers two pixels in three words, with the middle word
 - thus containing part of the first, and part of the second pixel. So all
 - pixels are processed in pairs of two.
 -
 - During reading of the LT24 display, we also need to read from the
 - framebuffer portion of the RAM and write to the scratchpad portion. Because
 - a RAM access takes two cycles to be available on the output port (there are
 - registers in the blockram), the two pixels that are being processed are read
 - in the same cycle, and the second pixel is temporarily stored in a register
 - in the state. This way, the scratchpad write does not interfere with reading
 - the pixels from the framebuffer.
 -}

{-
 - The state machine starts in FbIdle, waiting for an update request. To
 - process the update, LT24.LT24 needs to have finished whatever it was doing
 - before through the daisy chain. Once it is idle and an update is requested:
 - Clear `doUpdateF`. Queue cPASET command to the LT24 display. Store the
 - pageStart value from the input.
 -}
fbFSM2 s@(FbFSMS { fbState = FbIdle }) i@(FbFSMI { fbDoUpdateF = True
                                                 , fbLt24Ready = True
                                                 , fbPageStartI = pageStart })
    = ( s { fbState = FbSendPA1
          , fbMyActionS = LT24.Command
          , fbMyDInS = cPASET
          , fbPageStartS = pageStart
          }
      , fbFSMO2 { fbClearDU = True })

-- lt24 unavailable or no action needed; idle
fbFSM2 s@(FbFSMS { fbState = FbIdle }) i
    = (s, fbFSMO2)

{- Wait for acceptance of cPASET, then queue write of start high byte
 - Since there are only 240 rows, the high byte is always 0.
 -
 - This whole sequence of FbSendPA states could be rewritten in a nicer form.
 -}
fbFSM2 s@(FbFSMS { fbState = FbSendPA1 })
       (FbFSMI { fbLt24Ready = ready })
    | ready     = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbSendPA2
                      , fbMyActionS = LT24.Write
                      , fbMyDInS = 0
                      }
                  , fbFSMO2)

-- Wait for cPASET to complete
fbFSM2 s@(FbFSMS { fbState = FbSendPA2 })
       (FbFSMI { fbLt24Ready = ready })
    | not ready = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbSendPA3 }
                  , fbFSMO2)

-- Wait for acceptance of first write, then queue write of start low byte
fbFSM2 s@(FbFSMS { fbState = FbSendPA3
                 , fbPageStartS = pageStart })
       (FbFSMI { fbLt24Ready = ready })
    | ready     = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbSendPA4
                      , fbMyActionS = LT24.Write
                      , fbMyDInS = resize $ pageStart
                      }
                  , fbFSMO2)

-- Wait for first write to complete
fbFSM2 s@(FbFSMS { fbState = FbSendPA4 })
       (FbFSMI { fbLt24Ready = ready })
    | not ready = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbSendPA5 }
                  , fbFSMO2)

-- Wait for acceptance of second write, then queue write of end high byte
fbFSM2 s@(FbFSMS { fbState = FbSendPA5 })
       (FbFSMI { fbLt24Ready = ready })
    | ready     = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbSendPA6
                      , fbMyActionS = LT24.Write
                      , fbMyDInS = 0
                      }
                  , fbFSMO2)

-- Wait for second write to complete
fbFSM2 s@(FbFSMS { fbState = FbSendPA6 })
       (FbFSMI { fbLt24Ready = ready })
    | not ready = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbSendPA7 }
                  , fbFSMO2)

-- Wait for acceptance of third write, then queue write of end low byte
fbFSM2 s@(FbFSMS { fbState = FbSendPA7
                 , fbPageStartS = pageStart })
       (FbFSMI { fbLt24Ready = ready })
    | ready     = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbSendPA8
                      , fbMyActionS = LT24.Write
                      , fbMyDInS = resize $ pageStart + 1
                      }
                  , fbFSMO2)


-- Wait for third write to complete
fbFSM2 s@(FbFSMS { fbState = FbSendPA8 })
       (FbFSMI { fbLt24Ready = ready })
    | not ready = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbSendPA9 }
                  , fbFSMO2)

-- Wait for acceptance of fourth write, then queue cRAMRD command
fbFSM2 s@(FbFSMS { fbState = FbSendPA9 })
       (FbFSMI { fbLt24Ready = ready })
    | ready     = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbSendPA10
                      , fbMyActionS = LT24.Command
                      , fbMyDInS = cRAMRD
                      }
                  , fbFSMO2)

-- Wait for fourth write to complete
fbFSM2 s@(FbFSMS { fbState = FbSendPA10 })
       (FbFSMI { fbLt24Ready = ready })
    | not ready = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbReadCommand }
                  , fbFSMO2)

-- Wait for acceptance of cRAMRD, then queue dummy read
fbFSM2 s@(FbFSMS { fbState = FbReadCommand })
       (FbFSMI { fbLt24Ready = ready })
    | ready     = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbDiscardRead1
                      , fbMyActionS = LT24.ReadFM
                      }
                  , fbFSMO2)

-- Wait for cRAMRD to complete
fbFSM2 s@(FbFSMS { fbState = FbDiscardRead1 })
       (FbFSMI { fbLt24Ready = ready })
    | not ready = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbDiscardRead2 }
                  , fbFSMO2)

-- Wait for dummy read acceptance, then queue first real read
fbFSM2 s@(FbFSMS { fbState = FbDiscardRead2 })
       (FbFSMI { fbLt24Ready = ready })
    | ready     = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbDiscardRead3
                      , fbMyActionS = LT24.ReadFM
                      }
                  , fbFSMO2)

-- Wait for dummy read to complete
fbFSM2 s@(FbFSMS { fbState = FbDiscardRead3 })
       (FbFSMI { fbLt24Ready = ready })
    | not ready = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbRead1 0 }
                  , fbFSMO2)

-- Wait for first real read acceptance, queue second read
fbFSM2 s@(FbFSMS { fbState = FbRead1 n })
       (FbFSMI { fbLt24Ready = ready })
    | ready     = (s, fbFSMO2)
    | otherwise = (s { fbState = FbRead2 n
                     , fbMyActionS = LT24.ReadFM
                     }
                  , fbFSMO2)

-- Wait for read to complete, store red and green subpixels
fbFSM2 s@(FbFSMS { fbState = FbRead2 n })
       (FbFSMI { fbLt24Ready = ready
               , fbLt24DOut = dat
               })
    | not ready = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbRead3 n
                      , fbR1 = r1
                      , fbG1 = g1
                      }
                  , fbFSMO2)
    where
        r1 = pal6bTo5b $ resize $ dat `shiftR` 10
        g1 = resize $ dat `shiftR` 2

-- Wait for second read acceptance, queue third read
fbFSM2 s@(FbFSMS { fbState = FbRead3 n })
       (FbFSMI { fbLt24Ready = ready })
    | ready     = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbRead4 n
                      , fbMyActionS = LT24.ReadFM
                      }
                  , fbFSMO2)

-- Wait for second read to complete, compute and store first pixel
fbFSM2 s@(FbFSMS { fbState = FbRead4 n
                 , fbR1 = r1
                 , fbG1 = g1
                 })
       (FbFSMI { fbLt24Ready = ready
               , fbLt24DOut = dat
               , fbPixel1 = pixel1
               , fbPixel2 = pixel2
               , fbPixelColour = pixelColour
               })
    | not ready = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbRead5 (n+1)
                      , fbPixel2Buf = pixel2
                      , fbR2 = r2
                      }
                  , fbFSMO2 { fbAddrMode = FbScratchpad n
                            , fbMyRamDIn = pixel1Eff
                            , fbMyRamWrEn = True
                            , fbPixelVal = pixel1
                            })
    where
        pixel1Eff = if pixel1 == 0 then
                      oldPixelColour
                    else
                      pixelColour
        oldPixelColour = pal555To16bpp (r1, g1, b1)
        b1 = pal6bTo5b $ resize $ dat `shiftR` 10
        r2 = pal6bTo5b $ resize $ dat `shiftR` 2

{- 
 - Wait for third read to be accepted.
 - If we've processed the whole batch of 128 pixels, don't queue another
 - action, otherwise queue the next read.
 -}
fbFSM2 s@(FbFSMS { fbState = FbRead5 n })
       (FbFSMI { fbLt24Ready = ready })
    | ready     = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbRead6 n
                      , fbMyActionS = nextAction
                      }
                  , fbFSMO2)
    where
        nextAction | n == 127  = LT24.NOP
                   | otherwise = LT24.ReadFM

{- 
 - Wait for third read to complete, compute and store second pixel.
 - If we've processed the whole batch of 128 pixels, continue to the write
 - phase. Otherwise, continue with the next read.
 -}
fbFSM2 s@(FbFSMS { fbState = FbRead6 n
                 , fbPixel2Buf = pixel2
                 , fbR2 = r2
                 })
       (FbFSMI { fbLt24Ready = ready
               , fbLt24DOut = dat
               , fbPixelColour = pixelColour
               })
    | not ready = (s, fbFSMO2)
    | otherwise = ( s { fbState = state'
                      }
                  , fbFSMO2 { fbNextCoords = True
                            , fbAddrMode = FbScratchpad n
                            , fbMyRamDIn = pixel2Eff
                            , fbMyRamWrEn = True
                            , fbPixelVal = pixel2
                            })
    where
        state' | n == 127  = FbWriteCmd1
               | otherwise = FbRead1 (n + 1)
        pixel2Eff = if pixel2 == 0 then
                      oldPixelColour
                    else
                      pixelColour
        oldPixelColour = pal555To16bpp (r2, g2, b2)
        g2 = resize $ dat `shiftR` 10
        b2 = pal6bTo5b $ resize $ dat `shiftR` 2

-- Prepare blockram read
fbFSM2 s@(FbFSMS { fbState = FbWriteCmd1 }) i
    = ( s { fbState = FbWriteCmd2
          }
      , fbFSMO2 { fbAddrMode = FbScratchpad 0 })

-- Issue write command
fbFSM2 s@(FbFSMS { fbState = FbWriteCmd2 }) i
    = ( s { fbState = FbWrite1 0
          , fbMyActionS = LT24.Command
          , fbMyDInS = cRAMWR
          }
      , fbFSMO2 { fbAddrMode = FbScratchpad 0 })

-- Wait for cRAMWR to be accepted, then queue data write
fbFSM2 s@(FbFSMS { fbState = FbWrite1 n })
       (FbFSMI { fbLt24Ready = ready
               , fbMyRamDOut = ramOut
               })
    | ready     = (s, fbFSMO2 { fbAddrMode = FbScratchpad n })
    | otherwise = (s { fbState = FbWrite2 n
                     , fbMyActionS = LT24.Write
                     , fbMyDInS = ramOut
                     }
                  , fbFSMO2 { fbAddrMode = FbScratchpad (n+1) })

-- Wait for cRAMWR to complete
fbFSM2 s@(FbFSMS { fbState = FbWrite2 n })
       (FbFSMI { fbLt24Ready = ready })
    | not ready = (s, fbFSMO2 { fbAddrMode = FbScratchpad (n+1) })
    | otherwise = (s { fbState = state' }
                  , fbFSMO2 { fbAddrMode = FbScratchpad (n+1) })
    where
        state' | n == 127  = FbFinish1
               | otherwise = FbWrite1 (n+1)

{- 
 - Wait for final write to be accepted.
 - If we're not done yet, queue read command.  Otherwise, finish up (fbMyDInS
 - will be ignored).
 -}
fbFSM2 s@(FbFSMS { fbState = FbFinish1 })
       (FbFSMI { fbLt24Ready = ready
               , fbCoordsDone = coordsDone
               })
    | ready     = (s, fbFSMO2)
    | otherwise = (s { fbState = FbFinish2
                     , fbMyActionS = action
                     , fbMyDInS = cRead_Memory_Continue
                     }
                  , fbFSMO2)

    where
        action | coordsDone = LT24.NOP
               | otherwise  = LT24.Command

{- 
 - Wait for final write to complete, then continue or close up shop.
 - If we haven't transferred all pixels yet, transfer next block
 - Otherwise, if another update is requested, start over at the top.
 - Finally, if no action is needed, return to idle.
 -}
fbFSM2 s@(FbFSMS { fbState = FbFinish2
                 , fbPageStartS = pageStartS
                 })
       (FbFSMI { fbPageStartI = pageStartI
               , fbDoUpdateF = doUpdate
               , fbLt24Ready = ready
               , fbCoordsDone = coordsDone
               })
    | not ready = (s, fbFSMO2)
    | otherwise = (s { fbState = state'
                     , fbPageStartS = pageStart'
                     , fbMyActionS = action
                     , fbMyDInS = cPASET
                     }
                  , fbFSMO2 { fbClearDU = clear })

    where
        state' | not coordsDone || doUpdate = FbSendPA1
               | otherwise                  = FbIdle

        action | not coordsDone || doUpdate = LT24.Command
               | otherwise                  = LT24.NOP

        pageStart' | coordsDone = pageStartI
                   | otherwise  = pageStartS + 2

        clear = coordsDone && doUpdate

