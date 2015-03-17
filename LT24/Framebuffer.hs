{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module LT24.Framebuffer
       (framebuffer
       ) where

import CLaSH.Prelude
import Control.Applicative

import qualified LT24.LT24 as LT24
import LT24.Init (lt24WithInit)
import LT24.Commands
import Toolbox.Blockram2p
--import Simul.Toolbox.Blockram2p_2_2
import Toolbox.Misc

{-
 - Framebuffer interface to LT24; 4 colours
 -
 - This CλaSH component maintains a 64x48 framebuffer with 2 bpp (4 colours).
 - When requested, this frame is sent to the LT24 display. The caller is
 - responsible for setting up the controller to place this 64x48 framebuffer
 - somewhere on the 320x240 display (through cPASET and cCASET).
 -
 - As with LT24.Init, access to the display controller (through LT24.LT24) is
 - daisy-chained.
 -
 - Inputs:
 -   `actionDaisy`: Daisy-chained actions for LT24.LT24
 -   `dinDaisy`: The same for data
 -   `fbAddr`, `fbDin`, `fbWrEn`: Blockram access for the caller. See
 -       Toolbox.Blockram2p for discussion of the signals; these are connected
 -       to port A of the 2-port blockram. It provides access to 3072 words of
 -       2 bits. Each word is one pixel. Given a coordinate (x,y) in the range
 -       (0,0) to (63,47), the 6 least significant bits of the address are the
 -       x coordinate and the 6 most significant are the y coordinate. This
 -       means coordinates map to addresses as:
 -         coordinate  address
 -         ( 0,  0)    0
 -         (63,  0)    63
 -         ( 0,  1)    64
 -         ( 1,  1)    65
 -         (63,  1)    127
 -         ( 0,  2)    128
 -         (63, 47)    3072
 -
 -       The caller is free to read and write from and to this memory as it
 -       sees fit. Once it requests an update, this CλaSH component will use
 -       its own port to the 2-port blockram to read out all the pixels in
 -       address order and send them to the LT24 controller. Since it's a true
 -       2-port blockram, further access by the caller is certainly possible.
 -
 -       There is an important caveat: Because of the nature of the blockram,
 -       it takes 3 cycles for a write to a pixel to become visible on a read.
 -       Depending on the clock frequency of the design, the first pixel is
 -       read minimally 2 clock cycles after `doUpdate` is asserted. This means
 -       that if you assert `doUpdate` in the same cycle as writing the pixel
 -       at coordinate (0,0), and the clock frequency is low enough, the old
 -       pixel value will be sent to the LT24 display. If this is a possibility
 -       with your design, you're better off asserting `doUpdate` in the cycle
 -       /after/ you've written the last pixel, to prevent this situation. If
 -       you're certain coordinate (0,0) is never the last to be written, you
 -       can assert `doUpdate` in the same cycle as your last write.
 -
 -       Note that since the blockram is actually allocated as 4096 words, you
 -       could use the last 1024 words for something other than the
 -       framebuffer. It would not interfere with this CλaSH component, which
 -       only reads the first 3072 words.
 -   `doUpdate`: Make True to request an update. If you request another update
 -       before `updateDone` is asserted to indicate completion, another update
 -       is immediately done after the current one completes. This means that
 -       after you've written to the framebuffer, you can assert `doUpdate` and
 -       wait for completion regardless of whether an update is currently
 -       active. It does waste bandwidth, because an ongoing update is not
 -       aborted, even though the data will be subsequently overwritten by the
 -       next update.
 -   `pixelColour`: Part of the palette interface. The other part is the output
 -       `pixelVal`. To convert a 2-bit colour value from the framebuffer to a
 -       16-bit colour value for the LT24 display, this CλaSH component will
 -       output the 2-bit colour value it currently needs on `pixelVal`. It
 -       expects the corresponding 16-bit colour value back
 -       *in the same clock cycle*, so as a combinatorial circuit. The thus
 -       acquired 16-bit value is passed verbatim to the LT24 display.
 -   `ltdin`: See LT24.LT24.
 -
 - Outputs:
 -   `readyDaisy`: Daisy-chained ready signal.
 -   `fbDout`: Blockram access for the caller: data read from the blockram. See
 -       blockram inputs above for more discussion.
 -   `updateDone`: Will be True for one clock cycle when the LT24 display was
 -       succesfully updated with the new framebuffer data. When `doUpdate` was
 -       asserted multiple times, you still will just get one `updateDone`
 -       back.  This makes it easier to work with: you can simply assert
 -       `doUpdate` and wait for `updateDone` without consideration for whether
 -       there was already an update in progress.
 -   `pixelVal`: See `pixelColour` input.
 -   `lt24Dout`, `lcdOn`, `csx`, `resx`, `dcx`, `wrx`, `rdx`, `ltdout`, `oe`:
 -       See LT24.LT24.
 -
 - Note that this version of `framebuffer` is agnostic about coordinates; you
 - could create a 128x24 frame or a 16*192 one, for example. This `framebuffer`
 - implementation just transfers 3072 pixels to the display with the cRAMWR
 - command. The RMW variant in LT24.FramebufferRMW however, unfortunately has
 - to know about coordinates.
 -}
framebuffer (actionDaisy, dinDaisy, fbAddr, fbDin, fbWrEn, doUpdate
            , pixelColour, ltdin)
    = ( readyDaisy, fbDout, updateDone, pixelVal, lt24Dout, lcdOn, csx, resx
      , dcx, wrx, rdx, ltdout, oe)
    where

        (fbDout, pixelVal) = blockram2p d12 d12 d2 d2
                               (fbAddr, fbDin, fbWrEn, myRamAddr, signal 0
                               , signal False)

        (lt24Ready, lt24Dout, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe) =
            lt24WithInit (lt24Action, lt24Din, ltdin)

        (x,y, coordsDone) = (genCoords <^> (0, 0)) nextCoords
        myRamAddr  = ((ramAddr <$>) . pack) (x,y)

        -- Fold over time with "or", in other words: Output True if `doUpdate`
        -- was at some time True since the last `clearDU`.
        doUpdateF = tfoldD (||) False (doUpdate, clearDU)
        (readyDaisy, updateDone, clearDU, lt24Action, lt24Din, nextCoords)
            = (fbFSM <^> FbFSMS { fbState = FbIdle
                                , fbWaitState = FbWaitDone
                                , fbMyActionS = LT24.NOP
                                , fbMyDinS = 0
                                })
                ( actionDaisy, dinDaisy, doUpdateF, lt24Ready, coordsDone
                , pixelColour)

-- Step through all coordinates, stepping when `nextCoords` = True.
-- Assert `coordsDone` one clock cycle after last coordinate was produced.
genCoords (x, y) nextCoords = ((x', y'), (x', y', coordsDone))
    where
        (x', y') | nextCoords = case (x, y) of
                                  (63, 47) -> (0  , 0  )
                                  (63, _ ) -> (0  , y+1)
                                  ( _, _ ) -> (x+1, y  )
                 | otherwise  = (x, y)
        coordsDone = (x, y) == (63, 47)

-- Generate a linear address from a coordinate pair (x,y)
ramAddr :: (Unsigned 6, Unsigned 6)
          -> Unsigned 12
ramAddr (x,y) = fromBV $ toBV y <++> toBV x

data FbState = FbIdle | FbWrite | FbFinish1 | FbFinish2
    deriving (Show, Eq)

data FbWaitState = FbWaitAccept | FbWaitDone
    deriving (Show, Eq)

-- (S)tate for fbFSM
data FbFSMS = FbFSMS
    { fbState :: FbState
    , fbWaitState :: FbWaitState
    , fbMyActionS :: LT24.Action
    , fbMyDinS :: Unsigned 16
    }
    deriving (Show, Eq)

-- (I)nput for fbFSM
data FbFSMI = FbFSMI
    { fbState' :: FbState
    , fbActionDaisy :: LT24.Action
    , fbDinDaisy :: Unsigned 16
    , fbDoUpdateF :: Bool
    , fbLt24Ready :: Bool
    , fbCoordsDone :: Bool
    , fbPixelColour :: Unsigned 16
    , fbMyActionI :: LT24.Action
    , fbMyDinI :: Unsigned 16
    }

-- (O)utput for fbFSM1
data FbFSMO1 = FbFSMO1
    { fbReadyDaisy :: Bool
    , fbLt24Action :: LT24.Action
    , fbLt24Din :: Unsigned 16
    }

-- (O)utput for fbFSM2
data FbFSMO2 = FbFSMO2
    {  fbClearDU :: Bool
    , fbNextCoords :: Bool
    }

-- Default values for outputs
fbFSMO2 = FbFSMO2
    { fbClearDU = False
    , fbNextCoords = False    }

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
fbFSM s (actionDaisy, dinDaisy, doUpdateF, lt24Ready, coordsDone, pixelColour)
    = (s', (readyDaisy, updateDone, clearDU, lt24Action, lt24Din
           , nextCoords))
    where
        s' = s2'
        readyDaisy = fbReadyDaisy o1
        lt24Din = fbLt24Din o1
        nextCoords = fbNextCoords o2
        updateDone = fbState s2' == FbIdle
        clearDU = fbClearDU o2
        lt24Action = fbLt24Action o1

        i = FbFSMI { fbState' = fbState s2'
                   , fbActionDaisy = actionDaisy
                   , fbDinDaisy = dinDaisy
                   , fbDoUpdateF = doUpdateF
                   , fbLt24Ready = lt24Ready
                   , fbCoordsDone = coordsDone
                   , fbPixelColour = pixelColour
                   , fbMyActionI = fbMyActionS s2'
                   , fbMyDinI = fbMyDinS s2'
                   }

        o1 = fbFSM1 i
        (s2', o2) = fbFSM2 s i

-- Daisy chain handler: when we're not using the LT24, let the caller access it
fbFSM1 i@(FbFSMI { fbState' = FbIdle })
    = FbFSMO1 { fbReadyDaisy = fbLt24Ready i
              , fbLt24Action = fbActionDaisy i
              , fbLt24Din = fbDinDaisy i
              }
fbFSM1 i = FbFSMO1 { fbReadyDaisy = True
                   , fbLt24Action = fbMyActionI i
                   , fbLt24Din = fbMyDinI i
                   }

{-
 - doUpdate handler
 -
 - This is a state machine with a slightly convoluted pattern match order on
 - the state. State is defined by two variables, fbState and fbWaitState. Major
 - steps are in fbState, with fbWaitState synchronizing to LT24.LT24 in
 - between. It is somewhat hard to follow, and got really difficult once this
 - code was expanded for LT24.FramebufferRMW, where I thus got rid of it. When
 - reading this code and determining which clause triggers in the next state,
 - it is necessary to start pattern matching from the top again, as the flow
 - bounces around, as it were.
 -
 - Because the order is so convoluted, the documentation of each clause says
 - "Next clause: X." where X is the number that starts the comment block for
 - each clause.
 -
 - Actions for the LT24.LT24 are queued as soon as the previous action is
 - accepted. This way, maximum throughput is achieved even on low clock
 - frequencies.
 -
 - The states FbWrite and FbFinish1 share the same synchronization mechanism
 - that keeps them in step with LT24.LT24. The states FbIdle and FbFinish2 are
 - handled separately, which is why they come first in the pattern match.
 -}

{-
 - 1.
 - The state machine starts in FbIdle, waiting for an update request. To
 - process the update, LT24.LT24 needs to have finished whatever it was doing
 - before through the daisy chain. Once it is idle and an update is requested:
 - Clear `doUpdateF`. Queue cRAMWR command to the LT24 display. The next state
 - waits for acceptance of that action and continues to FbWrite where the
 - pixels will be transferred.
 -
 - Next clause: 4, followed by 5.
 -}
fbFSM2 s@(FbFSMS { fbState = FbIdle }) (FbFSMI { fbLt24Ready = False})
    = (s, fbFSMO2)
fbFSM2 s@(FbFSMS { fbState = FbIdle }) (FbFSMI { fbDoUpdateF = True
                                               , fbLt24Ready = True })
    = ( s { fbState = FbWrite
          , fbWaitState = FbWaitAccept
          , fbMyActionS = LT24.Command
          , fbMyDinS = cRAMWR
          }
      , fbFSMO2 { fbClearDU = True })

{-
 - 2.
 - Wait for LT24.LT24 to finish, then go idle.
 - Next clause: 1.
 -}
fbFSM2 s@(FbFSMS { fbState = FbFinish2 })
       (FbFSMI { fbLt24Ready = False })
    = (s, fbFSMO2)
fbFSM2 s@(FbFSMS { fbState = FbFinish2 })
       (FbFSMI { fbLt24Ready = True })
    = (s { fbState = FbIdle }, fbFSMO2)

{-
 - 3.
 - Wait for currently processing action to complete.
 - Next clause: 4.
 -}
fbFSM2 s@(FbFSMS { fbWaitState = FbWaitDone })
       (FbFSMI { fbLt24Ready = False })
    = (s, fbFSMO2)
fbFSM2 s@(FbFSMS { fbWaitState = FbWaitDone })
      (FbFSMI { fbLt24Ready = True })
    = (s { fbWaitState = FbWaitAccept }, fbFSMO2)

{-
 - 4.
 - Wait for queued action to be accepted, then fall through to fbState matching
 - below.
 -}
fbFSM2 s@(FbFSMS { fbWaitState = FbWaitAccept })
       (FbFSMI { fbLt24Ready = True })
    = (s, fbFSMO2)

{-
 - 5.
 - Queue a pixel write and request the next pixel from the RAM.
 - Once all pixels have been written:
 -     Wait for current action to complete and this newly queued action to be
 -     accepted, and go to FbFinish1. (Note the RAM is already set for
 -     coordinate (0,0) for our next run.)
 -     Next clauses: 3, 4, then 6.
 - If we haven't written all pixels yet:
 -     Wait for current action to complete and this newly queued action to be
 -     accepted, and queue the next pixel.
 -     Next clauses: 3, 4, then 5 (this one).
 -}
fbFSM2 s@(FbFSMS { fbState = FbWrite }) i
    = ( s { fbState = if fbCoordsDone i then FbFinish1 else FbWrite
          , fbWaitState = FbWaitDone
          , fbMyActionS = LT24.Write
          , fbMyDinS = fbPixelColour i
          }
      , fbFSMO2 { fbNextCoords = True })

{-
 - 6.
 - If another `doUpdate` request was made in the mean time:
 -     Clear `doUpdateF`. Immediately queue cRAMWR command. Wait for current
 -     action to complete and cRAMWR to be accepted, then go to FbWrite.
 -     Next clauses: 3, 4, then 5.
 - If no new update is requested:
 -     Don't queue a new action (i.e., set to LT24.NOP), and go to FbFinish2.
 -     Next clause: 2.
 -}
fbFSM2 s@(FbFSMS { fbState = FbFinish1 })
       (FbFSMI { fbDoUpdateF = True })
    = ( s { fbState = FbWrite
          , fbWaitState = FbWaitDone
          , fbMyActionS = LT24.Command
          , fbMyDinS = cRAMWR
          }
      , fbFSMO2 { fbClearDU = True })
fbFSM2 s@(FbFSMS { fbState = FbFinish1 }) i
    = ( s { fbState = FbFinish2
          , fbMyActionS = LT24.NOP}
      , fbFSMO2)
