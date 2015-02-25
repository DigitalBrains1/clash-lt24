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

framebuffer (actionDaisy, dInDaisy, fbAddr, fbDIn, fbWrEn, doUpdate, ltdin)
    = ( readyDaisy, fbDout, updateDone, lt24DOut, lcdOn, csx, resx, dcx
      , wrx, rdx, ltdout, oe)
    where

        (fbDout, myRamDOut) = blockram2p d12 d9 d2 d16
                            (fbAddr, fbDIn, fbWrEn, myRamAddr, myRamDIn
                            , myRamWrEn)

        (lt24Ready, lt24DOut, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe) =
            lt24WithInit (lt24Action, lt24DIn, ltdin)

        (x,y, coordsDone) = (genCoords <^> (0, 0)) nextCoords
        myRamAddr  = ((ramAddr <$>) . pack) (x,y, addrMode)
        (pixel1, pixel2) = (unpack . (pixelLanes <$>) . pack) (x, myRamDOut)
        -- Green, yellow, red, blue
        pixelColor = ($(v [ 0x1F :: Unsigned 16, 0xF800, 0xFFE0, 0x7E0 ])!)
                     <$> pixelOut

        doUpdateF = tfoldD (||) False (doUpdate, clearDU)
        (readyDaisy, updateDone, clearDU, lt24Action, lt24DIn, nextCoords, pixelOut, addrMode, myRamDIn, myRamWrEn)
            = (fbFSM <^> FbFSMS { fbState = FbIdle
                                , fbStartAddress = True
                                , fbPixel2Buf = 0
                                , fbR1 = 0
                                , fbG1 = 0
                                , fbR2 = 0
                                , fbMyActionS = LT24.NOP
                                , fbMyDInS = 0
                                })
                ( actionDaisy, dInDaisy, doUpdateF, lt24Ready, lt24DOut
                , coordsDone, pixel1, pixel2, pixelColor, myRamDOut)

{-
genCoords (x, y) nextCoords = ((x', y'), (x', y', coordsDone))
    where
        (x', y') | nextCoords = case (x, y) of
                                  (63, 47) -> (0  , 0  )
                                  (63, _ ) -> (0  , y+1)
                                  ( _, _ ) -> (x+1, y  )
                 | otherwise  = (x, y)
        coordsDone = (x, y) == (63, 47)
-}

genCoords (x, y) nextCoords = ((x', y'), (x, y, coordsDone))
    where
        (x', y') | nextCoords = case (x, y) of
                                  (62, 47) -> (0  , 0  )
                                  (62, _ ) -> (0  , y+1)
                                  ( _, _ ) -> (x+2, y  )
                 | otherwise  = (x, y)
        coordsDone = (x, y) == (0, 0)

ramAddr :: (Unsigned 6, Unsigned 6, FbAddrMode)
          -> Unsigned 9
ramAddr (x,y, FbFramebuffer ) = fromBV $ toBV y <++> vtakeI (toBV x)
ramAddr (_,_, FbScratchpad n) = resize n + 384

pixelLanes :: (Unsigned 6, Unsigned 16)
           -> (Unsigned 2, Unsigned 2)
pixelLanes (x, pixelW) = (pixel1, pixel2)
    where
        pixel1 = fromBV
               $ (vunconcatI $ toBV pixelW)!(resize x :: Unsigned 3)
        pixel2 = fromBV
               $ (vunconcatI $ toBV pixelW)!(resize x + 1 :: Unsigned 3)

data FbState = FbIdle | FbStart | FbDiscardRead1 | FbDiscardRead2
             | FbDiscardRead3 | FbRead1 (Unsigned 7) | FbRead2 (Unsigned 7)
             | FbRead3 (Unsigned 7) | FbRead4 (Unsigned 7)
             | FbRead5 (Unsigned 7) | FbRead6 (Unsigned 7) | FbWriteCmd1
             | FbWriteCmd2  | FbWrite1 (Unsigned 7) | FbWrite2 (Unsigned 7)
             | FbFinish1 | FbFinish2
    deriving (Show, Eq)

data FbAddrMode = FbFramebuffer | FbScratchpad (Unsigned 7)

data FbFSMS = FbFSMS
    { fbState :: FbState
    , fbStartAddress :: Bool
    , fbPixel2Buf :: Unsigned 2
    , fbR1 :: Unsigned 5
    , fbG1 :: Unsigned 5
    , fbR2 :: Unsigned 5
    , fbMyActionS :: LT24.Action
    , fbMyDInS :: Unsigned 16
    }
    deriving (Show, Eq)

data FbFSMI = FbFSMI
    { fbState' :: FbState
    , fbActionDaisy :: LT24.Action
    , fbDInDaisy :: Unsigned 16
    , fbDoUpdateF :: Bool
    , fbLt24Ready :: Bool
    , fbLt24DOut :: Unsigned 16
    , fbCoordsDone :: Bool
    , fbPixel1 :: Unsigned 2
    , fbPixel2 :: Unsigned 2
    , fbPixelColor :: Unsigned 16
    , fbMyRamDOut :: Unsigned 16
    , fbMyActionI :: LT24.Action
    , fbMyDInI :: Unsigned 16
    }

data FbFSMO1 = FbFSMO1
    { fbReadyDaisy :: Bool
    , fbLt24Action :: LT24.Action
    , fbLt24DIn :: Unsigned 16
    }

data FbFSMO2 = FbFSMO2
    { fbClearDU :: Bool
    , fbNextCoords :: Bool
    , fbPixelOut :: Unsigned 2
    , fbAddrMode :: FbAddrMode
    , fbMyRamDIn :: Unsigned 16
    , fbMyRamWrEn :: Bool
    }

fbFSMO2 = FbFSMO2
    { fbClearDU = False
    , fbNextCoords = False
    , fbPixelOut = 0
    , fbAddrMode = FbFramebuffer
    , fbMyRamDIn = 0
    , fbMyRamWrEn = False
    }

fbFSM s
      (actionDaisy, dInDaisy, doUpdateF, lt24Ready, lt24DOut, coordsDone
      , pixel1, pixel2, pixelColor, myRamDOut)
    = (s', ( readyDaisy, updateDone, clearDU, lt24Action, lt24DIn
           , nextCoords, pixelOut, addrMode, myRamDIn, myRamWrEn))
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
        pixelOut = fbPixelOut o2
        addrMode =fbAddrMode o2
        myRamDIn = fbMyRamDIn o2
        myRamWrEn = fbMyRamWrEn o2

        i = FbFSMI { fbState' = fbState s2'
                   , fbActionDaisy = actionDaisy
                   , fbDInDaisy = dInDaisy
                   , fbDoUpdateF = doUpdateF
                   , fbLt24Ready = lt24Ready
                   , fbLt24DOut = lt24DOut
                   , fbCoordsDone = coordsDone
                   , fbPixel1 = pixel1
                   , fbPixel2 = pixel2
                   , fbPixelColor = pixelColor
                   , fbMyRamDOut = myRamDOut
                   , fbMyActionI = fbMyActionS s2'
                   , fbMyDInI = fbMyDInS s2'
                   }

        (s1', o1) = fbFSM1 s i
        (s2', o2) = fbFSM2 s i

-- Daisy chain handler
fbFSM1 s i@(FbFSMI { fbState' = FbIdle })
    = (s, FbFSMO1 { fbReadyDaisy = fbLt24Ready i
                  , fbLt24Action = fbActionDaisy i
                  , fbLt24DIn = fbDInDaisy i
                  })
fbFSM1 s i = (s, FbFSMO1 { fbReadyDaisy = True
                         , fbLt24Action = fbMyActionI i
                         , fbLt24DIn = fbMyDInI i
                         })

------ doUpdate handler ------

-- lt24 available and update requested; go!
fbFSM2 s@(FbFSMS { fbState = FbIdle }) (FbFSMI { fbDoUpdateF = True
                                               , fbLt24Ready = True })
    = ( s { fbState = FbStart
          , fbMyActionS = LT24.Command
          , fbMyDInS = cRAMRD
          , fbStartAddress = True
          }
      , fbFSMO2 { fbClearDU = True })

-- lt24 unavailable or no action needed; idle
fbFSM2 s@(FbFSMS { fbState = FbIdle }) i
    = (s, fbFSMO2)

{-
 - If previous state = FbIdle:
 -     Wait for acceptance of cRAMRD, then queue dummy read
 - If previous state = FbFinish2:
 -     Wait for acceptance of cRead_Memory_Continue, then queue dummy read
 -}
fbFSM2 s@(FbFSMS { fbState = FbStart })
       (FbFSMI { fbLt24Ready = ready })
    | ready     = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbDiscardRead1
                      , fbMyActionS = LT24.ReadFM
                      }
                  , fbFSMO2)

-- Wait for cRAMRD/cRead_Memory_Continue to complete
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

{-
 - Wait for first real read acceptance, queue second read
 -}
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

-- Wait for second read to complete, compute first pixel
fbFSM2 s@(FbFSMS { fbState = FbRead4 n
                 , fbR1 = r1
                 , fbG1 = g1
                 })
       (FbFSMI { fbLt24Ready = ready
               , fbLt24DOut = dat
               , fbPixel1 = pixel1
               , fbPixel2 = pixel2
               , fbPixelColor = pixelColor
               })
    | not ready = (s, fbFSMO2)
    | otherwise = ( s { fbState = FbRead5 (n+1)
                      , fbPixel2Buf = pixel2
                      , fbR2 = r2
                      }
                  , fbFSMO2 { fbAddrMode = FbScratchpad n
                            , fbMyRamDIn = pixel1Eff
                            , fbMyRamWrEn = True
                            , fbPixelOut = pixel1
                            })
    where
        pixel1Eff = if pixel1 == 0 then
                      oldPixelColor
                    else
                      pixelColor
        oldPixelColor = pal555To16bpp (r1, g1, b1)
        b1 = pal6bTo5b $ resize $ dat `shiftR` 10
        r2 = pal6bTo5b $ resize $ dat `shiftR` 2

-- Wait for third read to be accepted, queue next action
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

-- Wait for third read to complete, compute second pixel
fbFSM2 s@(FbFSMS { fbState = FbRead6 n
                 , fbPixel2Buf = pixel2
                 , fbR2 = r2
                 })
       (FbFSMI { fbLt24Ready = ready
               , fbLt24DOut = dat
               , fbPixelColor = pixelColor
               })
    | not ready = (s, fbFSMO2)
    | otherwise = ( s { fbState = state'
                      }
                  , fbFSMO2 { fbNextCoords = True
                            , fbAddrMode = FbScratchpad n
                            , fbMyRamDIn = pixel2Eff
                            , fbMyRamWrEn = True
                            , fbPixelOut = pixel2
                            })
    where
        state' | n == 127  = FbWriteCmd1
               | otherwise = FbRead1 (n + 1)
        pixel2Eff = if pixel2 == 0 then
                      oldPixelColor
                    else
                      pixelColor
        oldPixelColor = pal555To16bpp (r2, g2, b2)
        g2 = resize $ dat `shiftR` 10
        b2 = pal6bTo5b $ resize $ dat `shiftR` 2

-- Prepare blockram read
fbFSM2 s@(FbFSMS { fbState = FbWriteCmd1 }) i
    = ( s { fbState = FbWriteCmd2
          }
      , fbFSMO2 { fbAddrMode = FbScratchpad 0 })

-- Issue write command
fbFSM2 s@(FbFSMS { fbState = FbWriteCmd2
                 , fbStartAddress = startAddress
                 }) i
    = ( s { fbState = FbWrite1 0
          , fbMyActionS = LT24.Command
          , fbMyDInS = writeCmd
          , fbStartAddress = False
          }
      , fbFSMO2 { fbAddrMode = FbScratchpad 0 })
    where
        writeCmd | startAddress = cRAMWR
                 | otherwise    = cWrite_Memory_Continue

-- Wait for previous action to be accepted, then queue data write
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

-- Wait for previous action to complete
fbFSM2 s@(FbFSMS { fbState = FbWrite2 n })
       (FbFSMI { fbLt24Ready = ready })
    | not ready = (s, fbFSMO2 { fbAddrMode = FbScratchpad (n+1) })
    | otherwise = (s { fbState = state' }
                  , fbFSMO2 { fbAddrMode = FbScratchpad (n+1) })
    where
        state' | n == 127  = FbFinish1
               | otherwise = FbWrite1 (n+1)

{- 
 - Wait for final write to be accepted
 - If we're not done yet, queue read command
 - Otherwise, finish up (fbMyDInS will be ignored)
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

-- Wait for final write to complete, then continue or close up shop
fbFSM2 s@(FbFSMS { fbState = FbFinish2 })
       (FbFSMI { fbLt24Ready = ready
               , fbCoordsDone = coordsDone
               })
    | not ready = (s, fbFSMO2)
    | otherwise = (s { fbState = state' }
                  , fbFSMO2)

    where
        state' | coordsDone = FbIdle
               | otherwise  = FbStart

