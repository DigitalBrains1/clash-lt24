{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module LT24.Framebuffer where

import CLaSH.Prelude
import Control.Applicative

import qualified LT24.LT24 as LT24
import LT24.Init (lt24WithInit)
import LT24.Commands
import Toolbox.Blockram2p
import Toolbox.Misc

framebuffer (actionDaisy, dinDaisy, fbAddr, fbDin, fbWrEn, doUpdate, ltdin)
    = ( readyDaisy, fbDout, updateDone, lt24Dout, lcdOn, csx, resx, dcx
      , wrx, rdx, ltdout, oe)
    where

        (fbDout, pixel) = blockram2p d12 d12 d2 d2
                            (fbAddr, fbDin, fbWrEn, myRamAddr, signal 0
                            , signal False)

        (lt24Ready, lt24Dout, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe) =
            lt24WithInit (lt24Action, lt24Din, ltdin)

        (x,y, coordsDone) = (genCoords <^> (0, 0)) nextCoords
        myRamAddr  = ((ramAddr <$>) . pack) (x,y)
        -- Black, red, green, blue
        pixelColor = ($(v [ 0x1F :: Unsigned 16, 0x7E0, 0xF800, 0 ])!)
                     <$> pixel

        doUpdateF = tfoldD (||) False (doUpdate, clearDU)
        (readyDaisy, updateDone, clearDU, lt24Action, lt24Din, nextCoords)
            = (fbFSM <^> FbFSMS { fbState = FbIdle
                                , fbWaitState = FbWaitDone
                                , fbMyActionS = LT24.NOP
                                , fbMyDinS = 0
                                })
                ( actionDaisy, dinDaisy, doUpdateF, lt24Ready, coordsDone
                , pixelColor)

genCoords (x, y) nextCoords = ((x', y'), (x', y', coordsDone))
    where
        (x', y') | nextCoords = case (x, y) of
                                  (47, 63) -> (0  , 0  )
                                  (47, _ ) -> (0  , y+1)
                                  ( _, _ ) -> (x+1, y  )
                 | otherwise  = (x, y)
        coordsDone = (x', y') == (47, 63)

ramAddr :: (Unsigned 6, Unsigned 6)
          -> Unsigned 12
ramAddr (x,y) = fromBV $ toBV x <++> toBV y

data FbState = FbIdle | FbWrite | FbFinish
    deriving (Show, Eq)

data FbWaitState = FbWaitAccept | FbWaitDone
    deriving (Show, Eq)

data FbFSMS = FbFSMS
    { fbState :: FbState
    , fbWaitState :: FbWaitState
    , fbMyActionS :: LT24.Action
    , fbMyDinS :: Unsigned 16
    }

data FbFSMI = FbFSMI
    { fbState' :: FbState
    , fbActionDaisy :: LT24.Action
    , fbDinDaisy :: Unsigned 16
    , fbDoUpdateF :: Bool
    , fbLt24Ready :: Bool
    , fbCoordsDone :: Bool
    , fbPixelColor :: Unsigned 16
    , fbMyActionI :: LT24.Action
    , fbMyDinI :: Unsigned 16
    }

data FbFSMO1 = FbFSMO1
    { fbReadyDaisy :: Bool
    , fbLt24Action :: LT24.Action
    , fbLt24Din :: Unsigned 16
    }

data FbFSMO2 = FbFSMO2
    {  fbClearDU :: Bool
    , fbNextCoords :: Bool
    }

fbFSMO2 = FbFSMO2
    { fbClearDU = False
    , fbNextCoords = False    }

fbFSM s (actionDaisy, dinDaisy, doUpdateF, lt24Ready, coordsDone, pixelColor)
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
                   , fbPixelColor = pixelColor
                   , fbMyActionI = fbMyActionS s2'
                   , fbMyDinI = fbMyDinS s2'
                   }

        (s1', o1) = fbFSM1 s i
        (s2', o2) = fbFSM2 s i

-- Daisy chain handler
fbFSM1 s i@(FbFSMI { fbState' = FbIdle })
    = (s, FbFSMO1 { fbReadyDaisy = fbLt24Ready i
                  , fbLt24Action = fbActionDaisy i
                  , fbLt24Din = fbDinDaisy i
                  })
fbFSM1 s i = (s, FbFSMO1 { fbReadyDaisy = True
                         , fbLt24Action = fbMyActionI i
                         , fbLt24Din = fbMyDinI i
                         })

-- doUpdate handler
fbFSM2 s@(FbFSMS { fbState = FbIdle }) (FbFSMI { fbDoUpdateF = True
                                               , fbLt24Ready = False})
    = (s, fbFSMO2)
fbFSM2 s@(FbFSMS { fbState = FbIdle }) (FbFSMI { fbDoUpdateF = True
                                               , fbLt24Ready = True })
    = (s { fbState = FbWrite
         , fbWaitState = FbWaitAccept
         , fbMyActionS = LT24.Command
         , fbMyDinS = cRAMWR
         }
      , fbFSMO2 { fbClearDU = True })

fbFSM2 s@(FbFSMS { fbWaitState = FbWaitDone })
       (FbFSMI { fbLt24Ready = False })
    = (s, fbFSMO2)
fbFSM2 s@(FbFSMS { fbWaitState = FbWaitDone })
      (FbFSMI { fbLt24Ready = True })
    = (s { fbWaitState = FbWaitAccept }, fbFSMO2)
fbFSM2 s@(FbFSMS { fbWaitState = FbWaitAccept })
       (FbFSMI { fbLt24Ready = True })
    = (s, fbFSMO2)

fbFSM2 s@(FbFSMS { fbState = FbWrite }) i
    = (s { fbState = if fbCoordsDone i then FbFinish else FbWrite
         , fbWaitState = FbWaitDone
         , fbMyActionS = LT24.Write
         , fbMyDinS = fbPixelColor i
         }
      , fbFSMO2 { fbNextCoords = True })

fbFSM2 s@(FbFSMS { fbState = FbFinish }) i
    = (s { fbState = FbIdle }, fbFSMO2)

fbFSM2 s i = (s, fbFSMO2)
