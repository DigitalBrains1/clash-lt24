{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module LT24.InitSteps where

import CLaSH.Prelude
import Language.Haskell.TH

import qualified LT24.LT24 as LT24
import LT24.Commands
import qualified Toolbox.ClockScale as CS
import Toolbox.Misc
import Toolbox.FClk

type IlSIndex = $(uToFit $ max (CS.ticksMinPeriod fClk 120e-3) (320*240))
data IlStep = IlL LT24.Action (Unsigned 16) | IlDoPalette5b | IlDoPalette6b
            | IlWait IlSIndex | IlWipeFb

{-
 - These are the steps necessary to initialize the LT24's controller.
 -
 - It is initialized in a 16bpp mode with a direct colour mapping. The
 - coordinates are chosen such that if you have the LT24 module oriented with
 - the silkscreen text "terasIC LT24" upright, then the coordinates (x,y) are
 - (0,0) top-left, (319,0) top-right, (0,239) bottom-left and (319,239)
 - bottom-right.
 -}
initSteps =  IlL LT24.Reset   0
          :> IlL LT24.Command cMADCTL
          :> IlL LT24.Write   232           -- Alternate row address order,
                                            -- alternate column address order,
                                            -- row / column exchange
                                            -- BGR subpixel order
          :> IlL LT24.Command cCOLMOD
          :> IlL LT24.Write   5             -- 16bpp MCU interface
          :> IlL LT24.Command cRGBSET
          :> IlDoPalette5b                  -- Red
          :> IlDoPalette6b                  -- Green
          :> IlDoPalette5b                  -- Blue
          :> IlL LT24.Command cCASET        -- Since we exchanged rows and
          :> IlL LT24.Write   0             -- columns, we need to set the
          :> IlL LT24.Write   0             -- end addresses correctly
          :> IlL LT24.Write   1
          :> IlL LT24.Write   0x3F
          :> IlL LT24.Command cPASET
          :> IlL LT24.Write   0
          :> IlL LT24.Write   0
          :> IlL LT24.Write   0
          :> IlL LT24.Write   0xEF
          :> IlL LT24.Command cRAMWR        -- Clear the framebuffer
          :> IlWipeFb
          :> IlL LT24.Command cSLPOUT
          :> IlWait $(CS.ticksMinPeriodTH fClk 120e-3)
          :> IlL LT24.Command cDISPON
          :> Nil

