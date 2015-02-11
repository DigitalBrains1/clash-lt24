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

type IlSIndex = $(uToFit $ max (CS.ticksMinPeriod fClk 120e-3) 63)
data IlStep = IlL LT24.Action (Unsigned 16) | IlDoPalette5b | IlDoPalette6b
            | IlWait IlSIndex

initSteps =  IlL LT24.Reset   0
          :> IlL LT24.Command cMADCTL
          :> IlL LT24.Write   8             -- BGR subpixel order
          :> IlL LT24.Command cCOLMOD
          :> IlL LT24.Write   5             -- 16bpp MCU interface
          :> IlL LT24.Command cRGBSET
          :> IlDoPalette5b                  -- Red
          :> IlDoPalette6b                  -- Green
          :> IlDoPalette5b                  -- Blue
          :> IlL LT24.Command cSLPOUT
          :> IlWait $(CS.ticksMinPeriodTH fClk 120e-3)
          :> IlL LT24.Command cDISPON
          :> Nil

