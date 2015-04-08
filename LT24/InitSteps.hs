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
                                            -- row / column exchange,
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

