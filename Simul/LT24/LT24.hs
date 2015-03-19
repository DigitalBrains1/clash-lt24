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

module Simul.LT24.LT24
       ( module Simul.LT24.LT24
       , module Simul.Common
       , module LT24.LT24
       , module Toolbox.FClk
       , module CLaSH.Prelude
       ) where

import CLaSH.Prelude

import Simul.Common
import LT24.LT24
import Toolbox.FClk

simulateLT24 :: [(Action, Unsigned 16, Unsigned 16)]
             -> [ ( Bool, Unsigned 16, Bit, Bit, Bit, Bit, Bit, Bit, Unsigned 16
                  , Bit)]

simulateLT24 = simulate (pack . lt24 . unpack)
             . (++ repeat (NOP, 0, 0))

data ELLT24 = Action LT24.LT24.Action | DIn (Unsigned 16) | LTDIn (Unsigned 16)

trELLT24 (action, din, ltdin) (Simul.LT24.LT24.Action action')
    = (action', din , ltdin )
trELLT24 (action, din, ltdin) (DIn din'      ) = (action , din', ltdin )
trELLT24 (action, din, ltdin) (LTDIn ltdin'  ) = (action , din , ltdin')
