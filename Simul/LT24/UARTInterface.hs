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

module Simul.LT24.UARTInterface
       ( module Simul.LT24.UARTInterface
       , module Simul.Common
       , module LT24.UARTInterface
       , module CLaSH.Prelude
       ) where

import CLaSH.Prelude

import Simul.Common
import LT24.UARTInterface
import qualified LT24.LT24 as LT24

simulateCommandIf :: [((Bool, Unsigned 8), Bool, Bool, Bool, Unsigned 16)]
                  -> [( (Bool, Unsigned 8, LT24.Action, Unsigned 16, Bit)
                      , ((Bool, Unsigned 8), Bool, Bool, Bool, Unsigned 16))]

simulateCommandIf = (\i -> zip (simulate (pack . commandIf . unpack) i) i)
                  . (++ repeat ((False, 0), False, True, True, 0))

data ELCI = RX (Unsigned 8) | RXV Bool | TXDone Bool | Ready Bool
          | DOut (Unsigned 16)

trELCI (rxoF, rxoV, txDone, ready, dout) (RX d          )
    = ((False, d), rxoV , txDone , ready , dout )
trELCI (rxoF, rxoV, txDone, ready, dout) (RXV rxoV'     )
    = (rxoF      , rxoV', txDone , ready , dout )
trELCI (rxoF, rxoV, txDone, ready, dout) (TXDone txDone')
    = (rxoF      , rxoV , txDone', ready , dout )
trELCI (rxoF, rxoV, txDone, ready, dout) (Ready ready'  )
    = (rxoF      , rxoV , txDone , ready', dout )
trELCI (rxoF, rxoV, txDone, ready, dout) (DOut dout'    )
    = (rxoF      , rxoV , txDone , ready , dout')

simulatePassCommand :: [(Unsigned 8, Unsigned 16, Bool, Bool)]
                    -> [( (LT24.Action, Unsigned 16, Bit, Bool)
                        , (Unsigned 8, Unsigned 16, Bool, Bool))]

simulatePassCommand = (\i -> zip (simulate pc i) i)
                    . (++ repeat (0, 0, False, True))
    where
        pc = (pack . (passCommand <^> (PCIdle, 0, 0, L)) . unpack)

passCommandStimulus1 ::[(Unsigned 8, Unsigned 16, Bool, Bool)]

passCommandStimulus1
    = [ ( 0 , 0 , True , True )
      , ( 1 , 11 , True , False )
      , ( 1 , 11 , True , False )
      , ( 1 , 11 , True , True )
      , ( 2 , 5 , True , False )
      , ( 2 , 8 , True , True )
      , ( 1 , 58 , True , False )
      , ( 1 , 58 , True , True )
      , ( 2 , 5 , True , True )
      , ( 2 , 5 , True , False )
      , ( 2 , 5 , True , False )
      , ( 2 , 5 , True , True )
      , ( 0 , 0 , False , True )
      , ( 0 , 0 , False , False )
      , ( 0 , 0 , False , True )
      , ( 0 , 0 , False , True )
      , ( 1 , 45 , True , True )
      , ( 0 , 0 , False , False )
      ]

passCommandStimulus1Expected :: [( (LT24.Action, Unsigned 16, Bit, Bool)
                                 , (Unsigned 8, Unsigned 16, Bool, Bool))]

passCommandStimulus1Expected
    = [ ( ( LT24.Reset , 0 , L , True ) , ( 0 , 0 , True , True ) )
      , ( ( LT24.Command , 11 , L , False ) , ( 1 , 11 , True , False ) )
      , ( ( LT24.Command , 11 , L , False ) , ( 1 , 11 , True , False ) )
      , ( ( LT24.Command , 11 , L , True ) , ( 1 , 11 , True , True ) )
      , ( ( LT24.Write , 5 , L , False ) , ( 2 , 5 , True , False ) )
      , ( ( LT24.Write , 5 , L , True ) , ( 2 , 8 , True , True ) )
      , ( ( LT24.Command , 58 , L , False ) , ( 1 , 58 , True , False ) )
      , ( ( LT24.Command , 58 , L , True ) , ( 1 , 58 , True , True ) )
      , ( ( LT24.Command , 58 , L , False ) , ( 2 , 5 , True , True ) )
      , ( ( LT24.Write , 5 , L , False ) , ( 2 , 5 , True , False ) )
      , ( ( LT24.Write , 5 , L , False ) , ( 2 , 5 , True , False ) )
      , ( ( LT24.Write , 5 , L , True ) , ( 2 , 5 , True , True ) )
      , ( ( LT24.Write , 5 , L , False ) , ( 0 , 0 , False , True ) )
      , ( ( LT24.NOP , 5 , L , False ) , ( 0 , 0 , False , False ) )
      , ( ( LT24.NOP , 5 , L , False ) , ( 0 , 0 , False , True ) )
      , ( ( LT24.NOP , 5 , L , False ) , ( 0 , 0 , False , True ) )
      , ( ( LT24.Command , 45 , L , True ) , ( 1 , 45 , True , True ) )
      , ( ( LT24.NOP , 45 , L , False ) , ( 0 , 0 , False , False ) )
      , ( ( LT24.NOP , 45 , L , False ) , ( 0 , 0 , False , True ) )
      , ( ( LT24.NOP , 45 , L , False ) , ( 0 , 0 , False , True ) )
      ]

