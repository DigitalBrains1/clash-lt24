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

module LT24.UARTInterface where

import CLaSH.Prelude
import Control.Applicative

import qualified LT24.LT24 as LT24
import LT24.Init
import qualified Toolbox.Serial as Serial
import qualified Toolbox.ClockScale as CS
import qualified Toolbox.FIFO as FIFO
import Toolbox.FClk

{-
 - Create an UART interface to the LT24
 -
 - UART protocol:
 -
 - The PC issues a request, and gets a response from the FPGA.  All requests
 - and responses are 3 bytes long and have the same format. The first byte is
 - the command and the remaining bytes are the associated data, little-endian
 - format:
 -
 - c dl dh
 -          - c = Command, d = (dh << 8) + dl = 16-bit data
 -
 - A positive response echoes the command. If there is no data to return, the
 - data from the request is echoed literally.
 -
 - If a command doesn't have any associated data, the data in the request is
 - ignored. If a command doesn't have associated data and doesn't return data
 - either, the data from the request is echoed literally in the response.
 -
 - Note that the data from the request is simply always passed to the `din`
 - argument of LT24.lt24, and it is that component which doesn't use `din` for
 - commands that don't have any associated data.
 -
 - Because responses echo parts of the request, communication issues are
 - hopefully often spotted and not wrongfully attributed to something else.
 -
 - The command byte can be one of:
 -
 - 0 -> LT24.Reset
 - 1 -> LT24.Command
 - 2 -> LT24.Write
 - 3 -> LT24.ReadFM
 - 4 -> LT24.ReadID
 - 5 -> Hold FIFO; up to 16 commands are queued until the FIFO is released
 - 6 -> Release FIFO; execute the queued commands as quickly as possible
 - 7 -> GPIO: set GPIO outputs, read GPIO inputs
 -
 - Examples (> is data from PC to FPGA, < is data from FPGA to PC, all numbers
 - hexadecimal):
 -
 - > 00 34 12       - Reset; dummy data 0x1234
 - < 00 34 12       - Reset completed; echoing dummy data
 - > 01 2C 00       - LT24.Command: cRAMWR Memory Write
 - < 01 2C 00       - LT24.Command completed; echoing value cRAMWR
 - > 03 34 12       - Read frame memory; dummy data ignored
 - < 03 EF BE       - Data from frame memory: 0xBEEF
 - > 07 01 00       - Set the "GPIO" output pin high (supports up to 32 pins)
 - < 07 00 00       - Returns the GPIO inputs; none yet, so always 0
 - > 05 00 00       - Hold commands (doesn't cause a response)
 - > 01 2C 00       - LT24.Command: cRAMWR (no response yet; queued only)
 - > 02 FE CA       - Write data: 0xCAFE (queued)
 - > 06 00 00       - Release commands: the two queued commands are executed
 - < 01 2C 00       - Response from LT24.Command
 - < 02 FE CA       - Response from write data
 -}

intfBare = intf LT24.lt24

{-
 - Create an UART interface, resetting the LT24 on powerup / reset
 -}
intfInited = intf lt24WithInit

intf lt24 i = o
    where
        o = ((combineOutput <$>) . pack)
              (gpioO, txd, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe)

        rxd = vhead <$> i
        ltdin = (fromBV . vtail) <$> i

        (ready, dout, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe) =
            lt24 (action, din, ltdin)

        tTick = ($(CS.staticAvgRate fClk 115200) <^> 1)
                  sCmd
        (sCmd, txDone, txd) = Serial.output (tTick, txiV, txi)
        rTick = ($(CS.staticAvgRate fClk (16*115200)) <^> 1)
                  (signal CS.Run)
        (rxoF, rxoV) = Serial.input (rTick, rxd)

        (txiV, txi, action, din, gpioO)
            = commandIf (rxoF, rxoV, txDone, ready, dout)

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

-- Command interface
commandIf (rxoF, rxoV, txDone, ready, dout) = (txiV, txi, action, din, gpioO)
    where
        (rxoFE, rxo) = unpack rxoF

        rxVec = (groupBytes <^> (vcopyI 0)) (rxo, rxoV)
        cValid = (countBytes <^> 2) rxoV

        (_, cFifoEmpty, _, cFifoO)
            = (FIFO.fifo <^> (0, 0, vcopy d16 (0, 0)))
                (cFifoI, cFifoWr, cFifoRd)
        cFifoI = pack (rxC, rxDW)
        (cFifoEn, cFifoWr) = (managFIFO <^> True) (rxC, cValid)
        doCmd = (((\(cFifoEmpty, cFifoEn) -> cFifoEn && not cFifoEmpty) <$>)
                 . pack) (cFifoEmpty, cFifoEn)

        (cFifoOCmd, cFifoOD) = unpack cFifoO
        (action, din, gpioO, cFifoRd)
            = (passCommand <^> (PCIdle, 255, 0, L))
                (cFifoOCmd, cFifoOD, doCmd, ready)

        (rxC, rxDW) = (unpack . (splitRxVec <$>) . pack) rxVec

        (_, rFifoEmpty, _, rFifoO)
            = (FIFO.fifo <^> (0, 0, vcopy d16 (0, 0)))
                (rFifoI, rFifoWr, rFifoRd)
        (rFifoI, rFifoWr) = (returnData <^> RDState 0 0 0)
                              (cFifoOCmd, cFifoOD, cFifoRd, ready, dout)
        (txi, txiV, rFifoRd) = (serResp <^> (0 :: Unsigned 2))
                                 (rFifoO, rFifoEmpty, txDone)

-- Split the 3 received bytes into one byte command and one 16-bit data word
splitRxVec i = (cmd, d)
    where
        cmd = vlast i
        d   = (fromBV . vconcat . vmap toBV . vinit) i

-- Keeps the last 3 received bytes from the UART
--           (rxo, rxoV  )
groupBytes s (_  , False ) = (s ,s )
groupBytes s (rxo, True  ) = (s',s')
    where
        s' = rxo +>> s

-- True every three bytes (i.o.w., when a complete command has been received)
countBytes :: Unsigned 2
           -> Bool
           -> (Unsigned 2, Bool)

--         s rxoV
countBytes s False = (s , False)
countBytes 0 True  = (2 , True )
countBytes s True  = (s', False)
    where
        s' = s - 1

-- Handle the FIFO commands, and store the other commands in the FIFO
--        fifoEn (rxC, cValid) = (fifoEn', (fifoEn', fifoWr))
managFIFO fifoEn (rxC, False ) = (fifoEn , (fifoEn , False ))
managFIFO _      (  5, True  ) = (False  , (False  , False ))
managFIFO _      (  6, True  ) = (True   , (True   , False ))
managFIFO fifoEn (  _, True  ) = (fifoEn , (fifoEn , True  ))

data PCState = PCIdle | PCWaitAccept | PCWaitReady

{-
 - Pass commands to LT24.lt24
 -
 - Wait for 'doCmd' indicating a command is available from the FIFO for
 - execution.
 - Then pass the command and the data to LT24.lt24, and wait for that component
 - to signal acceptance and then completion.
 -
 - To get the quickest response when bursting data (which is useful for testing
 - timings), back-to-back commands skip the Idle phase. However, this won't
 - work for GPIO access because returnData would get confused. So in the case
 - of a GPIO command, the state first goes back to Idle.
 -}

passCommand :: (PCState, Unsigned 8, Unsigned 16, Bit)
            -> (Unsigned 8, Unsigned 16, Bool, Bool)
            -> ( (PCState, Unsigned 8, Unsigned 16, Bit)
               , (LT24.Action, Unsigned 16, Bit, Bool))

{-
 - Translates commands (command bytes to LT24.Action), the rest is handled by
 - passCommand'.
 -}
passCommand (st, cbuf, dbuf, gpioOB) (cmd, d, doCmd, ready)
    = ( (st', cbuf', dbuf', gpioO) , (action, din, gpioO, fifoRd))
    where
        (st', cbuf', dbuf', gpioO, fifoRd)
            = passCommand' st cbuf dbuf gpioOB cmd d doCmd ready
        action = case cbuf' of
                   0 -> LT24.Reset
                   1 -> LT24.Command
                   2 -> LT24.Write
                   3 -> LT24.ReadFM
                   4 -> LT24.ReadID
                   _ -> LT24.NOP
        din = dbuf'

--           PCState      cbuf dbuf gpioOB cmd d doCmd ready
--    (PCState'    , cbuf', dbuf', gpioO , fifoRd)

-- Stay idle
passCommand' PCIdle       cbuf dbuf gpioOB cmd d False ready
    = (PCIdle      , cbuf , dbuf , gpioOB, False )

-- Handle GPIO command
passCommand' PCIdle       cbuf dbuf gpioOB 7   d True  ready
    = (PCIdle      , cbuf , dbuf , gpioO , True  )
    where
        gpioO = vexact d0 (toBV d)

-- Start LT24.lt24 action, pop command FIFO
passCommand' PCIdle       cbuf dbuf gpioOB cmd d True  ready
    = (PCWaitAccept, cmd  , d    , gpioOB, True  )

-- Wait for LT24.lt24 to accept action
passCommand' PCWaitAccept cbuf dbuf gpioOB cmd d doCmd True
    = (PCWaitAccept, cbuf , dbuf , gpioOB, False )

-- LT24.lt24 accepted, wait for completion. cbuf = 255 maps to LT24.NOP.
passCommand' PCWaitAccept cbuf dbuf gpioOB cmd d False False
    = (PCWaitReady , 255  , dbuf , gpioOB, False )

-- Exceptional handling for GPIO command: no shortcut
passCommand' PCWaitAccept cbuf dbuf gpioOB 7   d True  False
    = (PCWaitReady , 255  , dbuf , gpioOB, False )

-- Next command available, shortcut: offer next action already
passCommand' PCWaitAccept cbuf dbuf gpioOB cmd d True  False
    = (PCWaitReady , cmd  , d    , gpioOB, False )

-- No new command, wait for completion
passCommand' PCWaitReady  cbuf dbuf gpioOB cmd d doCmd False
    = (PCWaitReady , cbuf , dbuf , gpioOB, False )

-- Exceptional handling for GPIO command: no shortcut
passCommand' PCWaitReady  cbuf dbuf gpioOB 7   d True  True
    = (PCIdle      , cbuf , dbuf , gpioOB, False )

-- Action completed and new action queued; pop command FIFO
passCommand' PCWaitReady  cbuf dbuf gpioOB cmd d True  True
    = (PCWaitAccept, cbuf , dbuf , gpioOB, True  )

-- Action completed and no new command, go idle
passCommand' PCWaitReady  cbuf dbuf gpioOB cmd d False True
    = (PCIdle      , cbuf , dbuf , gpioOB, False )

data RDState = RDState
    { rdMode :: Unsigned 2
    , cmdBuf :: Unsigned 8
    , cmdDBuf :: Unsigned 16
    }

{-
 - Return a response to the PC
 -
 - Monitors the command FIFO to catch the start of a command, and then monitors
 - the `ready` line from LT24.lt24 to catch command completion and possibly a
 - result on `dout`. Pushes the result on the response FIFO.
 -}
returnData :: RDState
           -> (Unsigned 8, Unsigned 16, Bool, Bool, Unsigned 16)
           -> (RDState, ((Unsigned 8, Unsigned 16), Bool))

{- Template
returnData s@(RDState { rdMode = 0 }) (cmd, cmdD, cFifoRd, ready, dout) =
    (s                , (0 , 0 ), False ))
 -}

--         s                          (cmd, cmdD, cFifoRd, ready, dout)
--  (s'               , (rc, rd), rFifoWr))

-- Idle
returnData s@(RDState { rdMode = 0 }) (cmd, cmdD, False  , ready, dout) =
    (s               , ((0 , 0 ), False  ))

-- Read GPIO command (no inputs defined yet, so constant 0)
returnData s@(RDState { rdMode = 0 }) (7  , cmdD, True   , ready, dout) =
    (s               , ((7 , 0 ), True   ))

-- Start of command; capture command
returnData s@(RDState { rdMode = 0 }) (cmd, cmdD, True   , ready, dout) =
    (s'              , ((0 , 0 ), False  ))
    where
        s' = RDState 1 cmd cmdD

-- Wait for action acceptance
returnData s@(RDState { rdMode = 1 }) (cmd, cmdD, cFifoRd, True , dout) =
    (s               , ((0 , 0 ), False ))
returnData s@(RDState { rdMode = 1 }) (cmd, cmdD, cFifoRd, False, dout) =
    (s { rdMode = 2 }, ((0 , 0 ), False ))

--Wait for action completion (rdMode = 2)
returnData s                          (cmd, cmdD, cFifoRd, False, dout) =
    (s               , ((0 , 0 ), False ))
-- Back-to-back actions, capture next command and report current
returnData s                          (cmd, cmdD, True   , True , dout) =
    (s'              , ((rc, rd), True  ))
    where
        s' =RDState 1 cmd cmdD
        rc = cmdBuf s
        rd = case rc of
               -- Read data
               3 -> dout
               4 -> dout
               -- Echo request
               _ -> cmdDBuf s

-- No back-to-back, report result and go idle
returnData s                          (cmd, cmdD, False  , True , dout) =
    (s { rdMode = 0 }, ((rc, rd), True  ))
    where
        rc = cmdBuf s
        rd = case rc of
               -- Read data
               3 -> dout
               4 -> dout
               -- Echo request
               _ -> cmdDBuf s

{-
 - Serialize a response from the response FIFO to the serial port
 -
 - It splits the response in its three constituent bytes
 -}
--      s ((rc, rd), rFifoEmpty, txDone) = (s', (txi, txiV , rFifoRd))
serResp s ((rc, rd), _         , False ) = (s , (0  , False, False  ))
serResp s ((rc, rd), True      , _     ) = (0 , (0  , False, False  ))
serResp s ((rc, rd), False     , True  ) = (s', (txi, True , rFifoRd))
    where
        s' = case s of
               2 -> 0
               _ -> s + 1
        txi = case s of
                0 -> rc
                1 -> resize rd
                _ -> resize $ rd `shiftR` 8
        rFifoRd = s == 2

