module LT24.UARTInterface where

import CLaSH.Prelude
import Control.Applicative

import qualified LT24.LT24 as LT24
import LT24.Init
import qualified Toolbox.Serial as Serial
import qualified Toolbox.ClockScale as CS
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
 - Examples (> is data from PC to FPGA, < is data from FPGA to PC, all numbers
 - hexadecimal):
 -
 - > 00 34 12       - Reset; dummy data 0x1234
 - < 00 34 12       - Reset completed; echoing dummy data
 - > 01 2C 00       - LT24.Command: cRAMWR Memory Write
 - < 01 2C 00       - LT24.Command completed; echoing value cRAMWR
 - > 03 34 12       - Read frame memory; dummy data ignored
 - < 03 EF BE       - Data from frame memory: 0xBEEF
 -
 - The command byte can be one of:
 -
 - 0 -> LT24.Reset
 - 1 -> LT24.Command
 - 2 -> LT24.Write
 - 3 -> LT24.ReadFM
 - 4 -> LT24.ReadID
 -}

intfBare = intf LT24.lt24

{-
 - Create an UART interface, resetting the LT24 on powerup / reset
 -}
intfInited = intf lt24WithInit

intf lt24 i = o
    where
        o = ((combineOutput <$>) . pack)
              (txd, lcd_on, csx, resx, dcx, wrx, rdx, ltdout, oe)

        rxd = vhead <$> i
        ltdin = (fromBV . vtail) <$> i

        (ready, dout, lcd_on, csx, resx, dcx, wrx, rdx, ltdout, oe) =
            lt24 (action, din, ltdin)

        tTick = ($(CS.staticAvgRate fClk 115200) <^> 1)
                  sCmd
        (sCmd, txDone, txd) = Serial.output (tTick, txiV, txi)
        rTick = ($(CS.staticAvgRate fClk (16*115200)) <^> 1)
                  (signal CS.Run)
        (rxoF, rxoV) = Serial.input (rTick, rxd)

        (txiV, txi, action, din) = commandIf (rxoF, rxoV, txDone, ready, dout)

{-
 - Because CλaSH components should have a single input and a single output
 - (otherwise they can't be simulated), but tuples in the topEntity produce an
 - unpractical type in the generated VHDL, all inputs and outputs for the
 - topEntity are combined into a bitvector. The VHDL wrapper then untangles the
 - vector.
 -}
combineOutput (txd, lcd_on, csx, resx, dcx, wrx, rdx, ltdout, oe)
    = ((txd :> lcd_on :> csx :> resx :> dcx :> wrx :> rdx :> Nil)
       <++> toBV ltdout) <: oe

-- Command interface
commandIf (rxoF, rxoV, txDone, ready, dout) = (txiV, txi, action, din)
    where
        (rxoFE, rxo) = unpack rxoF

        rxVec = (groupBytes <^> (vcopyI 0)) (rxo, rxoV)
        cValid = (countBytes <^> 2) rxoV

        (action, din) = (passCommand <^> (PCIdle, LT24.NOP, 0))
                          (rxC, rxDW, cValid, ready)

        (rxC, rxDW) = (unpack . (splitRxVec <$>) . pack) rxVec

        (txi, txiV) = (returnData <^> RDState 0 0 0 0)
                        (txDone, rxC, rxDW, ready, dout)

splitRxVec i = (cmd, d)
    where
        cmd = vlast i
        d   = (fromBV . vconcat . vmap toBV . vinit) i

--           (rxo, rxoV  )
groupBytes s (_  , False ) = (s ,s )
groupBytes s (rxo, True  ) = (s',s')
    where
        s' = rxo +>> s

countBytes :: Unsigned 2
           -> Bool
           -> (Unsigned 2, Bool)

--         s rxoV  
countBytes s False = (s , False)
countBytes 0 True  = (2 , True )
countBytes s True  = (s', False)
    where
        s' = s - 1

data PCState = PCIdle | PCWaitAccept | PCWaitReady

{-
 - Pass commands to LT24.lt24
 -
 - Wait for 'cValid' indicating three bytes have been received from the PC.
 - Then pass the command and the data to LT24.lt24, and wait for that component
 - to signal acceptance and then completion.
 -}

passCommand :: (PCState, LT24.Action, Unsigned 16)
            -> (Unsigned 8, Unsigned 16, Bool, Bool)
            -> ((PCState, LT24.Action, Unsigned 16), (LT24.Action, Unsigned 16))

--            (PCState      , cbuf, dbuf) (cmd, d, cValid, ready )
passCommand s@(PCIdle       , cbuf, dbuf) (cmd, d, False , ready )
    = (s , (cbuf, dbuf))
passCommand   (PCIdle       , cbuf, dbuf) (cmd, d, True  , ready )
    = (s', (cbuf', d  ))
    where
        s' = (PCWaitAccept, cbuf', d)
        cbuf' = case cmd of
                    0 -> LT24.Reset
                    1 -> LT24.Command
                    2 -> LT24.Write
                    3 -> LT24.ReadFM
                    4 -> LT24.ReadID
                    _ -> LT24.NOP

passCommand s@(PCWaitAccept , cbuf, dbuf) (cmd, d, cValid, True )
    = (s , (cbuf , dbuf))

passCommand   (PCWaitAccept , cbuf, dbuf) (cmd, d, cValid, False)
    = (s', (cbuf', dbuf))
    where
        s' = (PCWaitReady, cbuf', dbuf)
        cbuf' = LT24.NOP

passCommand s@(PCWaitReady , cbuf, dbuf) (cmd, d, cValid, False)
    = (s , (cbuf , dbuf))

passCommand   (PCWaitReady , cbuf, dbuf) (cmd, d, cValid, True )
    = (s', (cbuf , dbuf))
    where
        s' = (PCIdle, cbuf, dbuf)

data RDState = RDState
    { rdMode :: Unsigned 3
    , rxCBuf :: Unsigned 8
    , rxDWBuf :: Unsigned 16
    , doutBuf :: Unsigned 16
    }

{- Return a response to the PC
 -
 - Waits for command completion, 
 -
 - The whole thing is paced by txDone (which indicates a byte has been sent/can
 - be sent to the PC).
 -
 - Output: (txi, txiV) inputs to Serial.output: byte to be transmitted, byte
 - valid if `txiV` is True.
 -}
returnData :: RDState
           -> (Bool, Unsigned 8, Unsigned 16, Bool, Unsigned 16)
           -> (RDState, (Unsigned 8, Bool))

--           (txDone, rxC, rxDW, ready, dout)
returnData s (False , rxC, rxDW, ready, dout) = (s, (0, False))
returnData s (True  , rxC, rxDW, ready, dout)
    = returnData' s (rxC, rxDW, ready, dout)

{- Template
returnData' s@(RDState { rdMode = 0 }) (rxC, rxDW, ready, dout) =
 -}

-- Idle, capture inputs
returnData' s@(RDState { rdMode = 0 }) (rxC, rxDW, True , dout) =
    (s'              , (0  , False))
    where
        s' = RDState 0 rxC rxDW (doutBuf s)

-- Start of command
returnData' s@(RDState { rdMode = 0 }) (rxC, rxDW, False, dout) =
    (s { rdMode = 1 }, (0, False))

-- Wait for command completion
returnData' s@(RDState { rdMode = 1 }) (rxC, rxDW, False, dout) =
    (s               , (0  , False))

-- Report on command completion
returnData' s@(RDState { rdMode = 1 }) (rxC, rxDW, True , dout) =
    (s'              , (d  , True ))
    where
        d = rxCBuf s
        s' = s { rdMode = mode', doutBuf = dout }
        mode' = case d of
                  -- Read commands: return data
                  3 -> 2
                  4 -> 2
                  -- Echo arguments
                  _ -> 4

-- Send doutBuf LSB
returnData' s@(RDState { rdMode = 2 }) (rxC, rxDW, ready, dout) =
    (s { rdMode = 3 }, (d  , True ))
    where
        d = resize $ doutBuf s

-- Send doutBuf MSB
returnData' s@(RDState { rdMode = 3 }) (rxC, rxDW, ready, dout) =
    (s { rdMode = 0 }, (d  , True ))
    where
        d = resize $ doutBuf s `shiftR` 8

-- Send rxDWBuf LSB
returnData' s@(RDState { rdMode = 4 }) (rxC, rxDW, ready, dout) =
    (s { rdMode = 5 }, (d  , True ))
    where
        d = resize $ rxDWBuf s

-- Send rxDWBuf MSB
returnData' s@(RDState { rdMode = 5 }) (rxC, rxDW, ready, dout) =
    (s { rdMode = 0 }, (d  , True ))
    where
        d = resize $ rxDWBuf s `shiftR` 8

