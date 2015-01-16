module LT24.UARTInterface where

import CLaSH.Prelude
import Control.Applicative

import qualified LT24.LT24 as LT24
import qualified Toolbox.Serial as Serial
import qualified Toolbox.ClockScale as CS
import LT24.Misc

intf i = o
    where
        o = ((combineOutput <$>) . pack)
              (txd, csx, resx, dcx, wrx, rdx, ltdout, oe)

        rxd = vhead <$> i
        ltdin = (fromBV . vtail) <$> i

        (ready, dout, csx, resx, dcx, wrx, rdx, ltdout, oe) =
            LT24.lt24 (action, din, ltdin)

        tTick = ($(CS.staticAvgRate fClk 115200) <^> 1)
                  sCmd
        (sCmd, txDone, txd) = Serial.output (tTick, txiV, txi)
        rTick = ($(CS.staticAvgRate fClk (16*115200)) <^> 1)
                  (signal CS.Run)
        (rxoF, rxoV) = Serial.input (rTick, rxd)
        (rxoFE, rxo) = unpack rxoF

        rxVec = (groupBytes <^> (vcopy d3 (0 :: Unsigned 8))) (rxo, rxoV)
        cValid = (countBytes <^> (0 :: Unsigned 2)) rxoV

        (action, din) = (passCommand <^> (LT24.NOP, 0, False))
                          (rxC, rxDW, cValid, ready)

--        rxC = (vlast :: Vec 3 (Unsigned 8) -> Unsigned 8) <$> rxVec
--        rxDW = (fromBV . vconcat . vmap toBV . vinit) <$> rxVec

        (rxC, rxDW) = (unpack . (splitInput <$>) . pack) rxVec

        (txi, txiV) = (returnData <^> RDState 0 0 0 0)
                        (txDone, rxoFE, rxC, rxDW, ready, dout)

combineOutput (txd, csx, resx, dcx, wrx, rdx, ltdout, oe)
    = ((txd :> csx :> resx :> dcx :> wrx :> rdx :> Nil)
       <++> toBV ltdout) <: oe

splitInput :: Vec 3 (Unsigned 8)
           -> (Unsigned 8, Unsigned 16)

splitInput i = (cmd, d)
    where
        cmd = vlast i
        d   = (fromBV . vconcat . vmap toBV . vinit) i

groupBytes :: Vec 3 (Unsigned 8)
           -> (Unsigned 8, Bool)
           -> (Vec 3 (Unsigned 8), Vec 3 (Unsigned 8))

--           (rxo, rxoV  )
groupBytes s (_  , False ) = (s,s)
groupBytes s (rxo, True  ) = (s',s')
    where
        s' = rxo +>> s

countBytes :: Unsigned 2
           -> Bool
           -> (Unsigned 2, Bool)

--         s rxoV  
countBytes s False = (s, False)
countBytes 0 True  = (2, True)
countBytes s True  = (s', False)
    where
        s' = s - 1

passCommand :: (LT24.Action, Unsigned 16, Bool)
            -> (Unsigned 8, Unsigned 16, Bool, Bool)
            -> ((LT24.Action, Unsigned 16, Bool), (LT24.Action, Unsigned 16))

--          s                    (cmd, d, cValid, ready)
passCommand s@(cbuf, dbuf, True) (cmd, d, cValid, False) = (s, (cbuf, dbuf))
passCommand   (cbuf, dbuf, wait) (cmd, d, False , ready) = (s', (cbuf', d))
    where
        s' = (cbuf', dbuf, False)
        cbuf' = LT24.NOP
passCommand   (cbuf, dbuf, wait) (cmd, d, True  , ready) = (s', (cbuf', d))
    where
        s' = (cbuf', dbuf, True)
        cbuf' = case cmd of
                    0 -> LT24.Reset
                    1 -> LT24.Command
                    2 -> LT24.Write
                    3 -> LT24.ReadFM
                    4 -> LT24.ReadID
                    _ -> LT24.NOP

data RDState = RDState
    { rdMode :: Unsigned 3
    , rxCBuf :: Unsigned 8
    , rxDWBuf :: Unsigned 16
    , doutBuf :: Unsigned 16
    }

returnData :: RDState
           -> (Bool, Bool, Unsigned 8, Unsigned 16, Bool, Unsigned 16)
           -> (RDState, (Unsigned 8, Bool))

--           (txDone, rxoFE, rxC, rxDW, ready, dout)
returnData s (False , rxoFE, rxC, rxDW, ready, dout) = (s, (0, False))
returnData s (True  , rxoFE, rxC, rxDW, ready, dout)
    = returnData' s (rxoFE, rxC, rxDW, ready, dout)

{- Template
returnData' s@(RDState { rdMode = 0 }) (rxoFE, rxC, rxDW, ready, dout) =
 -}

-- Frame error
returnData' s@(RDState { rdMode = 0 }) (True , rxC, rxDW, ready, dout) =
    (s               , (128, True))

-- Idle, capture inputs
returnData' s@(RDState { rdMode = 0 }) (rxoFE, rxC, rxDW, True , dout) =
    (s'              , (0  , False))
    where
        s' = RDState 0 rxC rxDW (doutBuf s)

-- Start of command
returnData' s@(RDState { rdMode = 0 }) (rxoFE, rxC, rxDW, False, dout) =
    (s { rdMode = 1 }, (0, False))

-- Wait for command completion
returnData' s@(RDState { rdMode = 1 }) (rxoFE, rxC, rxDW, False, dout) =
    (s               , (0  , False))

-- Report on command completion
returnData' s@(RDState { rdMode = 1 }) (rxoFE, rxC, rxDW, True , dout) =
    (s'              , (d  , True ))
    where
        d = rxCBuf s
        s' = s { rdMode = mode', doutBuf = dout }
        mode' = case d of
                  -- Read commands
                  3 -> 2
                  4 -> 2
                  -- Echo arguments
                  _ -> 4

-- Send doutBuf LSB
returnData' s@(RDState { rdMode = 2 }) (rxoFE, rxC, rxDW, ready, dout) =
    (s { rdMode = 3 }, (d  , True ))
    where
        d = resize $ doutBuf s

-- Send doutBuf MSB
returnData' s@(RDState { rdMode = 3 }) (rxoFE, rxC, rxDW, ready, dout) =
    (s { rdMode = 0 }, (d  , True ))
    where
        d = resize $ doutBuf s `shiftR` 8

-- Send rxDWBuf LSB
returnData' s@(RDState { rdMode = 4 }) (rxoFE, rxC, rxDW, ready, dout) =
    (s { rdMode = 5 }, (d  , True ))
    where
        d = resize $ rxDWBuf s

-- Send rxDWBuf MSB
returnData' s@(RDState { rdMode = 5 }) (rxoFE, rxC, rxDW, ready, dout) =
    (s { rdMode = 0 }, (d  , True ))
    where
        d = resize $ rxDWBuf s `shiftR` 8

