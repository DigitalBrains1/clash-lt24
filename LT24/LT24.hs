{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module LT24.LT24
       ( Action(..)
       , lt24
       ) where

import CLaSH.Prelude

import LT24.Timing

--import qualified Toolbox.ClockScale as CS
import Toolbox.PackInstances
import Toolbox.Misc

data Action = NOP | Reset | Command | Write | ReadFM | ReadID
    deriving (Show, Eq)

instance Lift Action where
    lift NOP     = [| NOP |]
    lift Reset   = [| Reset |]
    lift Command = [| Command |]
    lift Write   = [| Write |]
    lift ReadFM  = [| ReadFM |]
    lift ReadID  = [| ReadID |]

data LTState = LTIdle | LTReset | LTRead | LTWrite
    deriving (Show, Eq)

{-
 - Interface to the LT24 LC-display
 -
 - Inputs:
 -
 - `action`  - (Action) Communicate with the display controller:
 -               NOP     - Do nothing, stay idle
 -               Reset   - Do a hardware reset
 -               Command - Write a command to the controller
 -               Write   - Write data to the controller
 -               ReadFM  - Read with "frame memory" timing
 -               ReadID  - Read with "ID" timing
 - `din`     - (Unsigned 16) Data for controller data bus: a command from
 -             LT24.Commands, or parameters for the commands, or data for the
 -             frame memory.
 - `ltdin`   - (Unsigned 16) Connected to the tri-state buffer which is
 -             connected to the LT24 data bus.
 -
 - Outputs:
 -
 - `ready`   - (Bool) Command pacing: False when currently performing a
 -             command, True when ready to accept a new command. A transition
 -             from True to False indicates the command has been accepted, a
 -             transition from False to True indicates command completion and
 -             availability of data. Note that `ready` = True does not indicate
 -             that the command will be accepted directly; you should wait for
 -             a True to False transition signaling acceptance.
 - `dout`    - (Unsigned 16) Data read from the controller. When a command
 -             reads data from the controller (ReadFM, ReadID), and the `ready`
 -             signal has transitioned from False to True, `dout` is valid
 -             until the next command is accepted.
 - `lcd_on`
 - `csx`
 - `resx`
 - `dcx`
 - `wrx`
 - `rdx`     - (Bit) These should all be connected to the respective pins of
 -             the LT24. Note that what the Ilitek datasheet calls "D/CX" is
 -             called "RS" by the SGD datasheet, the LT24 schematic and the
 -             Terasic System Builder. Quite annoying.
 - `ltdout`  - (Unsigned 16) Connected to the tri-state buffer which is
 -             connected to the LT24 data bus.
 - `oe`      - (Bit) Output Enable for tri-state buffer.
 -
 - The interface to this component is through `action`, `din`, `ready` and
 - `dout`; all the other signals are connected to the LT24.
 -
 - `action` and `din` specify the desired action. When `ready` transitions from
 - True to False, LT24.lt24 has accepted the action and is performing it.
 - When `ready` transitions back to True, the action has completed, and if it
 - produces data (ReadFM or ReadID), it is available on `dout`. If by this time
 - `action` is not NOP, another action could be accepted immediately, causing
 - `ready` to transition back to False. This way, actions can be almost
 - back-to-back, starting the next action only a single cycle after the result
 - of the previous action was produced.
 -
 - This means that as soon as `ready` transitions to False to indicate command
 - acceptance, the next action or NOP should be passed to `action`. `action`
 - should not be allowed to "linger", lest it be executed twice.
 -
 - There are two timing profiles for reads: ReadID and ReadFM, as indicated on
 - page 238 of the Ilitek datasheet. I could not find conclusive documentation,
 - but I assume that FM is the timing to read from Frame Memory (cRAMRD,
 - cRead_Memory_Continue) and ID is the timing for all other reads, i.e., those
 - pertaining to the controller state and settings.
 -
 - The backlight is always on and Chip Select is always active.
 -
 - Output Enable (oe) and Write (wrx) are delayed to be in sync with the update
 - to the 3-state output buffer.
 -}

lt24 (action, din, ltdin)
    = (ready, dout, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe)
    where
        (ready, dout, resx, dcx, wrx, rdx, ltdout, oe) =
            (lt24'1 <^> initLt24) (action, din, ltdin)
        lcdOn = signal H
        csx = signal L

lt24'1 :: ( LTState, $(uToFit wLargest), (Bit, Bit, Bit, Bit, Bit, Unsigned 16)
          , (Action, Unsigned 16), Unsigned 16)
       -> (Action, Unsigned 16, Unsigned 16)
       -> ( ( LTState, $(uToFit wLargest)
            , (Bit, Bit, Bit, Bit, Bit, Unsigned 16), (Action, Unsigned 16)
            , Unsigned 16)
          , (Bool, Unsigned 16, Bit, Bit, Bit, Bit, Unsigned 16, Bit))

lt24'1 (st, wait, edgeBufO, cmdBufI, cmdBufO)
       (action, din, ltdin)
    = ( (st', wait', edgeBufO', cmdBufI', dout)
      , (ready, dout, resx, dcx, wrx, rdx, ltdout', oe))
    where
        (resx, dcx, wrx, rdx, oe, ltdout) = edgeBufO
        edgeBufO' = (resx', dcx', wrx', rdx', oe', ltdout')
        (actionD, dinD) = cmdBufI
        cmdBufI' = (action, din)
        {- The start of a second phase of a read stores the ltdin from the
         - display. For things that are not a read, it doesn't matter what
         - happens.
         -}
        dout     | st /= LTIdle && st' == LTIdle = ltdin
                 | otherwise                     = cmdBufO
        -- Latches as soon as we leave Idle
        ltdout' | st == LTIdle && st' /= LTIdle = dinD
                | otherwise                     = ltdout

        (st', wait', resx', dcx', wrx', rdx', oe')
            | wait > 0  = (st, wait - 1, resx, dcx, wrx, rdx, oe)
            | otherwise = case st of
                            LTIdle  -> acceptCommand actionD
                                    --          wait
                            _       -> (LTIdle, secondPhaseLength st
                                    --   resx dcx  wrx rdx oe
                                       , H  , dcx, H , H , oe)

        ready = st' == LTIdle

initLt24 = ( LTIdle
           , 0     -- wait
           , ( H   -- resx
             , H   -- dcx
             , H   -- wrx
             , H   -- rdx
             , L   -- oe
             , 0)  -- ltdout
           , ( NOP -- action
             , 0)  -- din
           , 0) -- cmbBufO

acceptCommand actionD
    = case actionD of --     wait                resx dcx wrx rdx oe
        NOP     -> (LTIdle , 0                 , H  , H , H , H , L)
        Reset   -> (LTReset, $(intLit wResetL) , L  , H , H , H , L)
        Command -> (LTWrite, $(intLit wWriteL) , H  , L , L , H , H)
        Write   -> (LTWrite, $(intLit wWriteL) , H  , H , L , H , H)
        ReadFM  -> (LTRead , $(intLit wReadLFM), H  , H , H , L , L)
        ReadID  -> (LTRead , $(intLit wReadLID), H  , H , H , L , L)

secondPhaseLength st = case st of
                         LTReset -> $(intLit wResetH)
                         LTWrite -> $(intLit wWriteH)
                         LTRead  -> $(intLit wReadH)

