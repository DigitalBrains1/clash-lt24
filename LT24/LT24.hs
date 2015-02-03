module LT24.LT24
       ( Action(..)
       , lt24
       ) where

import CLaSH.Prelude

import qualified Toolbox.ClockScale as CS
import Toolbox.PackInstances
import Toolbox.FClk
import Toolbox.Misc

data Action = NOP | Reset | Command | Write | ReadFM | ReadID
    deriving (Show, Eq)

data LTState = LTIdle | LTReset | LTRead | LTWrite
    deriving (Show, Eq)

-- TODO: Proper timing
-- TODO: ltdout is apparently delayed too much
-- TODO: Proper buffering to maximally shorten combinatorial path

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
    = (ready, dout, lcd_on, csx, resx, dcx, wrx, rdx, ltdout, oe)
    where
        (ready, dout, resx, dcx, wrxE, rdx, ltdout, oeE) =
            (lt24'1 <^> initLt24) (action, din, ltdin)
        lcd_on = signal H
        csx = signal L
        oe = register L oeE
        wrx = register H wrxE

lt24'1 :: ( (LTState, Unsigned 16, (Bit, Bit, Bit, Bit, Unsigned 16, Bit))
          , $(uToFit $(CS.ticksMinPeriod fClk 120e-3)))
       -> (Action, Unsigned 16, Unsigned 16)
       -> ( ( (LTState, Unsigned 16, (Bit, Bit, Bit, Bit, Unsigned 16, Bit))
            , $(uToFit $(CS.ticksMinPeriod fClk 120e-3)))
          , (Bool, Unsigned 16, Bit, Bit, Bit, Bit, Unsigned 16, Bit))

lt24'1 s i = (s', (ready, dout, resx, dcx, wrx, rdx, ltdout, oe))
    where
        s' = lt24'2 s i
        (is, _) = s
        (st, dout, obuf) = is
        ready = st == LTIdle
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

initLt24 = ( ( LTIdle
             , 0     -- dout
             , ( H   -- resx
               , H   -- dcx
               , H   -- wrx
               , H   -- rdx
               , 0   -- ltdout
               , L)) -- oe
           , 0)      -- wait

lt24'2 (is, 0   ) i = lt24'3 is i
lt24'2 (is, wait) _ = (is, wait - 1)


{- Template:
lt24'3 (st     , dout, obuf) (action , din, ltdin) = ( (s'    , dout , obuf')
                                                     , $(CS.ticksMinPeriod fClk
                                                           355e-9))
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (resx', dcx', wrx', rdx', ltdout', oe')
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf
-}

lt24'3 (LTIdle , dout, obuf) (NOP    , _  , ltdin) = ( (LTIdle , dout , obuf')
                                                     , 0          )
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , dcx , H   , H   , ltdout , L  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

lt24'3 (LTIdle , dout, obuf) (Reset  , din, ltdin) = ( (LTReset, dout , obuf')
                                                     , $(CS.ticksMinPeriod fClk
                                                           10e-6))
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (L    , H   , H   , H   , ltdout , L  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

lt24'3 (LTIdle , dout, obuf) (Command, din, ltdin) = ( (LTWrite, dout , obuf')
                                                     , $(CS.ticksMinPeriod fClk
                                                           355e-9) + 4)
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , L   , L   , H   , din    , H  )

lt24'3 (LTIdle , dout, obuf) (Write  , din, ltdin) = ( (LTWrite, dout , obuf')
                                                     , $(CS.ticksMinPeriod fClk
                                                           355e-9) + 4)
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , H   , L   , H   , din    , H  )

lt24'3 (LTIdle , dout, obuf) (ReadFM , din, ltdin) = ( (LTRead , dout , obuf')
-- max $(CS.ticksMinPeriod fClk   355e-9) ($(CS.ticksMinPeriod fClk 355e-9) + 1)
                                                     , $(CS.ticksMinPeriod fClk
                                                           355e-9) + 4)
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , H   , H   , L   , ltdout , L  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

lt24'3 (LTIdle , dout, obuf) (ReadID , din, ltdin) = ( (LTRead , dout , obuf')
                                                     , $(CS.ticksMinPeriod fClk
                                                           355e-9) + 4)
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , H   , H   , L   , ltdout , L  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

lt24'3 (LTReset, dout, obuf) (action , din, ltdin) = ( (LTIdle , dout , obuf')
                                                     , $(CS.ticksMinPeriod fClk
                                                           120e-3))
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , H   , H   , H   , ltdout , L  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

lt24'3 (LTWrite, dout, obuf) (action , din, ltdin) = ( (LTIdle , dout , obuf')
                                                     , $(CS.ticksMinPeriod fClk
                                                           355e-9) + 4)
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , dcx , H   , H   , ltdout , H  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

lt24'3 (LTRead , dout, obuf) (action , din, ltdin) = ( (LTIdle , dout', obuf')
                                                     , $(CS.ticksMinPeriod fClk
                                                           355e-9) + 4)
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , H   , H   , H   , ltdout , L  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf
        dout' = ltdin

