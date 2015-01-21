module LT24.LT24 where

import CLaSH.Prelude

import LT24.Misc
import qualified Toolbox.ClockScale as CS
import Toolbox.PackInstances

data Action = NOP | Reset | Command | Write | ReadFM | ReadID
    deriving (Show, Eq)

data LTState = LTIdle | LTReset | LTRead | LTWrite
    deriving (Show, Eq)

-- TODO: LCD_ON pin

{- Output Enable (oe) and Write (wrx) are delayed to be in sync with the update
 - to the 3-state output buffer
 -}
lt24 (action, din, ltdin) = (ready, dout, csx, resx, dcx, wrxD, rdx, ltdout, oeD)
    where
        (ready, dout, resx, dcx, wrx, rdx, ltdout, oe) =
            (lt24'1 <^> initLt24) (action, din, ltdin)
        csx = signal L
        oeD = register L oe
        wrxD = register H wrx

lt24'1 :: ( (LTState, Unsigned 16, (Bit, Bit, Bit, Bit, Unsigned 16, Bit))
          , Unsigned 13)
       -> (Action, Unsigned 16, Unsigned 16)
       -> ( ( (LTState, Unsigned 16, (Bit, Bit, Bit, Bit, Unsigned 16, Bit))
            , Unsigned 13)
          , (Bool, Unsigned 16, Bit, Bit, Bit, Bit, Unsigned 16, Bit))

lt24'1 s i = (s', (ready, dout, resx, dcx, wrx, rdx, ltdout, oe))
    where
        s' = lt24'2 s i
        (is, _) = s
        (st, ibuf, obuf) = is
        ready = st == LTIdle
        dout = ibuf
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

initLt24 = ( ( LTIdle
             , 0     -- ibuf
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
lt24'3 (st     , ibuf, obuf) (action , din, ltdin) = ( (s'    , ibuf , obuf')
                                                     , $(CS.ticksMinPeriod fClk
                                                         355e-9))
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (resx', dcx', wrx', rdx', ltdout', oe')
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf
-}

lt24'3 (LTIdle , ibuf, obuf) (NOP    , _  , ltdin) = ( (LTIdle , ibuf , obuf')
                                                     , 0          )
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , dcx , H   , H   , ltdout , L  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

lt24'3 (LTIdle , ibuf, obuf) (Reset  , din, ltdin) = ( (LTReset, ibuf , obuf')
                                                     , $(CS.ticksMinPeriod fClk
                                                           10e-6))
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (L    , H   , H   , H   , ltdout , L  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

lt24'3 (LTIdle , ibuf, obuf) (Command, din, ltdin) = ( (LTWrite, ibuf , obuf')
                                                     , 21)
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , L   , L   , H   , din    , H  )

lt24'3 (LTIdle , ibuf, obuf) (Write  , din, ltdin) = ( (LTWrite, ibuf , obuf')
                                                     , 21)
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , H   , L   , H   , din    , H  )

lt24'3 (LTIdle , ibuf, obuf) (ReadFM , din, ltdin) = ( (LTRead , ibuf , obuf')
-- max $(CS.ticksMinPeriod fClk 355e-9) ($(CS.ticksMinPeriod fClk 355e-9) + 1)
                                                     , 21)
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , H   , H   , L   , ltdout , L  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

lt24'3 (LTIdle , ibuf, obuf) (ReadID , din, ltdin) = ( (LTRead , ibuf , obuf')
                                                     , 21)
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , H   , H   , L   , ltdout , L  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

lt24'3 (LTReset, ibuf, obuf) (action , din, ltdin) = ( (LTIdle , ibuf , obuf')
                                                     , $(CS.ticksMinPeriod fClk
                                                           120e-6))
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , H   , H   , H   , ltdout , L  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

lt24'3 (LTWrite, ibuf, obuf) (action , din, ltdin) = ( (LTIdle , ibuf , obuf')
                                                     , 21)
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , dcx , H   , H   , ltdout , H  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf

lt24'3 (LTRead , ibuf, obuf) (action , din, ltdin) = ( (LTIdle , ibuf', obuf')
                                                     , 21)
    where
        --      (resx', dcx', wrx', rdx', ltdout', oe')
        obuf' = (H    , H   , H   , H   , ltdout , L  )
        (resx, dcx, wrx, rdx, ltdout, oe) = obuf
        ibuf' = ltdin

