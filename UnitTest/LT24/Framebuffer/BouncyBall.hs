{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module UnitTest.LT24.Framebuffer.BouncyBall where

import CLaSH.Prelude
import Control.Applicative

import qualified LT24.LT24 as LT24
import LT24.Framebuffer
import LT24.Commands
import UnitTest.LT24.Framebuffer.PixelBall
import Toolbox.Misc
import qualified Toolbox.ClockScale as CS
import Toolbox.FClk

topEntity i = o
    where
        o = ((combineOutput <$>) . pack)
              ( signal L, signal H, lcdOn, csx, resx, dcx, wrx, rdx, ltdout
              , oe)
        ltdin = (fromBV . vtail) <$> i

        period = ($(CS.staticOneShotPeriod fClk 0.05) <^> 1) doUpdate
        (x, y) = (ballPos <^> (0, 0, BpDown, BpRight)) doUpdate
        (lt24AD, fbAddr, fbDin, fbWrEn, doUpdate)
            = (drawBall <^> DbInitDisp 0) (x, y, accepted, period)

        (action, din, accepted) = untilAccept (lt24AD, ready)

        ( ready, fbDout, updateDone, dout, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe)
            = framebuffer ( action, din, fbAddr, fbDin, fbWrEn
                          , doUpdate, ltdin)

combineOutput (gpioO, txd, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe)
    = ((gpioO :> txd :> lcdOn :> csx :> resx :> dcx :> wrx :> rdx :> Nil)
       <++> toBV ltdout) <: oe

data DbState = DbInitDisp (Unsigned 4) | DbWriteRam (Unsigned 6) (Unsigned 6)
             | DbDone
    deriving (Show, Eq)

data DbI = DbI
    { dbX :: Unsigned 6
    , dbY :: Unsigned 6
    , dbAccepted :: Bool
    , dbPeriod :: Bool
    }

data DbO = DbO
    { dbLt24AD :: Maybe (LT24.Action, Unsigned 16)
    , dbFbAddr :: Unsigned 12
    , dbFbDin :: Unsigned 2
    , dbFbWrEn :: Bool
    , dbDoUpdate :: Bool
    }

dbO = DbO
    { dbLt24AD = Nothing
    , dbFbAddr = 0
    , dbFbDin = 0
    , dbFbWrEn = False
    , dbDoUpdate = False
    }

untilAccept = untilAccept' <^> (LT24.NOP, 0, True)
untilAccept' (c, d, lastReady) (i, ready)
    = ((c', d', ready), (c', d', accepted))
    where
        sawAccept = lastReady && not ready
        (c', d') = case (i, sawAccept) of
                     (Just (ci,di), _    ) -> (ci      , di)
                     (Nothing     , False) -> (c       , d )
                     (Nothing     , True ) -> (LT24.NOP, d )
        accepted = c == LT24.NOP

data BallHDir = BpLeft | BpRight
data BallVDir = BpUp | BpDown

ballPos (x, y, v, h) False = ((x , y , v , h ),(x, y))
ballPos (x ,y, v, h) True  = ((x', y', v', h'),(x, y))
    where
        h' = case (h, y) of
               (BpLeft , 0  ) -> BpRight
               (BpRight, 256) -> BpLeft
               _            -> h
        v' = case (v, x) of
               (BpUp  , 0  ) -> BpDown
               (BpDown, 192) -> BpUp

        x' = case v' of
               BpUp   -> x - 1
               BpDown -> x + 1
        y' = case h' of
               BpLeft  -> y - 1
               BpRight -> y + 1

drawBall s (x, y, accepted, period)
    = (s', (lt24AD, fbAddr, fbDin, fbWrEn, doUpdate))
    where
        i = DbI { dbX = x
                , dbY = y
                , dbAccepted = accepted
                , dbPeriod = period
                }
        (s', o) = drawBall' s i
        lt24AD = dbLt24AD o
        fbAddr = dbFbAddr o
        fbDin = dbFbDin o
        fbWrEn = dbFbWrEn o
        doUpdate = dbDoUpdate o

drawBall' s@(DbInitDisp n) (DbI { dbAccepted = False }) = (s, dbO)
drawBall' s@(DbInitDisp n) i
    = ( s'
      , dbO { dbLt24AD = Just ad })
    where
        s' | n == 9    = DbWriteRam 0 0
           | otherwise = DbInitDisp (n+1)
        ad = case n of
               0 -> (LT24.Command, cCASET)
               1 -> (LT24.Write  , 0     )
               2 -> (LT24.Write  , x     )
               3 -> (LT24.Write  , 0     )
               4 -> (LT24.Write  , x+47  )
               5 -> (LT24.Command, cPASET)
               6 -> (LT24.Write  , 0     )
               7 -> (LT24.Write  , y     )
               8 -> (LT24.Write  , 0     )
               9 -> (LT24.Write  , y+63  )
        x = resize (dbX i)
        y = resize (dbY i)

drawBall' s@(DbWriteRam x y ) _
    = ( s'
      , dbO { dbFbAddr = fromBV $ (toBV y) <++> (toBV x)
            , dbFbDin  = theBall!x!y
            , dbFbWrEn = True
            , dbDoUpdate = (x, y) == (46, 46)
            })
    where
        s' = case (x,y) of
               (46, 46) -> DbDone
               (46, _ ) -> DbWriteRam 0     (y+1)
               (_ , _ ) -> DbWriteRam (x+1) y

drawBall' s@(DbDone) (DbI { dbPeriod = True }) = (DbInitDisp 0, dbO)

drawBall' s i = (s, dbO)

theBall = vcopy d47 (vcopy d47 (0 :: Unsigned 2))
--theBall = $(pixelBallTH 23 5)
