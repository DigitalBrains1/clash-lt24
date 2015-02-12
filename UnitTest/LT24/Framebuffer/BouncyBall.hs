{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module UnitTest.LT24.Framebuffer.BouncyBall where

import CLaSH.Prelude
import Control.Applicative
import Debug.Trace

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

        period = ($(CS.staticOneShotPeriod fClk 0.01) <^> 1) doUpdate
        doUpdateD = register False doUpdate
        (x, y) = (ballPos <^> (5, 7, BpDown, BpRight)) doUpdateD
        (xD, yD) = (unpack . register (5, 7) . pack) (x,y)
        (wx, wy, rx, ry) = (unpack . (juggleCoords <$>) . pack) (x,y,xD,yD)
        (lt24AD, fbAddr, fbDin, fbWrEn, doUpdate)
            = (drawBall <^> DbInitDisp 0) (wx, wy, rx, ry, accepted, period)

        (action, din, accepted) = untilAccept (lt24AD, ready)

        ( ready, fbDout, updateDone, dout, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe)
            = framebuffer ( action, din, fbAddr, fbDin, fbWrEn
                          , doUpdate, ltdin)

combineOutput (gpioO, txd, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe)
    = ((gpioO :> txd :> lcdOn :> csx :> resx :> dcx :> wrx :> rdx :> Nil)
       <++> toBV ltdout) <: oe

data DbState = DbInitDisp (Unsigned 4) | DbWriteRam (Signed 10) (Signed 10)
             | DbDone
    deriving (Show, Eq)

data DbI = DbI
    { dbWx :: Signed 10
    , dbWy :: Signed 10
    , dbRx :: Signed 10
    , dbRy :: Signed 10
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
        h' = case (h, x, xl-x) of
               (BpLeft , 0, _) -> BpRight
               (BpRight, _, 0) -> BpLeft
               _               -> h
        v' = case (v, y, yl-y) of
               (BpUp  , 0, _) -> BpDown
               (BpDown, _, 0) -> BpUp
               _              -> v

        x' = case h' of
               BpLeft  -> x - 1
               BpRight -> x + 1
        y' = case v' of
               BpUp   -> y - 1
               BpDown -> y + 1

        xl = 320 - bbox
        yl = 240 - bbox

juggleCoords (x, y, xD, yD) = (wx, wy, rx, ry)
    where
        wx = max (320-64) $ min x xD
        wy = max (240-48) $ min y yD
        rx = x - wx
        ry = y - wy

drawBall s (wx, wy, rx, ry, accepted, period)
    = (s', (lt24AD, fbAddr, fbDin, fbWrEn, doUpdate))
    where
        i = DbI { dbWx = wx
                , dbWy = wy
                , dbRx = rx
                , dbRy = ry
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
               1 -> (LT24.Write  , xsH   )
               2 -> (LT24.Write  , xsL   )
               3 -> (LT24.Write  , xeH   )
               4 -> (LT24.Write  , xeL   )
               5 -> (LT24.Command, cPASET)
               6 -> (LT24.Write  , ysH   )
               7 -> (LT24.Write  , ysL   )
               8 -> (LT24.Write  , yeH   )
               9 -> (LT24.Write  , yeL   )
        xs = (resize . fromBV . toBV . dbWx) i :: Unsigned 9
        ys = (resize . fromBV . toBV . dbWy) i :: Unsigned 9
        xe = xs + 63
        ye = ys + 47
        xsL = resize (resize xs :: Unsigned 8)
        xsH = resize (xs `shiftR` 8)
        xeL = resize (resize xe :: Unsigned 8)
        xeH = resize (xe `shiftR` 8)
        ysL = resize (resize ys :: Unsigned 8)
        ysH = resize (ys `shiftR` 8)
        yeL = resize (resize ye :: Unsigned 8)
        yeH = resize (ye `shiftR` 8)

drawBall' s@(DbWriteRam x y ) i
    = ( s'
      , dbO { dbFbAddr = fromBV $ xBV <++> yBV
            , dbFbDin  = d
            , dbFbWrEn = True
            , dbDoUpdate = s' == DbDone
            })
    where
        s' = case (x,y) of
               (63, 47) -> DbDone
               (63, _ ) -> DbWriteRam 0     (y+1)
               (_ , _ ) -> DbWriteRam (x+1) y

        xBV = vselect d4 d1 d6 (toBV x) -- "Signed 10 -> Unsigned 6"
        yBV = vselect d4 d1 d6 (toBV y) -- "Signed 10 -> Unsigned 6"

        rxe = x - dbRx i -- Effective relative x-coordinate currently plotting
        rye = y - dbRy i -- Effective relative y-coordinate currently plotting

        d | rxe < 0     = 0
          | rxe >= bbox = 0
          | rye < 0     = 0
          | rye >= bbox = 0
          | otherwise   = theBall!rxe!rye

drawBall' s@(DbDone) (DbI { dbPeriod = True }) = (DbInitDisp 0, dbO)

drawBall' s i = (s, dbO)

bbox = fromInteger $ vlength theBall

--theBall = vcopy d47 (vcopy d47 (0 :: Unsigned 2))
theBall = $(pixelBallTH 23 5)
