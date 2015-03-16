{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module UnitTest.LT24.Framebuffer.BouncyCommon where

import CLaSH.Prelude
import Control.Applicative
import Debug.Trace

import qualified LT24.LT24 as LT24
import LT24.Commands
import Toolbox.Misc
import qualified Toolbox.ClockScale as CS
import Toolbox.FClk

bouncyBall fb i = o
    where
        o = ((combineOutput <$>) . pack)
              ( signal L, signal H, lcdOn, csx, resx, dcx, wrx, rdx, ltdout
              , oe)
        ltdin = (fromBV . vdrop d2) <$> i
        button = vhead <$> i

        buttonF = tfold (.&.) H (button, doUpdate)
        period = ($(CS.staticOneShotPeriod fClk 0.002) <^> 1) doUpdate
--        period = signal True
        doUpdateD = register False doUpdate
        (x, y) = (ballPos <^> (5, 7, BpDown, BpRight)) doUpdateD
        (xD, yD) = (delayCoords <^> (5, 7)) (x,y, doUpdateD)
        (wx, wy, rx, ry) = (unpack . (juggleCoords <$>) . pack) (x,y,xD,yD)
        (lt24AD, fbAddr, fbDin, fbWrEn, doUpdate, needAccess)
            = (drawBall <^> DbInitDisp 0)
                (wx, wy, rx, ry, accepted, period, buttonF)

        (action, din, accepted) = untilAccept (lt24AD, ready)

        pageStart = (resize . fromBV . toBV) <$> wy :: Signal (Unsigned 8)
        ( ready, fbDout, updateDone, pixelVal, dout, lcdOn, csx, resx, dcx,
          wrx, rdx, ltdout, oe)
--            = fb ( action, din, fbAddr, fbDin, fbWrEn, pageStart, doUpdate
--                 , pixelColor, ltdin)
            = fb ( action, din, fbAddr, fbDin, fbWrEn, pageStart
                 , not <$> needAccess, pixelColor, ltdin)
        -- Yellow, black, red, blue
        pixelColor = ($(v [ 0x1F :: Unsigned 16, 0xF800, 0, 0xFFE0 ])!)
                     <$> pixelVal

combineOutput (gpioO, txd, lcdOn, csx, resx, dcx, wrx, rdx, ltdout, oe)
    = ((gpioO :> txd :> lcdOn :> csx :> resx :> dcx :> wrx :> rdx :> Nil)
       <++> toBV ltdout) <: oe

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

delayCoords s (x, y, False) = (s     , s)
delayCoords s (x, y, True ) = ((x, y), s)

juggleCoords (x, y, xD, yD) = (wx, wy, rx, ry)
    where
        wx = min (320-64) $ min x xD
        wy = min (240-48) $ min y yD
        rx = x - wx
        ry = y - wy

data DbState = DbInitDisp (Unsigned 4) | DbWriteRam (Signed 14) (Signed 14)
             | DbDone
    deriving (Show, Eq)

data DbI = DbI
    { dbWx :: Signed 14
    , dbWy :: Signed 14
    , dbRx :: Signed 14
    , dbRy :: Signed 14
    , dbAccepted :: Bool
    , dbPeriod :: Bool
    , dbButtonF :: Bit
    }

data DbO = DbO
    { dbLt24AD :: Maybe (LT24.Action, Unsigned 16)
    , dbFbAddr :: Unsigned 12
    , dbFbDin :: Unsigned 2
    , dbFbWrEn :: Bool
    , dbDoUpdate :: Bool
    , dbNeedAccess :: Bool
    }

dbO = DbO
    { dbLt24AD = Nothing
    , dbFbAddr = 0
    , dbFbDin = 0
    , dbFbWrEn = False
    , dbDoUpdate = False
    , dbNeedAccess = False
    }

drawBall s (wx, wy, rx, ry, accepted, period, buttonF)
    = (s', (lt24AD, fbAddr, fbDin, fbWrEn, doUpdate, needAccess))
    where
        i = DbI { dbWx = wx
                , dbWy = wy
                , dbRx = rx
                , dbRy = ry
                , dbAccepted = accepted
                , dbPeriod = period
                , dbButtonF = buttonF
                }
        (s', o) = drawBall' s i
        lt24AD = dbLt24AD o
        fbAddr = dbFbAddr o
        fbDin = dbFbDin o
        fbWrEn = dbFbWrEn o
        doUpdate = dbDoUpdate o
        needAccess = dbNeedAccess o

drawBall' s (DbI { dbAccepted = False })
    = (s, dbO { dbNeedAccess = True })

drawBall' s@(DbInitDisp n) i
    = ( s'
      , dbO { dbLt24AD = Just ad
            , dbNeedAccess = True
            })
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
      , dbO { dbFbAddr = fromBV $ yBV <++> xBV
            , dbFbDin  = d
            , dbFbWrEn = True
            , dbDoUpdate = s' == DbDone
            })
    where
        s' = case (x,y) of
               (63, 47) -> DbDone
               (63, _ ) -> DbWriteRam 0     (y+1)
               (_ , _ ) -> DbWriteRam (x+1) y

        xBV = vdropI (toBV x) :: Vec 6 Bit
        yBV = vdropI (toBV y) :: Vec 6 Bit

        toHubX   = outerRBall + dbRx i - x
        toHubY   = outerRBall + dbRy i - y
        toHubSq  = toHubX * toHubX + toHubY * toHubY

        d | toHubSq <= innerRBall * innerRBall = 2
          | toHubSq <= outerRBall * outerRBall = 3
          | dbButtonF i == L                   = 1
          | otherwise                          = 0

drawBall' s@(DbDone) (DbI { dbPeriod = True }) = (DbInitDisp 0, dbO)

drawBall' s i = (s, dbO)

outerRBall = 23
innerRBall = 5
bbox = outerRBall * 2 + 1
