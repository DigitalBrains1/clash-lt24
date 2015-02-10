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

topEntity i = o
    where
        o = ((combineOutput <$>) . pack)
              ( signal L, signal H, lcdOn, csx, resx, dcx, wrx, rdx, ltdout
              , oe)
        ltdin = (fromBV . vtail) <$> i

        (lt24AD, fbAddr, fbDin, fbWrEn, doUpdate)
            = (drawBall <^> DbInitDisp 0) accepted
        --    = (drawBall <^> DbWriteRam 0 0) accepted

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

drawBall s accepted
    = (s', (lt24AD, fbAddr, fbDin, fbWrEn, doUpdate))
    where
        (s', o) = drawBall' s accepted
        lt24AD = dbLt24AD o
        fbAddr = dbFbAddr o
        fbDin = dbFbDin o
        fbWrEn = dbFbWrEn o
        doUpdate = dbDoUpdate o

drawBall' s@(DbInitDisp n) False = (s, dbO)
drawBall' s@(DbInitDisp n) True
    = ( s'
      , dbO { dbLt24AD = Just ad })
    where
        s' | n == 9    = DbWriteRam 0 0
           | otherwise = DbInitDisp (n+1)
        adv = $(v [ (LT24.Command, cCASET)
                  , (LT24.Write  , 0     )
                  , (LT24.Write  , 0     )
                  , (LT24.Write  , 0     )
                  , (LT24.Write  , 47    )
                  , (LT24.Command, cPASET)
                  , (LT24.Write  , 0     )
                  , (LT24.Write  , 0     )
                  , (LT24.Write  , 0     )
                  , (LT24.Write  , 63    )])
        ad = adv!(9 - n)
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

drawBall' s@(DbDone) _ = (s, dbO)

--theBall = vcopy d47 (vcopy d47 (0 :: Unsigned 2))
theBall = $(pixelBallTH 23 5)
