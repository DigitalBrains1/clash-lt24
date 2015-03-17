{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module UnitTest.LT24.Framebuffer.BouncyBall where

import CLaSH.Prelude

import LT24.Framebuffer
import UnitTest.LT24.Framebuffer.BouncyCommon

topEntity = bouncyBall fb

-- The RMW framebuffer has a "pageStart" argument, but the non-RMW version does
-- not. Lose the argument.
fb ( action, din, fbAddr, fbDin, fbWrEn, pageStart, doUpdate , pixelColour
   , ltdin)
   = framebuffer ( action, din, fbAddr, fbDin, fbWrEn, doUpdate, pixelColour
                 , ltdin)


