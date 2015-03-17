{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module UnitTest.LT24.Framebuffer.BouncyBallRMW where

import CLaSH.Prelude

import LT24.FramebufferRMW
import UnitTest.LT24.Framebuffer.BouncyCommon

{-
 - Make a ball travel at 45° angles and bounce against the sides of the screen.
 -
 - This uses LT24.FramebufferRMW; the ball will leave a blue streak unless you
 - press KEY1.
 -}
topEntity = bouncyBall framebuffer
