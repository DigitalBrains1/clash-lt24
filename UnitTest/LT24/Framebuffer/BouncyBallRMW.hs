{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module UnitTest.LT24.Framebuffer.BouncyBallRMW where

import CLaSH.Prelude

import LT24.FramebufferRMW
import UnitTest.LT24.Framebuffer.BouncyCommon

topEntity = bouncyBall framebuffer
