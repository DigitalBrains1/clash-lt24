{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module UnitTest.LT24.Framebuffer.BouncyBall where

import CLaSH.Prelude

import LT24.Framebuffer
import UnitTest.LT24.Framebuffer.BouncyCommon

topEntity = bouncyBall framebuffer
