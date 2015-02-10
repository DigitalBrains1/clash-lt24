{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module UnitTest.LT24.Framebuffer.PixelBall where

import CLaSH.Prelude
import Toolbox.Misc

pixelBall :: Int
          -> Int
          -> [[Unsigned 2]]

pixelBall r1 r2 = zipWith (zipWith coordColor) (repeat [0 .. 2*r1])
                    (map (replicate (2*r1+1)) [0 .. 2*r1])
    where
        coordColor x y | (r1 - x)^2 + (r1 - y)^2 <= r2^2 = 1
                       | (r1 - x)^2 + (r1 - y)^2 <= r1^2 = 3
                       | otherwise                       = 0

pixelBallTH r1 r2 = vv $ pixelBall r1 r2

asciiBall r1 r2 = putStr $ concat $ map line $ pixelBall r1 r2
    where
        line = (++ "\n") . map dot
        dot 0 = ' '
        dot 1 = '+'
        dot 3 = '*'

