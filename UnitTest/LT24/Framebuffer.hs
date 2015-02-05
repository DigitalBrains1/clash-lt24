module UnitTest.LT24.Framebuffer where

import CLaSH.Prelude

pixelBall :: Int
          -> Int
          -> [[Unsigned 2]]

pixelBall r1 r2 = zipWith (zipWith coordColor) (repeat [0 .. 2*r1])
                    (map (replicate (2*r1+1)) [0 .. 2*r1])
    where
        coordColor x y = coordColorF (fromIntegral x) (fromIntegral y)
        r1F = fromIntegral r1
        r2F = fromIntegral r2
        coordColorF x y | sqrt((r1F - x)^2 + (r1F - y)^2) <= r2F = 1
                        | sqrt((r1F - x)^2 + (r1F - y)^2) <= r1F = 3
                        | otherwise                              = 0

asciiBall r1 r2 = putStr $ concat $ map line $ pixelBall r1 r2
    where
        line = (++ "\n") . map dot
        dot 0 = ' '
        dot 1 = '+'
        dot 3 = '*'
