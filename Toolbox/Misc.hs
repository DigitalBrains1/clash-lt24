{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Toolbox.Misc
       ( showCodeLoc
       , intLit
       , uToFit
       , fitU
       , vS
       , vzip3
       , vzip4
       , vzip5
       , vunzip4
       , edgeTrigger
       , tfold
       , tfoldD
       ) where

import Language.Haskell.TH
import Control.Applicative
--import Data.Tuple.Select
import Unsafe.Coerce
import CLaSH.Prelude

instance Show Loc where
        showsPrec _ l =   (loc_filename l ++) . (':':)
                        . shows (fst $ loc_start l) . (':':)
                        . shows (snd $ loc_start l) . (':':)

{- A Template Haskell string literal with the location of the invocation of
 - showCodeLoc
 -}

showCodeLoc = do loc <- location
                 litE $ StringL (show loc)

-- Shorthand for creating an integer literal

intLit :: Integer -> ExpQ

intLit = litE . integerL

{- Construct an Unsigned n type with n just large enough to be able to
 - represent the number i
 -}

uToFit :: Integer -> TypeQ

uToFit i = (appT (conT ''Unsigned)
                 (  litT $ numTyLit $ toInteger $ max 1 $ floor
                  $ 1 + logBase 2 (fromInteger i)))

{- Convert an Integer expression to a constant of type "Unsigned n", with n
 - being just large enough to contain the value
 -}

fitU :: Integer -> ExpQ

fitU i = sigE (intLit i) (uToFit i)

{- Construct a vector from a list of signal names
 - Example: $(vS ['s1, 's2, 's3]) is a vector of the three signals s1, s2 and
 - s3.
 -}
vS :: [Name] -> ExpQ

vS [] = [| pure Nil |]
vS (x:xs) = [| liftA2 (:>) $(varE x) $(vS xs) |]

vzip3 as bs cs = (vzipWith (\a (b,c) -> (a,b,c)) as) (vzip bs cs)
vzip4 as bs cs ds = (vzipWith (\a (b,c,d) -> (a,b,c,d)) as) (vzip3 bs cs ds)
vzip5 as bs cs ds es
    = (vzipWith (\a (b,c,d,e) -> (a,b,c,d,e)) as) (vzip4 bs cs ds es)

vunzip4 :: Vec n (a,b,c,d) -> (Vec n a, Vec n b, Vec n c, Vec n d)
vunzip4 xs = (as, bs, cs, ds)
    where
--        as = vmap (sel1) xs
--        bs = vmap (sel2) xs
--        cs = vmap (sel3) xs
--        ds = vmap (sel4) xs
        as = vmap (\(a, b, c, d) -> a) xs
        bs = vmap (\(a, b, c, d) -> b) xs
        cs = vmap (\(a, b, c, d) -> c) xs
        ds = vmap (\(a, b, c, d) -> d) xs

-- Output True when input changes
edgeTrigger :: Eq a
            => a
            -> a
            -> (a, Bool)

edgeTrigger s i = (i, s /= i)

tfold :: Pack a
      => (a -> a -> a)
      -> a
      -> SignalP (a, Bool)
      -> SignalP a

tfold f z = tfold' f z <^> z
tfold' f z s (x1, start) = (s', s')
    where
        x0 | start     = z
           | otherwise = s
        s' = f x0 x1

tfoldD :: Pack a
       => (a -> a -> a)
       -> a
       -> SignalP (a, Bool)
       -> SignalP a

tfoldD f z = tfoldD' f z <^> z
tfoldD' f z s (x1, start) = (s', s)
    where
        x0 | start     = z
           | otherwise = s
        s' = f x0 x1

