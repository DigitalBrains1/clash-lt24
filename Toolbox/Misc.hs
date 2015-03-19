{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-
 - Copyright (c) 2015, Peter Lebbing <peter@digitalbrains.com>
 - All rights reserved.
 - 
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 - 
 - 1. Redistributions of source code must retain the above copyright notice,
 - this list of conditions and the following disclaimer.
 - 
 - 2. Redistributions in binary form must reproduce the above copyright notice,
 - this list of conditions and the following disclaimer in the documentation
 - and/or other materials provided with the distribution.
 - 
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 - AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 - IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 - ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 - LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 - CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 - SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 - INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 - CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 - ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 - POSSIBILITY OF SUCH DAMAGE.
 -}

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
       , vv
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

vv :: Lift a => [[a]] -> ExpQ

vv []     = [| Nil |]
vv (e:es) = [| $(v e) :> $(vv es) |]
