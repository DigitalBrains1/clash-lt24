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

module Toolbox.PackInstances() where

import CLaSH.Prelude
import Control.Applicative

instance Pack (a,b,c,d,e,f,g,h,i) where
  type SignalP (a,b,c,d,e,f,g,h,i) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i)
  pack (a,b,c,d,e,f,g,h,i) = (,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i
  unpack tup               = (fmap (\(x,_,_,_,_,_,_,_,_) -> x) tup
                             ,fmap (\(_,x,_,_,_,_,_,_,_) -> x) tup
                             ,fmap (\(_,_,x,_,_,_,_,_,_) -> x) tup
                             ,fmap (\(_,_,_,x,_,_,_,_,_) -> x) tup
                             ,fmap (\(_,_,_,_,x,_,_,_,_) -> x) tup
                             ,fmap (\(_,_,_,_,_,x,_,_,_) -> x) tup
                             ,fmap (\(_,_,_,_,_,_,x,_,_) -> x) tup
                             ,fmap (\(_,_,_,_,_,_,_,x,_) -> x) tup
                             ,fmap (\(_,_,_,_,_,_,_,_,x) -> x) tup
                             )

instance Pack (a,b,c,d,e,f,g,h,i,j) where
  type SignalP (a,b,c,d,e,f,g,h,i,j) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j)
  pack (a,b,c,d,e,f,g,h,i,j) = (,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j
  unpack tup                 = (fmap (\(x,_,_,_,_,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,x,_,_,_,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,x,_,_,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,x,_,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,x,_,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,x,_,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,_,x,_,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,_,_,x,_,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,_,_,_,x,_) -> x) tup
                               ,fmap (\(_,_,_,_,_,_,_,_,_,x) -> x) tup
                               )

instance Pack (a,b,c,d,e,f,g,h,i,j,k) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k)
  pack (a,b,c,d,e,f,g,h,i,j,k) = (,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k
  unpack tup                   = (fmap (\(x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                 ,fmap (\(_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                 ,fmap (\(_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                 ,fmap (\(_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                 ,fmap (\(_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                 ,fmap (\(_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                 ,fmap (\(_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                 ,fmap (\(_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                 ,fmap (\(_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                 ,fmap (\(_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                 ,fmap (\(_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                 )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l)
  pack (a,b,c,d,e,f,g,h,i,j,k,l) = (,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l
  unpack tup                     = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                   ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                   ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                   ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                   ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                   ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                   ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                   ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                   ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                   ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                   ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                   ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                   )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m) = (,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m
  unpack tup                       = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                     ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                     ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                     ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                     ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                     ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                     ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                     ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                     ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                     ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                     )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m, Signal n)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = (,,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n
  unpack tup                         = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                       ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                       ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                       ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                       ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                       ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                       ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                       ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                       ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                       ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                       )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m, Signal n, Signal o)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = (,,,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o
  unpack tup                           = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                         ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                         ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                         ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                         ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                         ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                         ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                         ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                         ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                         ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                         )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m, Signal n, Signal o, Signal p)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = (,,,,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o <*> p
  unpack tup                             = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                           ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                           ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                           ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                           ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                           ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                           ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                           ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                           ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                           ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                           )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m, Signal n, Signal o, Signal p, Signal q)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) = (,,,,,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o <*> p <*> q
  unpack tup                               = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                             ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                             ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                             ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                             ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                             ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                             ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                             ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                             ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                             ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                             )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m, Signal n, Signal o, Signal p, Signal q, Signal r)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) = (,,,,,,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o <*> p <*> q <*> r
  unpack tup                                 = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                               ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                               ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                               ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                               ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                               ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                               ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                               ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                               ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                               ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                               )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m, Signal n, Signal o, Signal p, Signal q, Signal r, Signal s)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) = (,,,,,,,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o <*> p <*> q <*> r <*> s
  unpack tup                                   = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                                 ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                                 )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m, Signal n, Signal o, Signal p, Signal q, Signal r, Signal s, Signal t)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) = (,,,,,,,,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o <*> p <*> q <*> r <*> s <*> t
  unpack tup                                     = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                                   ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                                   )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m, Signal n, Signal o, Signal p, Signal q, Signal r, Signal s, Signal t, Signal u)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) = (,,,,,,,,,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o <*> p <*> q <*> r <*> s <*> t <*> u
  unpack tup                                       = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                                     ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                                     )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m, Signal n, Signal o, Signal p, Signal q, Signal r, Signal s, Signal t, Signal u, Signal v)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) = (,,,,,,,,,,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o <*> p <*> q <*> r <*> s <*> t <*> u <*> v
  unpack tup                                         = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                                       ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                                       )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m, Signal n, Signal o, Signal p, Signal q, Signal r, Signal s, Signal t, Signal u, Signal v, Signal w)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) = (,,,,,,,,,,,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o <*> p <*> q <*> r <*> s <*> t <*> u <*> v <*> w
  unpack tup                                           = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                                         ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                                         )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m, Signal n, Signal o, Signal p, Signal q, Signal r, Signal s, Signal t, Signal u, Signal v, Signal w, Signal x)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) = (,,,,,,,,,,,,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o <*> p <*> q <*> r <*> s <*> t <*> u <*> v <*> w <*> x
  unpack tup                                             = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                                           ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                                           )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m, Signal n, Signal o, Signal p, Signal q, Signal r, Signal s, Signal t, Signal u, Signal v, Signal w, Signal x, Signal y)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y) = (,,,,,,,,,,,,,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o <*> p <*> q <*> r <*> s <*> t <*> u <*> v <*> w <*> x <*> y
  unpack tup                                               = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                                             ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                                             )

instance Pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) where
  type SignalP (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) = (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f, Signal g, Signal h, Signal i, Signal j, Signal k, Signal l, Signal m, Signal n, Signal o, Signal p, Signal q, Signal r, Signal s, Signal t, Signal u, Signal v, Signal w, Signal x, Signal y, Signal z)
  pack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) = (,,,,,,,,,,,,,,,,,,,,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o <*> p <*> q <*> r <*> s <*> t <*> u <*> v <*> w <*> x <*> y <*> z
  unpack tup                                                 = (fmap (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) tup
                                                               ,fmap (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) tup
                                                               )

