module Simul.Common
       ( module Simul.Common
       , module CLaSH.Prelude
       ) where

import qualified Text.Show.Pretty as Pr
import CLaSH.Prelude

pretty :: Show a => a -> IO ()
pretty = putStrLn . Pr.ppShow

