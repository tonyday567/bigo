{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Prelude
import BigO
import qualified Data.List as List
import CoreSyn
import GHC
import DynFlags

coreTest = runGhc
  (Just "/Users/tonyday/.stack/programs/x86_64-osx/ghc-8.10.4/lib/ghc-8.10.4")
  (do
      fs <- getProgramDynFlags
      _ <- setProgramDynFlags fs
      compileToCoreSimplified "app/coretest.hs")

main :: IO ()
main = pure ()

-- stack build --force-dirty --ghc-options="-ddump-simpl -ddump-to-file -fforce-recomp" 
-- git clone --recurse-submodules git://github.com/ghc/ghc

