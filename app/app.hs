{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import BigO
import CoreSyn
import qualified Data.List as List
import DynFlags
import GHC
import Prelude

coreTest =
  runGhc
    (Just "/Users/tonyday/.stack/programs/x86_64-osx/ghc-8.10.4/lib/ghc-8.10.4")
    ( do
        fs <- getProgramDynFlags
        _ <- setProgramDynFlags fs
        compileToCoreSimplified "app/coretest.hs"
    )

main :: IO ()
main = pure ()

-- stack build --force-dirty --ghc-options="-ddump-simpl -ddump-to-file -fforce-recomp"
-- git clone --recurse-submodules git://github.com/ghc/ghc
