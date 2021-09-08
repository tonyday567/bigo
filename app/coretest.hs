{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import BigO
import Prelude

main :: IO ()
main =
  -- print =<< bigOT (\x -> List.nub [0 .. (x - 1)]) 10000
  print $ (\x -> sum [0 :: Integer .. (x - 1)]) 10000
