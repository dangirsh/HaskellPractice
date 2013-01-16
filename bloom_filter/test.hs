{-# LANGUAGE TemplateHaskell #-}

import BloomFilter
import BloomFilter.BitArray
import BloomFilter.Hash
import Test.QuickCheck
import Test.QuickCheck.All

runTests = $quickCheckAll

fromList :: Int -> Int -> [String] -> Bloom
fromList m k = foldl insert (emptyFilter m k)

prop_check_insert xs =
    forAll (choose (10, 1000)) $ \m ->
        forAll (choose (1, m)) $ \k ->
            all (contains (fromList m k xs)) xs

