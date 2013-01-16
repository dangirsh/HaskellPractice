-- Dan Girshovich, 1/13
-- Hash functions for bloom filters

module BloomFilter.Hash (
    makeHashFuncs,
    getHashes,
    HashFunc
) where

    import Data.Char (chr)
    import Data.HashTable (hashString)

    type Hash = Int

    type HashFunc = String -> Hash

    -- ask: why!
    makeHashFuncs :: Int -> [HashFunc]
    makeHashFuncs k = map (\i -> hashFunc . (seeder i)) [1..k]
        where
            hashFunc = fromIntegral . hashString
            seeder = (:) . chr

    getHashes :: String -> [HashFunc] -> [Hash]
    getHashes s = map (flip ($) s)
