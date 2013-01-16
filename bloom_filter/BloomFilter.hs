-- Dan Girshovich, 1/13
-- Extremely basic and immutable bloom filter implementation.

-- m: # of bits in bit array
-- k: # of hash functions


module BloomFilter (
    emptyFilter,
    makeHashFuncs,
    insert,
    contains,
    Bloom
) where

    import BloomFilter.BitArray
    import BloomFilter.Hash

    -- ask: record use / pattern matching best practices
    data Bloom = Bloom {bitArray :: BitArray, hashFuncs :: [HashFunc]}

    -- ask: eta reduction best practice for type contructors
    emptyFilter :: Int -> Int -> Bloom
    emptyFilter m k = Bloom (emptyArray m) (makeHashFuncs k)

    insert :: Bloom -> String -> Bloom
    insert Bloom {bitArray = ba, hashFuncs = hfs} s =
        Bloom (setBits indicies ba) hfs
        where
            indicies = map (flip mod (length ba)) hashes
            hashes = map (flip ($) s) hfs

    -- false positives possible!
    contains :: Bloom -> String -> Bool
    contains Bloom {bitArray = ba, hashFuncs = hfs} s =
        checkBits indicies ba
        where
            indicies = map (flip mod (length ba)) hashes
            hashes = map (flip ($) s) hfs