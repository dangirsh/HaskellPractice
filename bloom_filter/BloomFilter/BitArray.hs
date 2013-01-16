-- Dan Girshovich, 1/13
-- Basic bit array

-- ask, namespacing for empty (emptyArray)
module BloomFilter.BitArray (
    emptyArray,
    setBits,
    checkBits,
    BitArray
) where

    -- ask: type synonym best practices
    type Bit = Bool

    type BitArray = [Bit]

    type Index = Int

    emptyArray :: Int -> BitArray
    emptyArray m = replicate m False

    -- ask: eta reduction semantics & best practices
    setBits :: [Index] -> BitArray -> BitArray
    setBits is ba = map (\(i, b) -> b || (elem i is)) $ zip [0..] ba

    -- True if all bits at given indicies are true
    checkBits :: [Index] -> BitArray -> Bool
    checkBits is ba = all id $ map (getBit ba) is

    getBit :: BitArray -> Index -> Bool
    getBit = (!!)