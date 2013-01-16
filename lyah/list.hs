{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck.All
import Test.QuickCheck.Function
import Test.QuickCheck
import Data.List

runTests = $quickCheckAll

prop_maximum_check xs = not (null xs) ==> maximum' xs == maximum xs
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "No max of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)


prop_take_check n xs = take' n xs == take n xs
take' :: Int -> [b] -> [b]
take' _ [] = []
take' n (x:xs) = if n <= 0 then [] else x : take (n-1) xs


--prop_repeat_check x = repeat' x == repeat x
repeat' :: a -> [a]
repeat' x = x : repeat' x


--prop_replicate_check n v = replicate' n v == replicate n v
replicate' :: Int -> b -> [b]
replicate' n v = take' n (repeat' v)


prop_reverse_check xs = reverse' xs == reverse xs
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]


prop_zip_check xs ys = zip' xs ys == zip xs ys
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys


prop_elem_check x xs = elem' x xs == elem x xs
elem' :: (Eq a) => a -> [a] -> Bool
elem' x = any' (== x)


prop_notElem_check x xs = notElem' x xs == notElem x xs
notElem' :: (Eq a) => a -> [a] -> Bool
notElem' x = all' (/= x)


--prop_zipWith_check f xs ys = zipWith' f xs ys == zipWith f xs ys
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)


and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && (and' xs)


or' :: [Bool] -> Bool
or' [] = False
or' (x:xs)
  | x     = True
  | otherwise = or' xs


any' :: (a -> Bool) -> [a] -> Bool
any' p = or' . map p


all' :: (a -> Bool) -> [a] -> Bool
all' p = and' . map p


--prop_map_check f xs = map' f xs == map f xs
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : (map' f xs)


--prop_filter_check p xs = filter' p xs == filter p xs
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x   = x : (filter' p xs)
  | otherwise = filter' p xs


--prop_takeWhile_check p xs = takeWhile' p xs == takeWhile p xs
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x   = x : (takeWhile' p xs)
  | otherwise = []


--prop_foldl_check f acc xs = foldl'' f acc xs == foldl f acc xs
foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' _ acc [] = acc
foldl'' f acc (x:xs) = foldl'' f (f acc x) xs


--prop_foldr_check f acc xs = foldr' f acc xs == foldr f acc xs
foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)


--prop_scanl_check f acc xs = scanl' f acc xs == scanl f acc xs
scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' _ acc [] = [acc]
scanl' f acc (x:xs) = acc : (scanl' f (f acc x) xs)


--prop_scanr_check f acc xs = scanr' f acc xs == scanr f acc xs
scanr' :: (b -> a -> a) -> a -> [b] -> [a]
scanr' _ acc [] = [acc]
scanr' f acc (x:xs) = f x (head tailAccList) : tailAccList
  where
    tailAccList = scanr' f acc xs


--prop_dropWhile_check p xs = dropWhile' p xs == dropWhile p xs
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
  | p x   = dropWhile' p xs
  | otherwise = (x:xs)



--prop_span_check p xs = span' p xs == span p xs
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' p xs = (takeWhile' p xs, dropWhile' p xs)


--prop_break_check p xs = break' p xs == break p xs
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p = span' (not . p)


prop_group_check xs = group' xs == group xs
group' :: (Eq a) => [a] -> [[a]]
group' []  = []
group' (x:xs) = (\(a, b) -> a : group' b) $ span (== x) (x:xs)


prop_init_check xs = not (null xs) ==> init' xs == init xs
init' :: [a] -> [a]
init' [] = error "No init of empty list"
init' [_] = []
init' (x:xs) = x : (init' xs)


prop_inits_check xs = inits' xs == inits xs
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' xs = (inits' (init' xs)) ++ [xs]


prop_tail_check xs = not (null xs) ==> tail' xs == tail xs
tail' :: [a] -> [a]
tail' [] = error "No tail of empty list"
tail' (_:xs) = xs


prop_tails_check xs = tails' xs == tails xs
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' xs = xs : (tails' (tail' xs))


prop_intersperse_check y xs = intersperse' y xs == intersperse y xs
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' y (x:xs) = x : y : (intersperse' y xs)


prop_concat_check xs = concat' xs == concat xs
concat' :: [[a]] -> [a]
concat' [] = []
concat' [[]] = []
concat' (x:xs) = x ++ (concat' xs)


concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = concat' . map f


iterate' :: (a -> a) -> a -> [a]
iterate' f a = a : iterate' f (f a)


splitAt' :: Int -> [b] -> ([b], [b])
splitAt' i xs = (take i xs, drop i xs)


intercalate' :: [a] -> [[a]] -> [a]
intercalate' xs = concat' . intersperse' xs


isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' xs ys = (take (length xs) ys) == xs


isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' xs ys = (drop (length ys - length xs) ys) == xs


isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
isInfixOf' xs ys = any (isPrefixOf' xs) (tails' ys)


partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p xs = (filter p xs, filter (not . p) xs)
