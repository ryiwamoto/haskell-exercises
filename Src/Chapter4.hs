module Src.Chapter4
    (
      tri_number
    , tetration
    , index
    , even_odd
    , insert
    , isort
    , part_num
    ) where

    -- (1.1)
    tri_number :: Int -> Int
    tri_number 0 = 0
    tri_number n = n + tri_number (n - 1)

    -- (1.2)
    tetration :: Integer -> Integer -> Integer
    tetration x 0 = 1
    tetration n m = n ^ tetration n (m - 1)

    -- (1.3)
    index :: Int -> [a] -> a
    index n [] = error "empty"
    index n xs | n >= 0 && n > length xs = error "out of range"
    index n xs = xs !! n

    -- (1.4) 
    even_odd :: [Int] -> ([Int], [Int])
    even_odd [] = ([], [])
    even_odd (x:xs) 
        | x `mod` 2 == 0 = (odds, x:evens) --cons
        | otherwise = (x:odds, evens)
        where (odds, evens) = even_odd xs --whereでパターンマッチができる

    -- (2.1)
    insert :: Ord a => [a] -> a -> [a]
    insert [] y = [y]
    insert (x:xs) y
        | x > y = y:x:xs
        | otherwise = x: insert xs y

    -- (2.2)
    isort :: Ord a => [a] -> [a]
    isort [] = []
    isort (x:xs) = insert (isort xs) x

    --- (3)
    part_num :: Int -> Int
    part_num n = undefined {- Rewrite HERE! -}
