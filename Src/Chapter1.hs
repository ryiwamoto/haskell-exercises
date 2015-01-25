module Src.Chapter1
    (
      manlen
    , points
    , mancircle
    ) where

    -- (1)
    manlen :: (Int, Int) -> (Int, Int) -> Int
    manlen (a, b) (c, d) = abs(a - c) + abs(b - d)

    -- (2)
    points :: Int -> [(Int, Int)]
    points a = [(c, d) | c <- [-1 * a..a], d <- [-1 * a..a]]

    -- (3)
    mancircle :: Int -> [(Int, Int)]
    mancircle a = [(c, d) | (c, d) <- points a, manlen(0, 0)(c, d) == a]
