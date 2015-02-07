module Src.Chapter6
    (
      sortByFrequency
    , initialMap
    , infixPalindromicNumber
    , vernam
    ) where
    import Data.Ord
    import Data.List
    import qualified Data.Map as Map
    import Data.Char
    import Data.Bits

    -- (1.1)
    sortByFrequency :: Ord a => [a] -> [a]
    sortByFrequency = map head . sortBy (flip $ comparing length) . group . sort

    -- (1.2)
    initialMap :: [String] -> Map.Map Char [String]
    -- initialMap ss = undefined {- Rewrite HERE! -}
    initialMap = Map.fromList . Data.List.map (\x -> (head $ head x, x)) . groupBy (\x y -> head x  ==  head y) . sortBy (comparing $ Down . length) . Data.List.filter ((<) 0 . length)

    -- (1.3)
    infixPalindromicNumber :: Int -> Int
    infixPalindromicNumber n  =  head [m | m <- [0..], let m'  =  show m, reverse m'  =  =  m', show n `isInfixOf` m'] --解答を見た

    -- (2)
    vernam :: String -> String -> String
    vernam k s = undefined {- Rewrite HERE! -}
