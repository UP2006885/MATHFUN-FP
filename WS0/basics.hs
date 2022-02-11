-- run ghci
-- :load basics.hs
-- mult2 x
-- To quit do :q

mult2 :: Int -> Int
mult2 x = 2 * x

mult4 :: Int -> Int
mult4 x = mult2 (mult2 x)