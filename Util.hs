module Util (powerMod) where
-- Returns (b^e) mod m
powerMod :: (Integral a, Integral b) => a -> b -> a -> a
powerMod b e m 
    | e == 0 = 1
    | odd e = ( b * (powerMod (temp) (e `div` 2) m)) `mod` m 
    | otherwise = (powerMod (temp) (e `div` 2) m)
    where temp = (b * b) `mod` m