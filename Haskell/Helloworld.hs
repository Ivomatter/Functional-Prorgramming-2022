sum' :: Int -> Int -> Int
sum' a b = a + b


doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = x*2 + y*2

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x < 100
                        then doubleMe x
                        else x


doubleLargeNumber x = if x > 100
                        then doubleMe x
                        else x

lostNumbers :: [Integer]
lostNumbers = [4,8,15,16,23,42]




        
