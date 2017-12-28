

_zip :: [a] -> [b] -> [(a,b)]
_zip _ [] = []
_zip [] _ = []
_zip (x:xs) (y:ys) = (x,y) : _zip xs ys

--maximum (map fst [(1,2),(3,4)])

--maximum (map snd [(1,2),(3,4)])

_changeAt :: (Eq i, Num i) => i -> [Char] -> [Char]
_changeAt _ [] = []
_changeAt n (x:xs) 
                  | n == 0 = 'h' : _changeAt (n-1) xs
                  | otherwise = x : _changeAt (n-1) xs

_processNo :: (Int,Int) -> Int
_processNo (n,t) = 6 * (n - 1) + t - 1

--_changeAt (_processno (1,2)) ['x','x','x','x','x','x']