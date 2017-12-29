import Data.List

_zip :: [a] -> [b] -> [(a,b)]
_zip _ [] = []
_zip [] _ = []
_zip (x:xs) (y:ys) = (x,y) : _zip xs ys

--maximum (map fst [(1,2),(3,4)])

{- --non-used old method
_changeAt :: (Eq i, Num i) => i -> [Char] -> [Char]
_changeAt _ [] = []
_changeAt n (x:xs) 
                  | n == 0 = 'h' : _changeAt (n-1) xs
                  | otherwise = x : _changeAt (n-1) xs
-}

_checkIfExist :: [Int] -> Int -> Bool
_checkIfExist [] _ = False
_checkIfExist (x:xs) n 
                       | x == n = True
					   | otherwise = _checkIfExist xs n


		-- [Lista miejsc do zmiany] -> maksymalny elem na liscie ->
		-- [Lista charów zmienianych] -> wynik (board) 
_changeAtBasic :: [Int] -> Int -> [Char] -> [Char]
_changeAtBasic _ _ [] = [] 
_changeAtBasic xs n (y:ys)
				  | _checkIfExist xs n == True = 'h' : _changeAtBasic xs (n-1) ys
				  | otherwise = y : _changeAtBasic xs (n-1) ys


_processNo :: (Int,Int) -> Int
_processNo (n,t) = 6 * (n - 1) + t - 1

_board :: Int -> char -> [char]
_board 0 _ = []
_board n x = x : _board (n-1) x



main = do
	--putStrLn (_changeAt (_processNo (1,2)) (_board 36 'x'))
	
	putStrLn (reverse (_changeAtBasic (map _processNo [(1,2),(1,3),(5,1)]) 36 (_board 36 'x')))
	
	