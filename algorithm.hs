import Data.List

--maximum (map fst [(1,2),(3,4)])
{- --non-used old method
_changeAt :: (Eq i, Num i) => i -> [Char] -> [Char]
_changeAt _ [] = []
_changeAt n (x:xs) 
                  | n == 0 = 'h' : _changeAt (n-1) xs
                  | otherwise = x : _changeAt (n-1) xs
-}

-- check if in table exist provided item
_checkIfExist :: [Int] -> Int -> Bool
_checkIfExist [] _ = False
_checkIfExist (x:xs) n 
                       | x == n = True
					   | otherwise = _checkIfExist xs n

	-- [Lista miejsc do zmiany] -> maksymalny elem na liscie ->
	-- [Lista charów zmienianych] -> wynik (board) 
-- placing houses on provided char list of elements
_changeAtBasic :: [Int] -> Int -> [Char] -> [Char]
_changeAtBasic _ _ [] = [] 
_changeAtBasic xs n (y:ys)
				  | _checkIfExist xs n == True = 'h' : _changeAtBasic xs (n-1) ys
				  | otherwise = y : _changeAtBasic xs (n-1) ys

-- processing provided tuple into an index of table number
_processNo :: (Int,Int) -> Int
_processNo (n,t) = 6 * n + t

-- board creation 
_board :: Int -> Char -> [Char]
_board 0 _ = []
_board n x = x : _board (n-1) x

-- ELEMENTS PROVIDING POSIBILITY TO SHOW HOUSES BOARD
-- show board by index: 
-- _showBoardByIndex index iterator array
_showBoardByIndex :: Int -> Int -> [Char] -> [Char]
_showBoardByIndex _ _ [] = []
_showBoardByIndex index iterator (x:xs) 
				  | index == iterator = (_getList 6 (x:xs))
				  |	otherwise = _showBoardByIndex index (iterator + 1) xs
				  
-- get 'length' elements of the list
_getList :: Int -> [Char] -> [Char]
_getList _ [] = []
_getList length (x:xs) 
				  | (length > 0) = x : _getList (length - 1) xs
				  | otherwise = _getList (length - 1) xs
			

main = do
	putStrLn (_showBoardByIndex 0 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 'x'))))
	putStrLn (_showBoardByIndex 6 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 'x'))))
	putStrLn (_showBoardByIndex 12 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 'x'))))
	putStrLn (_showBoardByIndex 18 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 'x'))))
	putStrLn (_showBoardByIndex 24 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 'x'))))
	putStrLn (_showBoardByIndex 30 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 'x'))))
	