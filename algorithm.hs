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
				  | _checkIfExist xs n == True = 'H' : _changeAtBasic xs (n-1) ys
				  | otherwise = y : _changeAtBasic xs (n-1) ys

-- processing provided tuple into an index of table number
_processNo :: (Int,Int) -> Int
_processNo (n,t) = 6 * n + t

-- board creation 
_board :: Int -> Char -> [Char]
_board 0 _ = []
_board n x = x : _board (n-1) x

-- ELEMENTS PROVIDING POSIBILITY TO SHOW HOUSES BOARD
-- get 'length' elements of the list
_getList :: Int -> [Char] -> [Char]
_getList _ [] = []
_getList length (x:xs) 
				  | (length > 0) = x : _getList (length - 1) xs
				  | otherwise = _getList (length - 1) xs

-- show board by index: 
-- _showBoardByIndex index iterator array
_showBoardByIndex :: Int -> Int -> [Char] -> [Char]
_showBoardByIndex _ _ [] = []
_showBoardByIndex index iterator (x:xs) 
				  | index == iterator = (_getList 6 (x:xs))
				  |	otherwise = _showBoardByIndex index (iterator + 1) xs				  

-- if at the beginning is 0 than all row will be erased
_ereaseIf0AtRow :: Int -> [Char] -> [Char]
_ereaseIf0AtRow _ [] = []
_ereaseIf0AtRow n (x:xs) 
				  | n == 0 = 'X' : _ereaseIf0AtRow n xs
				  | otherwise = x : _ereaseIf0AtRow n xs
				  
-- if at the beginning of column is  0 than column will be erased
_ereaseIf0AtCol :: [Int] -> Int -> [Char] -> [Char]
_ereaseIf0AtCol _ _ [] = [] 
_ereaseIf0AtCol (x:xs) index (y:ys) 
				  | x == 0 = 'X' : _ereaseIf0AtCol xs (index + 1) ys
				  | otherwise = y : _ereaseIf0AtCol xs (index + 1) ys
				  
main = do
	putStrLn "1 - 1 - 2 - 1 - 1 - 1"
	putStrLn ( unlines ["1","0","2","1","2","1"])
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 1 (_showBoardByIndex 0 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 0 (_showBoardByIndex 6 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 2 (_showBoardByIndex 12 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 1 (_showBoardByIndex 18 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 2 (_showBoardByIndex 24 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 1 (_showBoardByIndex 30 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))))))
	
	