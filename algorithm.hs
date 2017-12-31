import Data.List

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
_getListChar :: Int -> [Char] -> [Char]
_getListChar _ [] = []
_getListChar length (x:xs) 
				  | (length > 0) = x : _getListChar (length - 1) xs
				  | otherwise = _getListChar (length - 1) xs

-- get 'length' elements of the list
_getListInt :: Int -> [Int] -> [Int]
_getListInt _ [] = []
_getListInt length (x:xs) 
				  | (length > 0) = x : _getListInt (length - 1) xs
				  | otherwise = _getListInt (length - 1) xs
				  
-- show board by index: 
-- _showBoardByIndexChar index iterator array
_showBoardByIndexChar :: Int -> Int -> [Char] -> [Char]
_showBoardByIndexChar _ _ [] = []
_showBoardByIndexChar index iterator (x:xs) 
				  | index == iterator = (_getListChar 6 (x:xs))
				  |	otherwise = _showBoardByIndexChar index (iterator + 1) xs			

_showBoardByIndexInt :: Int -> Int -> [Int] -> [Int]
_showBoardByIndexInt _ _ [] = []
_showBoardByIndexInt index iterator (x:xs) 
				  | index == iterator = (_getListInt 6 (x:xs))
				  |	otherwise = _showBoardByIndexInt index (iterator + 1) xs				  
				  

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

-- creation of number board 				
_numberBoard :: Int -> [Int]
_numberBoard n =  sub 0
			where sub i | i >= n =  []
						| otherwise = 0 : sub (i+1)
						
						-- list numberBoard | list of house indexes
_placeHousesOnBoard :: [Int] -> [Int] -> Int -> [Int]
_placeHousesOnBoard [] _ _ = []
_placeHousesOnBoard (x:xs) ys index	
						| (_checkIfExist ys index) = (3) : _placeHousesOnBoard xs ys (index + 1)
						| otherwise = x : _placeHousesOnBoard xs ys (index + 1)

_makeHousesIndexList :: [Char] -> Int -> [Int]
_makeHousesIndexList [] _ = []
_makeHousesIndexList (x:xs) index 
								| x == 'H' = index : _makeHousesIndexList xs (index + 1)
								| otherwise = _makeHousesIndexList xs (index + 1)
--_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0
--_placeHousesOnBoard (_numberBoard 36) (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0) 0

_increaseListByFittingCells :: [Int] -> [Int]
_increaseListByFittingCells [] = []
_increaseListByFittingCells (x:xs) = [x - 6, x - 1, x, x + 1, x + 6] ++ _increaseListByFittingCells xs
--_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)
--_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0
--_ereaseIfNotFitting (_showBoardByIndexInt 12 0 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 1 (_showBoardByIndexChar 12 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))))))

-- ereasing elements 
_ereaseIfNotFitting :: [Int] -> [Char] -> [Char]
_ereaseIfNotFitting [] [] = []
_ereaseIfNotFitting _ [] = []
_ereaseIfNotFitting (x:xs) (y:ys) 
								| x == 0 = 'X' : _ereaseIfNotFitting xs ys
								| otherwise = y : _ereaseIfNotFitting xs ys 
								
main = do
	--putStrLn "1 - 1 - 2 - 1 - 1 - 1"
	--putStrLn ( unlines ["1","0","2","1","2","1"])
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_showBoardByIndexInt 0 0 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_showBoardByIndexChar 0 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 0 (_ereaseIfNotFitting (_showBoardByIndexInt 6 0 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_showBoardByIndexChar 6 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 2 (_ereaseIfNotFitting (_showBoardByIndexInt 12 0 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_showBoardByIndexChar 12 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_showBoardByIndexInt 18 0 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_showBoardByIndexChar 18 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 2 (_ereaseIfNotFitting (_showBoardByIndexInt 24 0 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_showBoardByIndexChar 24 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_showBoardByIndexInt 30 0 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_showBoardByIndexChar 30 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))

	{-
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 1 (_showBoardByIndexChar 0 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 0 (_showBoardByIndexChar 6 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 2 (_showBoardByIndexChar 12 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 1 (_showBoardByIndexChar 18 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 2 (_showBoardByIndexChar 24 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))))))
	putStrLn (_ereaseIf0AtCol [1,1,2,1,1,1] 0 (_ereaseIf0AtRow 1 (_showBoardByIndexChar 30 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))))))
	-}	
	