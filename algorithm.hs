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

-- if at the beginning is 0 than all row will be erased
_ereaseIf0AtRow :: Int -> [Char] -> [Char]
_ereaseIf0AtRow _ [] = []
_ereaseIf0AtRow n (x:xs) 
				  | n == 0 = 'X' : _ereaseIf0AtRow n xs
				  | otherwise = x : _ereaseIf0AtRow n xs
				  
-- creation of number board 				
_numberBoard :: Int -> [Int]
_numberBoard n =  sub 0
			where sub i | i >= n =  []
						| otherwise = 0 : sub (i+1)
						
						-- list numberBoard | list of house indexes
_placeHousesOnBoard :: Int -> [Int] -> [Int] -> Int -> [Int]
_placeHousesOnBoard _ [] _ _ = []
_placeHousesOnBoard wage (x:xs) ys index	
						| (_checkIfExist ys index) = (x + wage) : _placeHousesOnBoard wage xs ys (index + 1)
						| otherwise = x : _placeHousesOnBoard wage xs ys (index + 1)

_makeHousesIndexList :: [Char] -> Int -> [Int]
_makeHousesIndexList [] _ = []
_makeHousesIndexList (x:xs) index 
								| x == 'H' = index : _makeHousesIndexList xs (index + 1)
								| otherwise = _makeHousesIndexList xs (index + 1)

-- joining int two lists generated (houses list & warmers)								
_joinLists :: [Int] -> [Int] -> [Int]
_joinLists [] [] = []
_joinLists (x:xs) (y:ys) = (x + y) : _joinLists xs ys 
--_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0
--_placeHousesOnBoard (_numberBoard 36) (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0) 0

_increaseListByFittingCells :: [Int] -> [Int]
_increaseListByFittingCells [] = []
_increaseListByFittingCells (x:xs) = [x - 6, x - 1, x, x + 1, x + 6] ++ _increaseListByFittingCells xs

-- ereasing elements using array of zeros
_ereaseIfNotFitting :: [Int] -> [Char] -> [Char]
_ereaseIfNotFitting [] [] = []
_ereaseIfNotFitting _ [] = []
_ereaseIfNotFitting (x:xs) (y:ys) 
								| x == 0 = 'X' : _ereaseIfNotFitting xs ys
								| otherwise = y : _ereaseIfNotFitting xs ys 
	
-- count elements which are '0'
_countOfElem elem list = length $ filter (\x -> x == elem) list

-- settling warmers if count of '0' is the same as number at row beginning
_changeRowIfEqual :: [Char] -> [Char]
_changeRowIfEqual [] = []
_changeRowIfEqual (x:xs) 
								| x == '0' = 'W' : _changeRowIfEqual xs
								| otherwise = x : _changeRowIfEqual xs

-- check if count of '0' is the same as number at row beginning
_checkEqualityRow :: Int -> [Char] -> [Char]
_checkEqualityRow _ [] = []
_checkEqualityRow rowNo xs
								| rowNo == (_countOfElem '0' xs) = (_changeRowIfEqual xs)
								| otherwise = xs

-- this method was provided to resolve _checkEqualityRow problem - we've got to count warmers in row and differ this count from overall number provided at the beginning of row								
_countWarmersInRow :: [Char] -> Int
_countWarmersInRow [] = 0
_countWarmersInRow (x:xs) 
								| x == 'W' = 1 + _countWarmersInRow xs
								| otherwise = 0 + _countWarmersInRow xs 
								
-- show board by index: 
-- _showBoardByIndexChar index iterator array
-- provide column/row values as a list of elements, bool to chose if column or row
_columnToRow :: Bool -> Int -> [a] -> [a]								
_columnToRow _ _ [] = []
_columnToRow rowOrCol number xs = _iteratingThroughTab rowOrCol number 0 xs

_iteratingThroughTab :: Bool -> Int ->  Int -> [a] -> [a]
_iteratingThroughTab _ _ _ [] = []						
_iteratingThroughTab rowOrCol number iterator (x:xs) 
								| (_checkIfExist (_genIndexInColList rowOrCol number) iterator) = x : _iteratingThroughTab rowOrCol number (iterator + 1) xs
								| otherwise = _iteratingThroughTab rowOrCol number (iterator + 1) xs

_genIndexInColList :: Bool -> Int -> [Int]
_genIndexInColList rowOrCol number 
								| rowOrCol == True = [ x + number | x <- [ 0, 6, 12, 18, 24, 30 ] ]
								| otherwise = [ x + number * 6 | x <- [ 0, 1, 2, 3, 4, 5 ] ]
-- end of showing board by index

-- the rows and columns imposition
-- this imposition compares & concatenates both row & column lists to obtain final result
_impositionRowsCols :: [Char] -> [Char] -> [Char]
_impositionRowsCols [] [] = []
_impositionRowsCols (x:xs) (y:ys)
								| (x == 'X' || x == 'W' || x == 'H') = x : _impositionRowsCols xs ys
								| (y == 'X' || y == 'W' || y == 'H') = y : _impositionRowsCols xs ys
								| otherwise = '0' : _impositionRowsCols xs ys

-- return element of column
_getColElem :: Int -> [Char] -> [Char]
_getColElem _ [] = []
_getColElem index xs = (take 1 (drop index xs))

-- concatenate columns 
_procCol :: Int -> [Char] -> [Char]
_procCol _ [] = []
_procCol interval xs = (take 1 xs) ++ _procCol interval (drop interval xs) 

_procColChoice :: Int -> [Char] -> [Char]
_procColChoice _ [] = []
_procColChoice column xs 
								| column == 0 = (_procCol 6 xs)
								| otherwise = (_procCol 6 (drop column xs))

-- STEP 2: 						
-- we' ve got to find out if count of settled warmers is equal to number at the beginning - if yes, other '0' fields will be 'X'						
_checkIfAllWarmersInRow :: Int -> [Char] -> [Char]
_checkIfAllWarmersInRow _ [] = []
_checkIfAllWarmersInRow number xs
								| number == (_countOfElem 'W' xs) = _changeRowIfEqualWarmers xs
								| otherwise = xs
								
_changeRowIfEqualWarmers :: [Char] -> [Char]
_changeRowIfEqualWarmers [] = []
_changeRowIfEqualWarmers (x:xs) 
								| x == '0' = 'X' : _changeRowIfEqualWarmers xs
								| otherwise = x : _changeRowIfEqualWarmers xs
								
_makeWarmersIndexList :: Int -> [Char] -> [Int]
_makeWarmersIndexList _ [] = []
_makeWarmersIndexList index (x:xs) 
								| x == 'W' = index : _makeWarmersIndexList (index + 1) xs
								| otherwise = _makeWarmersIndexList (index + 1) xs

-- provides a list of indexes of cells, which should be ereased
_ereaseByWarmersList :: Int -> [Int] -> [Int]
_ereaseByWarmersList _ [] = []
_ereaseByWarmersList interval (x:xs) = [ x | x <- [x - interval - 1, x - interval + 1, x + interval - 1, x + interval + 1] ] ++ _ereaseByWarmersList interval xs			
			
_ereaseByWarmers :: Int -> [Int] -> [Char] -> [Char] 
_ereaseByWarmers _ _ [] = []
_ereaseByWarmers index xs (y:ys) 
								| (_checkIfExist xs index) && (y == '0') = 'X' : _ereaseByWarmers (index + 1) xs ys
								| otherwise = y : _ereaseByWarmers (index + 1) xs ys

-- added cell ratio - increasing cell value in case of greater amount of heat warmer putting options:
-- zero options = 3
-- one option = 4
-- two options = 5
-- three options = 6
-- four options = 7		  index	of house place | board | value on this place					
_getIndexAndCheckOptions :: Int -> [Char] -> Int -> Int
_getIndexAndCheckOptions _ [] _ = 0
_getIndexAndCheckOptions index xs value = _getOptions index len [-6, -1, 1, 6] xs
								where len = (length xs) - 1									

_getOptions :: Int -> Int -> [Int] -> [Char] -> Int
_getOptions _ _ [] _ = 0
_getOptions index len (x:xs) ys
								| ((index + x) >= 0) && ((index + x) <= len) && (!!) ys (index + x) == '0' = 1 + _getOptions index len xs ys
								| otherwise = 0 + _getOptions index len xs ys
									
-- 									first is index, second is houses index list, third is char board
_updateAllIntBoardByCheckOptions :: Int -> [Int] -> [Char] -> [Int]
_updateAllIntBoardByCheckOptions _ [] _ = []
_updateAllIntBoardByCheckOptions _ _ [] = []
_updateAllIntBoardByCheckOptions index _ zs
								| index > (length zs) = []
_updateAllIntBoardByCheckOptions index (y:ys) zs 
								| (_checkIfExist (y:ys) index) == True = (_getIndexAndCheckOptions index zs 0) : _updateAllIntBoardByCheckOptions (index + 1) (y:ys) zs 
								| otherwise = _updateAllIntBoardByCheckOptions (index + 1) (y:ys) zs

-- now we will add to int board wages created in previous steps. Arguments: list of houses indexes, list of wages for each house, int board								
_updateWageBoardByOptions :: [Int] -> [Int] -> [Int] -> [Int]								
_updateWageBoardByOptions [] _ zs = zs
_updateWageBoardByOptions _ [] zs = zs
_updateWageBoardByOptions (x:xs) (y:ys) zs = _updateWageBoardByOptions xs ys (_iterateThroughBoard 0 x y zs)						
									
_iterateThroughBoard :: Int -> Int -> Int -> [Int] -> [Int]		
_iterateThroughBoard _ _ _ [] = [] 
_iterateThroughBoard iterator house_index wage (z:zs) 
								| iterator == house_index = (z + wage) : (_iterateThroughBoard (iterator + 1) house_index wage zs)
								| iterator == house_index - 6 = (z + wage) : (_iterateThroughBoard (iterator + 1) house_index wage zs)
								| iterator == house_index - 1 = (z + wage) : (_iterateThroughBoard (iterator + 1) house_index wage zs)
								| iterator == house_index + 1= (z + wage) : (_iterateThroughBoard (iterator + 1) house_index wage zs)
								| iterator == house_index + 6= (z + wage) : (_iterateThroughBoard (iterator + 1) house_index wage zs)
								| otherwise = z : (_iterateThroughBoard (iterator + 1) house_index wage zs)

-- now there will be used method that checks int board and char board - it will erease number if on board field will be item other than 'H' or '0'
_ereaseFromIntIfOccupied :: [Int] -> [Char] -> [Int]
_ereaseFromIntIfOccupied [] [] = []
_ereaseFromIntIfOccupied (x:xs) (y:ys) 
								| x /= 0 && ( y == '0' || y == 'H' ) = x : _ereaseFromIntIfOccupied xs ys
								| otherwise = 0 : _ereaseFromIntIfOccupied xs ys

_placeIfCellEquals4 :: [Int] -> [Char] -> [Char]
_placeIfCellEquals4 [] [] = []
_placeIfCellEquals4 (x:xs) (y:ys) 
								| x == 4 && y == '0' = 'W' : _placeIfCellEquals4 xs ys
								| otherwise = y : _placeIfCellEquals4 xs ys 

								
main = do
	putStrLn "- 1 - 1 - 2 - 1 - 1 - 1"
	putStrLn ( unlines ["1","0","2","1","2","1"])
	
	let tuple_from_file = [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]
	let left_numbers_from_file = [ 1, 0, 2, 1, 2, 1 ]
	let up_numbers_from_file = [ 1, 1, 2, 1, 1, 1 ] 
	
	let startup_char_board = (reverse (_changeAtBasic (map _processNo tuple_from_file) 35 (_board 36 '0')))
	let startup_int_board = (_placeHousesOnBoard 7 (_numberBoard 36) (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo tuple_from_file) 35 (_board 36 '0'))) 0) 0)
	let startup_int_board2 = (_placeHousesOnBoard 3 (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo tuple_from_file) 35 (_board 36 '0'))) 0)) 0)
	let joined_int_boards = _joinLists startup_int_board startup_int_board2
	let house_index_list = (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo tuple_from_file) 35 (_board 36 '0'))) 0)
	--print startup_int_board
	--print startup_int_board2
	--print joined_int_boards
	
	-- STEP0 ereasing rows and columns with 0 number 
	let rowSTEP0_0 = (_ereaseIf0AtRow 1 (_columnToRow False 0 startup_char_board))
	let rowSTEP0_1 = (_ereaseIf0AtRow 0 (_columnToRow False 1 startup_char_board))
	let rowSTEP0_2 = (_ereaseIf0AtRow 2 (_columnToRow False 2 startup_char_board))
	let rowSTEP0_3 = (_ereaseIf0AtRow 1 (_columnToRow False 3 startup_char_board))
	let rowSTEP0_4 = (_ereaseIf0AtRow 2 (_columnToRow False 4 startup_char_board))
	let rowSTEP0_5 = (_ereaseIf0AtRow 1 (_columnToRow False 5 startup_char_board))
	let concatRowsSTEP0 = rowSTEP0_0 ++ rowSTEP0_1 ++ rowSTEP0_2 ++ rowSTEP0_3 ++ rowSTEP0_4 ++ rowSTEP0_5
	
	let colSTEP0_0 = (_ereaseIf0AtRow 1 (_columnToRow True 0 startup_char_board))
	let colSTEP0_1 = (_ereaseIf0AtRow 1 (_columnToRow True 1 startup_char_board))
	let colSTEP0_2 = (_ereaseIf0AtRow 2 (_columnToRow True 2 startup_char_board))
	let colSTEP0_3 = (_ereaseIf0AtRow 1 (_columnToRow True 3 startup_char_board))
	let colSTEP0_4 = (_ereaseIf0AtRow 1 (_columnToRow True 4 startup_char_board))
	let colSTEP0_5 = (_ereaseIf0AtRow 1 (_columnToRow True 5 startup_char_board))
	let concatColsSTEP0 = colSTEP0_0 ++ colSTEP0_1 ++ colSTEP0_2 ++ colSTEP0_3 ++ colSTEP0_4 ++ colSTEP0_5
	let concatColsProcessedSTEP0 = ((_procColChoice 0 concatColsSTEP0) ++ (_procColChoice 1 concatColsSTEP0) ++ (_procColChoice 2 concatColsSTEP0) ++ (_procColChoice 3 concatColsSTEP0) ++ (_procColChoice 4 concatColsSTEP0) ++ (_procColChoice 5 concatColsSTEP0))
	
	let resultSTEP0 = (_impositionRowsCols concatRowsSTEP0 concatColsProcessedSTEP0)
	
	-- wages calculation
	let calculate_wages_housesSTEP0 = (_updateAllIntBoardByCheckOptions 0 house_index_list resultSTEP0)
	let int_board_with_wagesSTEP0 = (_updateWageBoardByOptions house_index_list calculate_wages_housesSTEP0 joined_int_boards)
	
	{-print concatRowsSTEP0
	print concatColsProcessedSTEP0
	print resultSTEP0
	print int_board_with_wagesSTEP0
	
	print " "
	print "END OF STEP 0!"
	print " " -}
	
	-- STEP1 ereasing cells which are on the edges for hauses & placing warmers if numbers at row/col beginning are the same
	-- First step of project in rows
	let rowSTEP1_0 = (_checkEqualityRow 1 (_ereaseIfNotFitting (_columnToRow False 0 int_board_with_wagesSTEP0) (_columnToRow False 0 resultSTEP0)))
	let rowSTEP1_1 = (_checkEqualityRow 0 (_ereaseIfNotFitting (_columnToRow False 1 int_board_with_wagesSTEP0) (_columnToRow False 1 resultSTEP0)))
	let rowSTEP1_2 = (_checkEqualityRow 2 (_ereaseIfNotFitting (_columnToRow False 2 int_board_with_wagesSTEP0) (_columnToRow False 2 resultSTEP0)))
	let rowSTEP1_3 = (_checkEqualityRow 1 (_ereaseIfNotFitting (_columnToRow False 3 int_board_with_wagesSTEP0) (_columnToRow False 3 resultSTEP0)))
	let rowSTEP1_4 = (_checkEqualityRow 2 (_ereaseIfNotFitting (_columnToRow False 4 int_board_with_wagesSTEP0) (_columnToRow False 4 resultSTEP0)))
	let rowSTEP1_5 = (_checkEqualityRow 1 (_ereaseIfNotFitting (_columnToRow False 5 int_board_with_wagesSTEP0) (_columnToRow False 5 resultSTEP0)))
	let concatRowsSTEP1 = rowSTEP1_0 ++ rowSTEP1_1 ++ rowSTEP1_2 ++ rowSTEP1_3 ++ rowSTEP1_4 ++ rowSTEP1_5
	
	-- First step of project in columns
	let colSTEP1_0 = (_checkEqualityRow 1 (_ereaseIfNotFitting (_columnToRow True 0 int_board_with_wagesSTEP0) (_columnToRow True 0 resultSTEP0)))
	let colSTEP1_1 = (_checkEqualityRow 1 (_ereaseIfNotFitting (_columnToRow True 1 int_board_with_wagesSTEP0) (_columnToRow True 1 resultSTEP0)))
	let colSTEP1_2 = (_checkEqualityRow 2 (_ereaseIfNotFitting (_columnToRow True 2 int_board_with_wagesSTEP0) (_columnToRow True 2 resultSTEP0)))
	let colSTEP1_3 = (_checkEqualityRow 1 (_ereaseIfNotFitting (_columnToRow True 3 int_board_with_wagesSTEP0) (_columnToRow True 3 resultSTEP0)))
	let colSTEP1_4 = (_checkEqualityRow 1 (_ereaseIfNotFitting (_columnToRow True 4 int_board_with_wagesSTEP0) (_columnToRow True 4 resultSTEP0)))
	let colSTEP1_5 = (_checkEqualityRow 1 (_ereaseIfNotFitting (_columnToRow True 5 int_board_with_wagesSTEP0) (_columnToRow True 5 resultSTEP0)))
	let concatColsSTEP1 = colSTEP1_0 ++ colSTEP1_1 ++ colSTEP1_2 ++ colSTEP1_3 ++ colSTEP1_4 ++ colSTEP1_5
	let concatColsProcessedSTEP1 = ((_procColChoice 0 concatColsSTEP1) ++ (_procColChoice 1 concatColsSTEP1) ++ (_procColChoice 2 concatColsSTEP1) ++ (_procColChoice 3 concatColsSTEP1) ++ (_procColChoice 4 concatColsSTEP1) ++ (_procColChoice 5 concatColsSTEP1))
	
	let resultSTEP1 = (_impositionRowsCols concatRowsSTEP1 concatColsProcessedSTEP1)
	
	-- wages calculation
	let calculate_wages_housesSTEP1 = (_updateAllIntBoardByCheckOptions 0 house_index_list resultSTEP1)
	let int_board_with_wagesSTEP1 = _ereaseFromIntIfOccupied (_updateWageBoardByOptions house_index_list calculate_wages_housesSTEP1 joined_int_boards) resultSTEP1
	
	{-print concatRowsSTEP1
	print concatColsProcessedSTEP1
	print resultSTEP1
	print int_board_with_wagesSTEP1
	
	print " "
	print "END OF STEP 1!"
	print " " -}
	
	-- STEP2 (we' ve got to find out if count of settled warmers is equal to number at the beginning - if yes, other '0' fields will be 'X'): 
	let rowSTEP2_0 = (_checkIfAllWarmersInRow 1 (_columnToRow False 0 resultSTEP1))
	let rowSTEP2_1 = (_checkIfAllWarmersInRow 0 (_columnToRow False 1 resultSTEP1))
	let rowSTEP2_2 = (_checkIfAllWarmersInRow 2 (_columnToRow False 2 resultSTEP1))
	let rowSTEP2_3 = (_checkIfAllWarmersInRow 1 (_columnToRow False 3 resultSTEP1))
	let rowSTEP2_4 = (_checkIfAllWarmersInRow 2 (_columnToRow False 4 resultSTEP1))
	let rowSTEP2_5 = (_checkIfAllWarmersInRow 1 (_columnToRow False 5 resultSTEP1))
	let concatRowsSTEP2 = rowSTEP2_0 ++ rowSTEP2_1 ++ rowSTEP2_2 ++ rowSTEP2_3 ++ rowSTEP2_4 ++ rowSTEP2_5
	
	let colSTEP2_0 = (_checkIfAllWarmersInRow 1 (_columnToRow True 0 resultSTEP1))
	let colSTEP2_1 = (_checkIfAllWarmersInRow 1 (_columnToRow True 1 resultSTEP1))
	let colSTEP2_2 = (_checkIfAllWarmersInRow 2 (_columnToRow True 2 resultSTEP1))
	let colSTEP2_3 = (_checkIfAllWarmersInRow 1 (_columnToRow True 3 resultSTEP1))
	let colSTEP2_4 = (_checkIfAllWarmersInRow 1 (_columnToRow True 4 resultSTEP1))
	let colSTEP2_5 = (_checkIfAllWarmersInRow 1 (_columnToRow True 5 resultSTEP1))
	let concatColsSTEP2 = colSTEP2_0 ++ colSTEP2_1 ++ colSTEP2_2 ++ colSTEP2_3 ++ colSTEP2_4 ++ colSTEP2_5
	let concatColsProcessedSTEP2 = ((_procColChoice 0 concatColsSTEP2) ++ (_procColChoice 1 concatColsSTEP2) ++ (_procColChoice 2 concatColsSTEP2) ++ (_procColChoice 3 concatColsSTEP2) ++ (_procColChoice 4 concatColsSTEP2) ++ (_procColChoice 5 concatColsSTEP2))
	
	let resultSTEP2 = (_impositionRowsCols concatRowsSTEP2 concatColsProcessedSTEP2)
	
	-- wages calculation
	let calculate_wages_housesSTEP2 = (_updateAllIntBoardByCheckOptions 0 house_index_list resultSTEP2)
	let int_board_with_wagesSTEP2 = (_updateWageBoardByOptions house_index_list calculate_wages_housesSTEP2 joined_int_boards)
	
	{-print concatRowsSTEP2
	print concatColsProcessedSTEP2 
	print resultSTEP2
	print int_board_with_wagesSTEP2
	
	print " "
	print "END OF STEP 2!"
	print " " -}
	
	-- STEP3 erease cells at the edges of warmers
	let resultSTEP3 = _ereaseByWarmers 0 (_ereaseByWarmersList 6 (_makeWarmersIndexList 0 resultSTEP2)) resultSTEP2	
	
	-- wages calculation
	let calculate_wages_housesSTEP3 = (_updateAllIntBoardByCheckOptions 0 house_index_list resultSTEP3)
	let int_board_with_wagesSTEP3 = _ereaseFromIntIfOccupied (_updateWageBoardByOptions house_index_list calculate_wages_housesSTEP3 joined_int_boards) resultSTEP3
	
	{-print "warmerList:"
	print resultSTEP3
	print int_board_with_wagesSTEP3
	
	print " "
	print "END OF STEP 3!"
	print " " -}
	
	-- STEP4 once again STEP1
	-- ereasing rows and columns with 0 number & ereasing cells which are on the edges for hauses
	let rowSTEP4_0 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow False 0 resultSTEP3)) (_ereaseIfNotFitting (_columnToRow False 0 joined_int_boards) (_columnToRow False 0 resultSTEP3)))
	let rowSTEP4_1 = (_checkEqualityRow (0 - _countWarmersInRow (_columnToRow False 1 resultSTEP3)) (_ereaseIfNotFitting (_columnToRow False 1 joined_int_boards) (_columnToRow False 1 resultSTEP3)))
	let rowSTEP4_2 = (_checkEqualityRow (2 - _countWarmersInRow (_columnToRow False 2 resultSTEP3)) (_ereaseIfNotFitting (_columnToRow False 2 joined_int_boards) (_columnToRow False 2 resultSTEP3)))
	let rowSTEP4_3 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow False 3 resultSTEP3)) (_ereaseIfNotFitting (_columnToRow False 3 joined_int_boards) (_columnToRow False 3 resultSTEP3)))
	let rowSTEP4_4 = (_checkEqualityRow (2 - _countWarmersInRow (_columnToRow False 4 resultSTEP3)) (_ereaseIfNotFitting (_columnToRow False 4 joined_int_boards) (_columnToRow False 4 resultSTEP3)))
	let rowSTEP4_5 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow False 5 resultSTEP3)) (_ereaseIfNotFitting (_columnToRow False 5 joined_int_boards) (_columnToRow False 5 resultSTEP3)))
	let concatRowsSTEP4 = rowSTEP4_0 ++ rowSTEP4_1 ++ rowSTEP4_2 ++ rowSTEP4_3 ++ rowSTEP4_4 ++ rowSTEP4_5
	
	-- ereasing rows and columns with 0 number & ereasing cells which are on the edges for hauses
	let colSTEP4_0 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow True 0 resultSTEP3)) (_ereaseIfNotFitting (_columnToRow True 0 joined_int_boards) (_columnToRow True 0 resultSTEP3)))
	let colSTEP4_1 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow True 1 resultSTEP3)) (_ereaseIfNotFitting (_columnToRow True 1 joined_int_boards) (_columnToRow True 1 resultSTEP3)))
	let colSTEP4_2 = (_checkEqualityRow (2 - _countWarmersInRow (_columnToRow True 2 resultSTEP3)) (_ereaseIfNotFitting (_columnToRow True 2 joined_int_boards) (_columnToRow True 2 resultSTEP3)))
	let colSTEP4_3 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow True 3 resultSTEP3)) (_ereaseIfNotFitting (_columnToRow True 3 joined_int_boards) (_columnToRow True 3 resultSTEP3)))
	let colSTEP4_4 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow True 4 resultSTEP3)) (_ereaseIfNotFitting (_columnToRow True 4 joined_int_boards) (_columnToRow True 4 resultSTEP3)))
	let colSTEP4_5 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow True 5 resultSTEP3)) (_ereaseIfNotFitting (_columnToRow True 5 joined_int_boards) (_columnToRow True 5 resultSTEP3)))
	let concatColsSTEP4 = colSTEP4_0 ++ colSTEP4_1 ++ colSTEP4_2 ++ colSTEP4_3 ++ colSTEP4_4 ++ colSTEP4_5
	let concatColsProcessedSTEP4 = ((_procColChoice 0 concatColsSTEP4) ++ (_procColChoice 1 concatColsSTEP4) ++ (_procColChoice 2 concatColsSTEP4) ++ (_procColChoice 3 concatColsSTEP4) ++ (_procColChoice 4 concatColsSTEP4) ++ (_procColChoice 5 concatColsSTEP4))
	
	let resultSTEP4 = _impositionRowsCols concatRowsSTEP4 concatColsProcessedSTEP4
	
	-- wages calculation
	let calculate_wages_housesSTEP4 = (_updateAllIntBoardByCheckOptions 0 house_index_list resultSTEP4)
	let int_board_with_wagesSTEP4 = _ereaseFromIntIfOccupied (_updateWageBoardByOptions house_index_list calculate_wages_housesSTEP4 joined_int_boards) resultSTEP4
	
	{-print concatRowsSTEP4
	print concatColsProcessedSTEP4
	print resultSTEP4
	print int_board_with_wagesSTEP4
	
	print " "
	print "END OF STEP 4!"
	print " " -}
	
	-- STEP5 checking if we can find over int board 4 value - if so, we can surely place there warmer, because it's the only place where house could have warmer!
	let resultSTEP5 = _placeIfCellEquals4 int_board_with_wagesSTEP4 resultSTEP4
	
	-- wages calculation
	let calculate_wages_housesSTEP5 = (_updateAllIntBoardByCheckOptions 0 house_index_list resultSTEP5)
	let int_board_with_wagesSTEP5 = _ereaseFromIntIfOccupied (_updateWageBoardByOptions house_index_list calculate_wages_housesSTEP5 joined_int_boards) resultSTEP5
	
	{-print " "
	print "END OF STEP 5!"
	print " " 	-}
	
	-- STEP6 once again STEP2 
	let rowSTEP6_0 = (_checkIfAllWarmersInRow 1 (_columnToRow False 0 resultSTEP5))
	let rowSTEP6_1 = (_checkIfAllWarmersInRow 0 (_columnToRow False 1 resultSTEP5))
	let rowSTEP6_2 = (_checkIfAllWarmersInRow 2 (_columnToRow False 2 resultSTEP5))
	let rowSTEP6_3 = (_checkIfAllWarmersInRow 1 (_columnToRow False 3 resultSTEP5))
	let rowSTEP6_4 = (_checkIfAllWarmersInRow 2 (_columnToRow False 4 resultSTEP5))
	let rowSTEP6_5 = (_checkIfAllWarmersInRow 1 (_columnToRow False 5 resultSTEP5))
	let concatRowsSTEP6 = rowSTEP6_0 ++ rowSTEP6_1 ++ rowSTEP6_2 ++ rowSTEP6_3 ++ rowSTEP6_4 ++ rowSTEP6_5
	
	let colSTEP6_0 = (_checkIfAllWarmersInRow 1 (_columnToRow True 0 resultSTEP5))
	let colSTEP6_1 = (_checkIfAllWarmersInRow 1 (_columnToRow True 1 resultSTEP5))
	let colSTEP6_2 = (_checkIfAllWarmersInRow 2 (_columnToRow True 2 resultSTEP5))
	let colSTEP6_3 = (_checkIfAllWarmersInRow 1 (_columnToRow True 3 resultSTEP5))
	let colSTEP6_4 = (_checkIfAllWarmersInRow 1 (_columnToRow True 4 resultSTEP5))
	let colSTEP6_5 = (_checkIfAllWarmersInRow 1 (_columnToRow True 5 resultSTEP5))
	let concatColsSTEP6 = colSTEP6_0 ++ colSTEP6_1 ++ colSTEP6_2 ++ colSTEP6_3 ++ colSTEP6_4 ++ colSTEP6_5
	let concatColsProcessedSTEP6 = ((_procColChoice 0 concatColsSTEP6) ++ (_procColChoice 1 concatColsSTEP6) ++ (_procColChoice 2 concatColsSTEP6) ++ (_procColChoice 3 concatColsSTEP6) ++ (_procColChoice 4 concatColsSTEP6) ++ (_procColChoice 5 concatColsSTEP6))
	
	let resultSTEP6 = (_impositionRowsCols concatRowsSTEP6 concatColsProcessedSTEP6)
	
	-- wages calculation
	let calculate_wages_housesSTEP6 = (_updateAllIntBoardByCheckOptions 0 house_index_list resultSTEP6)
	let int_board_with_wagesSTEP6 = _ereaseFromIntIfOccupied (_updateWageBoardByOptions house_index_list calculate_wages_housesSTEP6 joined_int_boards) resultSTEP6
	
	{-print " "
	print "END OF STEP 6!"
	print " " -}
	
	-- STEP7 once again STEP1
	-- ereasing rows and columns with 0 number & ereasing cells which are on the edges for hauses
	let rowSTEP7_0 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow False 0 resultSTEP6)) (_ereaseIfNotFitting (_columnToRow False 0 joined_int_boards) (_columnToRow False 0 resultSTEP6)))
	let rowSTEP7_1 = (_checkEqualityRow (0 - _countWarmersInRow (_columnToRow False 1 resultSTEP6)) (_ereaseIfNotFitting (_columnToRow False 1 joined_int_boards) (_columnToRow False 1 resultSTEP6)))
	let rowSTEP7_2 = (_checkEqualityRow (2 - _countWarmersInRow (_columnToRow False 2 resultSTEP6)) (_ereaseIfNotFitting (_columnToRow False 2 joined_int_boards) (_columnToRow False 2 resultSTEP6)))
	let rowSTEP7_3 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow False 3 resultSTEP6)) (_ereaseIfNotFitting (_columnToRow False 3 joined_int_boards) (_columnToRow False 3 resultSTEP6)))
	let rowSTEP7_4 = (_checkEqualityRow (2 - _countWarmersInRow (_columnToRow False 4 resultSTEP6)) (_ereaseIfNotFitting (_columnToRow False 4 joined_int_boards) (_columnToRow False 4 resultSTEP6)))
	let rowSTEP7_5 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow False 5 resultSTEP6)) (_ereaseIfNotFitting (_columnToRow False 5 joined_int_boards) (_columnToRow False 5 resultSTEP6)))
	let concatRowsSTEP7 = rowSTEP7_0 ++ rowSTEP7_1 ++ rowSTEP7_2 ++ rowSTEP7_3 ++ rowSTEP7_4 ++ rowSTEP7_5
	
	-- ereasing rows and columns with 0 number & ereasing cells which are on the edges for hauses
	let colSTEP7_0 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow True 0 resultSTEP6)) (_ereaseIfNotFitting (_columnToRow True 0 joined_int_boards) (_columnToRow True 0 resultSTEP6)))
	let colSTEP7_1 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow True 1 resultSTEP6)) (_ereaseIfNotFitting (_columnToRow True 1 joined_int_boards) (_columnToRow True 1 resultSTEP6)))
	let colSTEP7_2 = (_checkEqualityRow (2 - _countWarmersInRow (_columnToRow True 2 resultSTEP6)) (_ereaseIfNotFitting (_columnToRow True 2 joined_int_boards) (_columnToRow True 2 resultSTEP6)))
	let colSTEP7_3 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow True 3 resultSTEP6)) (_ereaseIfNotFitting (_columnToRow True 3 joined_int_boards) (_columnToRow True 3 resultSTEP6)))
	let colSTEP7_4 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow True 4 resultSTEP6)) (_ereaseIfNotFitting (_columnToRow True 4 joined_int_boards) (_columnToRow True 4 resultSTEP6)))
	let colSTEP7_5 = (_checkEqualityRow (1 - _countWarmersInRow (_columnToRow True 5 resultSTEP6)) (_ereaseIfNotFitting (_columnToRow True 5 joined_int_boards) (_columnToRow True 5 resultSTEP6)))
	let concatColsSTEP7 = colSTEP7_0 ++ colSTEP7_1 ++ colSTEP7_2 ++ colSTEP7_3 ++ colSTEP7_4 ++ colSTEP7_5
	let concatColsProcessedSTEP7 = ((_procColChoice 0 concatColsSTEP7) ++ (_procColChoice 1 concatColsSTEP7) ++ (_procColChoice 2 concatColsSTEP7) ++ (_procColChoice 3 concatColsSTEP7) ++ (_procColChoice 4 concatColsSTEP7) ++ (_procColChoice 5 concatColsSTEP7))
	
	let resultSTEP7 = _impositionRowsCols concatRowsSTEP7 concatColsProcessedSTEP7
	
	-- wages calculation
	let calculate_wages_housesSTEP7 = (_updateAllIntBoardByCheckOptions 0 house_index_list resultSTEP7)
	let int_board_with_wagesSTEP7 = _ereaseFromIntIfOccupied (_updateWageBoardByOptions house_index_list calculate_wages_housesSTEP7 joined_int_boards) resultSTEP7
	
	-- print int_board_with_wagesSTEP7
	
	putStrLn (_columnToRow False 0 resultSTEP7)
	putStrLn (_columnToRow False 1 resultSTEP7)
	putStrLn (_columnToRow False 2 resultSTEP7)
	putStrLn (_columnToRow False 3 resultSTEP7)
	putStrLn (_columnToRow False 4 resultSTEP7)
	putStrLn (_columnToRow False 5 resultSTEP7)
	
	{-print " "
	print "END OF STEP 6!"
	print " " 
	-}


	
	
	
	
	
	
	