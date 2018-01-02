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
									
									
main = do
	putStrLn "- 1 - 1 - 2 - 1 - 1 - 1"
	putStrLn ( unlines ["1","0","2","1","2","1"])
	
	let startup_char_board = (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))
	let startup_int_board = (_placeHousesOnBoard 7 (_numberBoard 36) (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0) 0)
	let startup_int_board2 = (_placeHousesOnBoard 3 (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)
	let joined_int_boards = _joinLists startup_int_board startup_int_board2
	let house_index_list = (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)
	--print startup_int_board
	--print startup_int_board2
	--print joined_int_boards
	
	-- STEP1 ereasing rows and columns with 0 number & ereasing cells which are on the edges for hauses
	-- First step of project in rows
	let row_0 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow False 0 joined_int_boards) (_columnToRow False 0 startup_char_board))))
	let row_1 = (_checkEqualityRow 0 (_ereaseIf0AtRow 0 (_ereaseIfNotFitting (_columnToRow False 1 joined_int_boards) (_columnToRow False 1 startup_char_board))))
	let row_2 = (_checkEqualityRow 2 (_ereaseIf0AtRow 2 (_ereaseIfNotFitting (_columnToRow False 2 joined_int_boards) (_columnToRow False 2 startup_char_board))))
	let row_3 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow False 3 joined_int_boards) (_columnToRow False 3 startup_char_board))))
	let row_4 = (_checkEqualityRow 2 (_ereaseIf0AtRow 2 (_ereaseIfNotFitting (_columnToRow False 4 joined_int_boards) (_columnToRow False 4 startup_char_board))))
	let row_5 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow False 5 joined_int_boards) (_columnToRow False 5 startup_char_board))))
	let concatRows = row_0 ++ row_1 ++ row_2 ++ row_3 ++ row_4 ++ row_5
	
	-- First step of project in columns
	let col_0 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 0 joined_int_boards) (_columnToRow True 0 startup_char_board))))
	let col_1 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 1 joined_int_boards) (_columnToRow True 1 startup_char_board))))
	let col_2 = (_checkEqualityRow 2 (_ereaseIf0AtRow 2 (_ereaseIfNotFitting (_columnToRow True 2 joined_int_boards) (_columnToRow True 2 startup_char_board))))
	let col_3 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 3 joined_int_boards) (_columnToRow True 3 startup_char_board))))
	let col_4 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 4 joined_int_boards) (_columnToRow True 4 startup_char_board))))
	let col_5 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 5 joined_int_boards) (_columnToRow True 5 startup_char_board))))
	let concatCols = col_0 ++ col_1 ++ col_2 ++ col_3 ++ col_4 ++ col_5
	let concatColsProcessed = ((_procColChoice 0 concatCols) ++ (_procColChoice 1 concatCols) ++ (_procColChoice 2 concatCols) ++ (_procColChoice 3 concatCols) ++ (_procColChoice 4 concatCols) ++ (_procColChoice 5 concatCols))
	
	-- STEP2 (we' ve got to find out if count of settled warmers is equal to number at the beginning - if yes, other '0' fields will be 'X'): 
	let rowSTEP2_0 = (_checkIfAllWarmersInRow 1 (_columnToRow False 0 (_impositionRowsCols concatRows concatColsProcessed)))
	let rowSTEP2_1 = (_checkIfAllWarmersInRow 0 (_columnToRow False 1 (_impositionRowsCols concatRows concatColsProcessed)))
	let rowSTEP2_2 = (_checkIfAllWarmersInRow 2 (_columnToRow False 2 (_impositionRowsCols concatRows concatColsProcessed)))
	let rowSTEP2_3 = (_checkIfAllWarmersInRow 1 (_columnToRow False 3 (_impositionRowsCols concatRows concatColsProcessed)))
	let rowSTEP2_4 = (_checkIfAllWarmersInRow 2 (_columnToRow False 4 (_impositionRowsCols concatRows concatColsProcessed)))
	let rowSTEP2_5 = (_checkIfAllWarmersInRow 1 (_columnToRow False 5 (_impositionRowsCols concatRows concatColsProcessed)))
	let concatRowsSTEP2 = rowSTEP2_0 ++ rowSTEP2_1 ++ rowSTEP2_2 ++ rowSTEP2_3 ++ rowSTEP2_4 ++ rowSTEP2_5
	
	let colSTEP2_0 = (_checkIfAllWarmersInRow 1 (_columnToRow True 0 (_impositionRowsCols concatRows concatColsProcessed)))
	let colSTEP2_1 = (_checkIfAllWarmersInRow 1 (_columnToRow True 1 (_impositionRowsCols concatRows concatColsProcessed)))
	let colSTEP2_2 = (_checkIfAllWarmersInRow 2 (_columnToRow True 2 (_impositionRowsCols concatRows concatColsProcessed)))
	let colSTEP2_3 = (_checkIfAllWarmersInRow 1 (_columnToRow True 3 (_impositionRowsCols concatRows concatColsProcessed)))
	let colSTEP2_4 = (_checkIfAllWarmersInRow 1 (_columnToRow True 4 (_impositionRowsCols concatRows concatColsProcessed)))
	let colSTEP2_5 = (_checkIfAllWarmersInRow 1 (_columnToRow True 5 (_impositionRowsCols concatRows concatColsProcessed)))
	let concatColsSTEP2 = colSTEP2_0 ++ colSTEP2_1 ++ colSTEP2_2 ++ colSTEP2_3 ++ colSTEP2_4 ++ colSTEP2_5
	let concatColsProcessedSTEP2 = ((_procColChoice 0 concatColsSTEP2) ++ (_procColChoice 1 concatColsSTEP2) ++ (_procColChoice 2 concatColsSTEP2) ++ (_procColChoice 3 concatColsSTEP2) ++ (_procColChoice 4 concatColsSTEP2) ++ (_procColChoice 5 concatColsSTEP2))
	
	
	--print concatRows
	--print concatColsProcessed 
	-- STEP3 erease cells at the edges of warmers
	let warmersList = _ereaseByWarmers 0 (_ereaseByWarmersList 6 (_makeWarmersIndexList 0 (_impositionRowsCols concatRowsSTEP2 concatColsProcessedSTEP2))) (_impositionRowsCols concatRowsSTEP2 concatColsProcessedSTEP2)	

	{-let rowLASTSTEP_0 = (_columnToRow False 0 warmersList)
	let rowLASTSTEP_1 = (_columnToRow False 1 warmersList)
	let rowLASTSTEP_2 = (_columnToRow False 2 warmersList)
	let rowLASTSTEP_3 = (_columnToRow False 3 warmersList)
	let rowLASTSTEP_4 = (_columnToRow False 4 warmersList)
	let rowLASTSTEP_5 = (_columnToRow False 5 warmersList)
	
	let colLASTSTEP_0 = (_columnToRow True 0 warmersList)
	let colLASTSTEP_1 = (_columnToRow True 1 warmersList)
	let colLASTSTEP_2 = (_columnToRow True 2 warmersList)
	let colLASTSTEP_3 = (_columnToRow True 3 warmersList)
	let colLASTSTEP_4 = (_columnToRow True 4 warmersList)
	let colLASTSTEP_5 = (_columnToRow True 5 warmersList)-}
	
	-- STEP4 once again STEP1
	-- First step of project in rows
	let rowLASTSTEP_0 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow False 0 joined_int_boards) (_columnToRow False 0 warmersList))))
	let rowLASTSTEP_1 = (_checkEqualityRow 0 (_ereaseIf0AtRow 0 (_ereaseIfNotFitting (_columnToRow False 1 joined_int_boards) (_columnToRow False 1 warmersList))))
	let rowLASTSTEP_2 = (_checkEqualityRow 2 (_ereaseIf0AtRow 2 (_ereaseIfNotFitting (_columnToRow False 2 joined_int_boards) (_columnToRow False 2 warmersList))))
	let rowLASTSTEP_3 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow False 3 joined_int_boards) (_columnToRow False 3 warmersList))))
	let rowLASTSTEP_4 = (_checkEqualityRow 2 (_ereaseIf0AtRow 2 (_ereaseIfNotFitting (_columnToRow False 4 joined_int_boards) (_columnToRow False 4 warmersList))))
	let rowLASTSTEP_5 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow False 5 joined_int_boards) (_columnToRow False 5 warmersList))))
	let concatRowsSTEP3 = rowLASTSTEP_0 ++ rowLASTSTEP_1 ++ rowLASTSTEP_2 ++ rowLASTSTEP_3 ++ rowLASTSTEP_4 ++ rowLASTSTEP_5
	
	-- First step of project in columns
	let colLASTSTEP_0 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 0 joined_int_boards) (_columnToRow True 0 warmersList))))
	let colLASTSTEP_1 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 1 joined_int_boards) (_columnToRow True 1 warmersList))))
	let colLASTSTEP_2 = (_checkEqualityRow 2 (_ereaseIf0AtRow 2 (_ereaseIfNotFitting (_columnToRow True 2 joined_int_boards) (_columnToRow True 2 warmersList))))
	let colLASTSTEP_3 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 3 joined_int_boards) (_columnToRow True 3 warmersList))))
	let colLASTSTEP_4 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 4 joined_int_boards) (_columnToRow True 4 warmersList))))
	let colLASTSTEP_5 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 5 joined_int_boards) (_columnToRow True 5 warmersList))))
	let concatColsSTEP3 = colLASTSTEP_0 ++ colLASTSTEP_1 ++ colLASTSTEP_2 ++ colLASTSTEP_3 ++ colLASTSTEP_4 ++ colLASTSTEP_5
	let concatColsProcessedSTEP3 = ((_procColChoice 0 concatColsSTEP3) ++ (_procColChoice 1 concatColsSTEP3) ++ (_procColChoice 2 concatColsSTEP3) ++ (_procColChoice 3 concatColsSTEP3) ++ (_procColChoice 4 concatColsSTEP3) ++ (_procColChoice 5 concatColsSTEP3))
	
	let impResult = _impositionRowsCols concatRowsSTEP3 concatColsProcessedSTEP3
	
	let calculate_wages_houses = (_updateAllIntBoardByCheckOptions 0 house_index_list impResult)
	
	print joined_int_boards
	print (_updateWageBoardByOptions house_index_list calculate_wages_houses joined_int_boards)
	
	-- 									first is index, second is houses index list, third is char board
	{-print "house index list:"
	print house_index_list
	print "char board:"
	print (impResult)
	print calculate_wages_houses-}
	
	
	{-putStrLn "Normal board view:"
	putStrLn ""
	putStrLn rowLASTSTEP_0
	putStrLn rowLASTSTEP_1
	putStrLn rowLASTSTEP_2
	putStrLn rowLASTSTEP_3
	putStrLn rowLASTSTEP_4
	putStrLn rowLASTSTEP_5
	
	{-print row_1
	print row_2
	print row_3
	print row_4 
	print row_5	-}

	putStrLn "" 
	putStrLn "Pivoted board view:"
	putStrLn ""
	putStrLn colLASTSTEP_0
	putStrLn colLASTSTEP_1
	putStrLn colLASTSTEP_2
	putStrLn colLASTSTEP_3
	putStrLn colLASTSTEP_4
	putStrLn colLASTSTEP_5-}


	
	
	
	
	
	
	