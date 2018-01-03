import Data.List
import Data.Char
import System.IO  
import System.Directory    
import Control.Monad

-- file data extraction from file
f :: [String] -> [Int]
f = map read

byTwos :: [a] -> [(a,a)]
byTwos [] = []
byTwos xs = zip firsts seconds 
    where enumerated = zip xs [1..]
          firsts     = [fst x | x <- enumerated, odd $ snd x]
          seconds    = [fst x | x <- enumerated, even $ snd x]
-- end of data extraction from file
		  
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

_processNo :: Int -> (Int, Int) -> Int
_processNo dim (n,t) = dim * n + t

-- board creation 
_board :: Int -> Char -> [Char]
_board 0 _ = []
_board n x = x : _board (n-1) x

-- if at the beginning is 0 than all row will be erased
_ereaseIf0AtRow :: Int -> [Char] -> [Char]
_ereaseIf0AtRow _ [] = []
_ereaseIf0AtRow n (x:xs) 
						| n == 0 && x == '0' = 'X' : _ereaseIf0AtRow n xs
						| otherwise = x : _ereaseIf0AtRow n xs
				  
-- creation of number board 				
_numberBoard :: Int -> [Int]
_numberBoard n =  sub 0
			where sub i | i >= n =  []
						| otherwise = 0 : sub (i + 1)
						
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

_increaseListByFittingCells :: Int -> [Int] -> [Int]
_increaseListByFittingCells _ [] = []   		-- [x - 6, x - 1, x + 1, x + 6]
_increaseListByFittingCells dim (x:xs) = [x - dim, x - (dim - 5), x, x + (dim - 5), x + dim] ++ _increaseListByFittingCells dim xs

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
_columnToRow :: Int -> Bool -> Int -> [a] -> [a]								
_columnToRow _ _ _ [] = []
_columnToRow dim rowOrCol number xs = _iteratingThroughTab dim rowOrCol number 0 xs [0, dim..(dim-1)*dim] [0, 1..(dim-1)]

_iteratingThroughTab :: Int -> Bool -> Int ->  Int -> [a] -> [Int] -> [Int] -> [a]
_iteratingThroughTab _ _ _ _ [] _ _ = []						
_iteratingThroughTab dim rowOrCol number iterator (x:xs) ys zs
								| (_checkIfExist (_genIndexInColList dim rowOrCol number ys zs) iterator) = x : _iteratingThroughTab dim rowOrCol number (iterator + 1) xs ys zs
								| otherwise = _iteratingThroughTab dim rowOrCol number (iterator + 1) xs ys zs

_genIndexInColList :: Int -> Bool -> Int -> [Int] -> [Int] -> [Int]
_genIndexInColList dim rowOrCol number xs ys
								| rowOrCol == True = [ x + number | x <- xs ] -- x <- [ 0, 6, 12, 18, 24, 30 ] ]
								| otherwise = [ x + number * dim | x <- ys ] -- x <- [ 0, 1, 2, 3, 4, 5 ] ]
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

_procColChoice :: Int -> Int -> [Char] -> [Char]
_procColChoice _ _ [] = []
_procColChoice dim column xs 
								| column == 0 = (_procCol dim xs)
								| otherwise = (_procCol dim (drop column xs))

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
_getIndexAndCheckOptions :: Int -> Int -> [Char] -> Int -> Int
_getIndexAndCheckOptions _ _ [] _ = 0
_getIndexAndCheckOptions dim index xs value = _getOptions index len [-dim, 5 - dim, dim - 5, dim] xs
								where len = (length xs) - 1									

_getOptions :: Int -> Int -> [Int] -> [Char] -> Int
_getOptions _ _ [] _ = 0
_getOptions index len (x:xs) ys
								| ((index + x) >= 0) && ((index + x) <= len) && (!!) ys (index + x) == '0' = 1 + _getOptions index len xs ys
								| otherwise = 0 + _getOptions index len xs ys
									
-- 									first is index, second is houses index list, third is char board
_updateAllIntBoardByCheckOptions :: Int -> Int -> [Int] -> [Char] -> [Int]
_updateAllIntBoardByCheckOptions _ _ [] _ = []
_updateAllIntBoardByCheckOptions _ _ _ [] = []
_updateAllIntBoardByCheckOptions _ index _ zs
								| index > (length zs) = []
_updateAllIntBoardByCheckOptions dim index (y:ys) zs 
								| (_checkIfExist (y:ys) index) == True = (_getIndexAndCheckOptions dim index zs 0) : _updateAllIntBoardByCheckOptions dim (index + 1) (y:ys) zs 
								| otherwise = _updateAllIntBoardByCheckOptions dim (index + 1) (y:ys) zs

-- now we will add to int board wages created in previous steps. Arguments: list of houses indexes, list of wages for each house, int board								
_updateWageBoardByOptions :: Int -> [Int] -> [Int] -> [Int] -> [Int]								
_updateWageBoardByOptions _ [] _ zs = zs
_updateWageBoardByOptions _ _ [] zs = zs
_updateWageBoardByOptions dim (x:xs) (y:ys) zs = _updateWageBoardByOptions dim xs ys (_iterateThroughBoard dim 0 x y zs)						
									
_iterateThroughBoard :: Int -> Int -> Int -> Int -> [Int] -> [Int]		
_iterateThroughBoard _ _ _ _ [] = [] 
_iterateThroughBoard dim iterator house_index wage (z:zs) 
								| iterator == house_index = (z + wage) : (_iterateThroughBoard dim (iterator + 1) house_index wage zs)
								| iterator == house_index - dim = (z + wage) : (_iterateThroughBoard dim (iterator + 1) house_index wage zs)
								| iterator == house_index - dim + 5 = (z + wage) : (_iterateThroughBoard dim (iterator + 1) house_index wage zs)
								| iterator == house_index + dim - 5 = (z + wage) : (_iterateThroughBoard dim (iterator + 1) house_index wage zs)
								| iterator == house_index + dim = (z + wage) : (_iterateThroughBoard dim (iterator + 1) house_index wage zs)
								| otherwise = z : (_iterateThroughBoard dim (iterator + 1) house_index wage zs)

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

{--- here will be placed generic transformation part - naive version will be changed into generic one ---}
{--- STEP0 ---}
_genericStep0RowsOrCols :: Int -> Bool -> [Int] -> [Int] -> [Char] -> [Char]	
_genericStep0RowsOrCols _ _ [] [] _ = []							
_genericStep0RowsOrCols dim choice (x:xs) (y:ys) zs = (_ereaseIf0AtRow x (_columnToRow dim choice y zs)) ++ (_genericStep0RowsOrCols dim choice xs ys zs)
-- choice - if choice false = ROWS, if choice true = COLUMNS

_concatColsProcessed :: Int -> [Int] -> [Char] -> [Char]
_concatColsProcessed _ [] _ = []
_concatColsProcessed dim (x:xs) ys = (_procColChoice dim x ys) ++ (_concatColsProcessed dim xs ys)				
			
{--- STEP1 ---}	
-- _checkEqualityRow 1 (_ereaseIfNotFitting (_columnToRow False 0 int_board_with_wagesSTEP0) (_columnToRow False 0 resultSTEP0))
_genericEreasingHousesAndPlacingWarmers :: Int -> Bool -> [Int] -> [Int] -> [Int] -> [Char] -> [Char]	
_genericEreasingHousesAndPlacingWarmers _ _ [] [] _ _ = []							
_genericEreasingHousesAndPlacingWarmers dim choice (x:xs) (y:ys) zs ts = (_checkEqualityRow x (_ereaseIfNotFitting (_columnToRow dim choice y zs) (_columnToRow dim choice y ts))) ++ (_genericEreasingHousesAndPlacingWarmers dim choice xs ys zs ts)	

{--- STEP2 | STEP6 ---}
-- _checkIfAllWarmersInRow 1 (_columnToRow False 0 resultSTEP1)
_genericCheckIfAllWarmers :: Int -> Bool -> [Int] -> [Int] -> [Char] -> [Char]	
_genericCheckIfAllWarmers _ _ [] [] _ = []
_genericCheckIfAllWarmers dim choice (x:xs) (y:ys) zs = (_checkIfAllWarmersInRow x (_columnToRow dim choice y zs)) ++ (_genericCheckIfAllWarmers dim choice xs ys zs)
			
{--- STEP4 ---}			
-- _checkEqualityRow (1 - _countWarmersInRow (_columnToRow False 0 resultSTEP3)) (_ereaseIfNotFitting (_columnToRow False 0 joined_int_boards) (_columnToRow False 0 resultSTEP3))
_genericEreasingHouAndWarmAdditional :: Int -> Bool -> [Int] -> [Int] -> [Int] -> [Char] -> [Char]	
_genericEreasingHouAndWarmAdditional _ _ [] [] _ _ = [] 
_genericEreasingHouAndWarmAdditional dim choice (x:xs) (y:ys) zs ts = (_checkEqualityRow (x - _countWarmersInRow (_columnToRow dim choice y ts)) (_ereaseIfNotFitting (_columnToRow dim choice y zs) (_columnToRow dim choice y ts))) ++ (_genericEreasingHouAndWarmAdditional dim choice xs ys zs ts)

{--- Additional ---}
-- putStrLn (_columnToRow False 0 resultSTEP7)
		
			
main = do
	--putStrLn "- 1 - 1 - 2 - 1 - 1 - 1"
	--putStrLn ( unlines ["1","0","2","1","2","1"])
	
	handle <- openFile "input_file.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = lines contents
            --numberedParams = zipWith (\n line -> line) [0..] singlewords
        let list = f (words (singlewords!!0))
        let listb = f (words (singlewords!!1))
        let tuple_from_file2 = byTwos ( f (words (singlewords!!2)))
		
        --print tuple_from_file2
        --print listb
        --print tuple_from_file
		
        hClose handle 
	
	let tuple_from_file = [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]
	let left_numbers_from_file = [1,0,2,1,2,1]
	let up_numbers_from_file = [1,1,2,1,1,1] 
	
	--let tuple_from_file = [(0,4),(1,1),(1,3),(1,5),(2,2),(3,0),(4,2),(5,5)]
	--let left_numbers_from_file = [2,0,3,0,2,1]
	--let up_numbers_from_file = [1,1,1,2,0,3] 
	
	--let tuple_from_file = [(0,4),(3,1),(4,3),(5,0),(5,2),(5,4),(5,5),(5,6),(6,4)]
	--let left_numbers_from_file = [1,0,1,1,3,1,2]
	--let up_numbers_from_file = [1,2,0,2,1,2,1] 
	
	let board_dim_1 = length left_numbers_from_file
	let board_dim_2 = length up_numbers_from_file
	let board_size = board_dim_1 * board_dim_2
	let rows_list = [0..board_dim_1-1]
	let cols_list = [0..board_dim_2-1]
	
	let houses_basic_wage = 7
	let houses_neighbours_basic_wage = 3
	
	let startup_char_board = reverse (_changeAtBasic (map (_processNo board_dim_1) tuple_from_file) (board_size - 1) (_board board_size '0'))
	let startup_int_board = _placeHousesOnBoard houses_basic_wage (_numberBoard board_size) (_makeHousesIndexList (reverse (_changeAtBasic (map (_processNo board_dim_1) tuple_from_file) (board_size - 1) (_board board_size '0'))) 0) 0
	let startup_int_board2 = _placeHousesOnBoard houses_neighbours_basic_wage (_numberBoard board_size) (_increaseListByFittingCells board_dim_1 (_makeHousesIndexList (reverse (_changeAtBasic (map (_processNo board_dim_1) tuple_from_file) (board_size - 1) (_board board_size '0'))) 0)) 0
	let joined_int_boards = _joinLists startup_int_board startup_int_board2
	let house_index_list = _makeHousesIndexList (reverse (_changeAtBasic (map (_processNo board_dim_1) tuple_from_file) (board_size - 1) (_board board_size '0'))) 0
	
	
	{--- STEP0 ereasing rows and columns with 0 number ---}
	let concatRowsSTEP0 = _genericStep0RowsOrCols board_dim_1 False left_numbers_from_file rows_list startup_char_board
	let concatColsSTEP0 = _genericStep0RowsOrCols board_dim_1 True up_numbers_from_file cols_list startup_char_board
	let concatColsProcessedSTEP0 = _concatColsProcessed board_dim_1 cols_list concatColsSTEP0
	
	let resultSTEP0 = (_impositionRowsCols concatRowsSTEP0 concatColsProcessedSTEP0)
	
	-- wages calculation
	let calculate_wages_housesSTEP0 = (_updateAllIntBoardByCheckOptions board_dim_1 0 house_index_list resultSTEP0)
	let int_board_with_wagesSTEP0 = (_updateWageBoardByOptions board_dim_1 house_index_list calculate_wages_housesSTEP0 joined_int_boards)
	
	{-
	print " "
	print "END OF STEP 0!"
	print " " -}
	
	
	{--- STEP1 ereasing cells which are on the edges for hauses & placing warmers if numbers at row/col beginning are the same ---}
	-- First step of project in rows
	let concatRowsSTEP1 = _genericEreasingHousesAndPlacingWarmers board_dim_1 False left_numbers_from_file rows_list int_board_with_wagesSTEP0 resultSTEP0
	-- First step of project in columns
	let concatColsSTEP1 = _genericEreasingHousesAndPlacingWarmers board_dim_1 True up_numbers_from_file cols_list int_board_with_wagesSTEP0 resultSTEP0
	let concatColsProcessedSTEP1 = _concatColsProcessed board_dim_1 cols_list concatColsSTEP1
	
	let resultSTEP1 = _impositionRowsCols concatRowsSTEP1 concatColsProcessedSTEP1
	
	-- wages calculation
	let calculate_wages_housesSTEP1 = _updateAllIntBoardByCheckOptions board_dim_1 0 house_index_list resultSTEP1
	let int_board_with_wagesSTEP1 = _ereaseFromIntIfOccupied (_updateWageBoardByOptions board_dim_1 house_index_list calculate_wages_housesSTEP1 joined_int_boards) resultSTEP1
	
	{-print " "
	print "END OF STEP 1!"
	print " " -}
	
	
	{--- STEP2 (we' ve got to find out if count of settled warmers is equal to number at the beginning - if yes, other '0' fields will be 'X'): ---}
	let concatRowsSTEP2 = _genericCheckIfAllWarmers board_dim_1 False left_numbers_from_file rows_list resultSTEP1
	let concatColsSTEP2 = _genericCheckIfAllWarmers board_dim_1 True up_numbers_from_file cols_list resultSTEP1
	let concatColsProcessedSTEP2 = _concatColsProcessed board_dim_1 cols_list concatColsSTEP2
	
	let resultSTEP2 = _impositionRowsCols concatRowsSTEP2 concatColsProcessedSTEP2
	
	-- wages calculation
	let calculate_wages_housesSTEP2 = _updateAllIntBoardByCheckOptions board_dim_1 0 house_index_list resultSTEP2
	let int_board_with_wagesSTEP2 = _updateWageBoardByOptions board_dim_1 house_index_list calculate_wages_housesSTEP2 joined_int_boards
	
	{-print " "
	print "END OF STEP 2!"
	print " " -}
	
	
	{--- STEP3 erease cells at the edges of warmers ---}
	let resultSTEP3 = _ereaseByWarmers 0 (_ereaseByWarmersList board_dim_1 (_makeWarmersIndexList 0 resultSTEP2)) resultSTEP2	
	
	-- wages calculation
	let calculate_wages_housesSTEP3 = _updateAllIntBoardByCheckOptions board_dim_1 0 house_index_list resultSTEP3
	let int_board_with_wagesSTEP3 = _ereaseFromIntIfOccupied (_updateWageBoardByOptions board_dim_1 house_index_list calculate_wages_housesSTEP3 joined_int_boards) resultSTEP3
	
	{-print " "
	print "END OF STEP 3!"
	print " " -}
	
	
	{--- STEP4 once again STEP1 ---}
	-- ereasing rows and columns with 0 number & ereasing cells which are on the edges for hauses
	let concatRowsSTEP4 = _genericEreasingHouAndWarmAdditional board_dim_1 False left_numbers_from_file rows_list joined_int_boards resultSTEP3
	-- ereasing rows and columns with 0 number & ereasing cells which are on the edges for hauses
	let concatColsSTEP4 = _genericEreasingHouAndWarmAdditional board_dim_1 True up_numbers_from_file cols_list joined_int_boards resultSTEP3
	let concatColsProcessedSTEP4 = _concatColsProcessed board_dim_1 cols_list concatColsSTEP4
	
	let resultSTEP4 = _impositionRowsCols concatRowsSTEP4 concatColsProcessedSTEP4
	
	-- wages calculation
	let calculate_wages_housesSTEP4 = _updateAllIntBoardByCheckOptions board_dim_1 0 house_index_list resultSTEP4
	let int_board_with_wagesSTEP4 = _ereaseFromIntIfOccupied (_updateWageBoardByOptions board_dim_1 house_index_list calculate_wages_housesSTEP4 joined_int_boards) resultSTEP4
	
	{-print " "
	print "END OF STEP 4!"
	print " " -}
	
	
	{--- STEP5 checking if we can find over int board 4 value - if so, we can surely place there warmer, because it's the only place where house could have warmer! ---}
	let resultSTEP5 = _placeIfCellEquals4 int_board_with_wagesSTEP4 resultSTEP4
	
	-- wages calculation
	let calculate_wages_housesSTEP5 = _updateAllIntBoardByCheckOptions board_dim_1 0 house_index_list resultSTEP5
	let int_board_with_wagesSTEP5 = _ereaseFromIntIfOccupied (_updateWageBoardByOptions board_dim_1 house_index_list calculate_wages_housesSTEP5 joined_int_boards) resultSTEP5
	
	{-print " "
	print "END OF STEP 5!"
	print " " 	-}
	
	
	{--- STEP6 once again STEP2 ---}
	let concatRowsSTEP6 = _genericCheckIfAllWarmers board_dim_1 False left_numbers_from_file rows_list resultSTEP5
	let concatColsSTEP6 = _genericCheckIfAllWarmers board_dim_1 True up_numbers_from_file cols_list resultSTEP5
	let concatColsProcessedSTEP6 = _concatColsProcessed board_dim_1 cols_list concatColsSTEP6
	
	let resultSTEP6 = _impositionRowsCols concatRowsSTEP6 concatColsProcessedSTEP6
	
	-- wages calculation
	let calculate_wages_housesSTEP6 = _updateAllIntBoardByCheckOptions board_dim_1 0 house_index_list resultSTEP6
	let int_board_with_wagesSTEP6 = _ereaseFromIntIfOccupied (_updateWageBoardByOptions board_dim_1 house_index_list calculate_wages_housesSTEP6 joined_int_boards) resultSTEP6
	
	{-print " "
	print "END OF STEP 6!"
	print " " -}
	
	
	{--- STEP7 once again STEP1 ---}
	-- ereasing rows and columns with 0 number & ereasing cells which are on the edges for hauses
	let concatRowsSTEP7 = _genericEreasingHouAndWarmAdditional board_dim_1 False left_numbers_from_file rows_list joined_int_boards resultSTEP6
	-- ereasing rows and columns with 0 number & ereasing cells which are on the edges for hauses
	let concatColsSTEP7 = _genericEreasingHouAndWarmAdditional board_dim_1 True up_numbers_from_file cols_list joined_int_boards resultSTEP6
	let concatColsProcessedSTEP7 = _concatColsProcessed board_dim_1 cols_list concatColsSTEP7
	
	let resultSTEP7 = _impositionRowsCols concatRowsSTEP7 concatColsProcessedSTEP7
	
	-- wages calculation
	let calculate_wages_housesSTEP7 = _updateAllIntBoardByCheckOptions board_dim_1 0 house_index_list resultSTEP7
	let int_board_with_wagesSTEP7 = _ereaseFromIntIfOccupied (_updateWageBoardByOptions board_dim_1 house_index_list calculate_wages_housesSTEP7 joined_int_boards) resultSTEP7
	
	-- print int_board_with_wagesSTEP7
	
	putStrLn (_columnToRow board_dim_1 False 0 resultSTEP7)
	putStrLn (_columnToRow board_dim_1 False 1 resultSTEP7)
	putStrLn (_columnToRow board_dim_1 False 2 resultSTEP7)
	putStrLn (_columnToRow board_dim_1 False 3 resultSTEP7)
	putStrLn (_columnToRow board_dim_1 False 4 resultSTEP7)
	putStrLn (_columnToRow board_dim_1 False 5 resultSTEP7)
	--putStrLn (_columnToRow board_dim_1 False 6 resultSTEP7)
	
	{-print " "
	print "END OF STEP 7!"
	print " " -}
	
	