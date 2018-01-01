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
				  
-- if at the beginning of column is  0 than column will be erased
{-
_ereaseIf0AtCol :: [Int] -> Int -> [Char] -> [Char]
_ereaseIf0AtCol _ _ [] = [] 
_ereaseIf0AtCol (x:xs) index (y:ys) 
				  | x == 0 = 'X' : _ereaseIf0AtCol xs (index + 1) ys
				  | otherwise = y : _ereaseIf0AtCol xs (index + 1) ys				  
-}

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
								| rowOrCol == True = [ x + number | x <- [0,6,12,18,24,30] ]
								| otherwise = [ x + number * 6 | x <- [0,1,2,3,4,5] ]
-- end of showing board by index

-- the rows and columns imposition
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
								

				 
main = do
	putStrLn "- 1 - 1 - 2 - 1 - 1 - 1"
	putStrLn ( unlines ["1","0","2","1","2","1"])
	
	-- First step of project in rows
	let row_0 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow False 0 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_columnToRow False 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	let row_1 = (_checkEqualityRow 0 (_ereaseIf0AtRow 0 (_ereaseIfNotFitting (_columnToRow False 1 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_columnToRow False 1 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	let row_2 = (_checkEqualityRow 2 (_ereaseIf0AtRow 2 (_ereaseIfNotFitting (_columnToRow False 2 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_columnToRow False 2 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	let row_3 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow False 3 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_columnToRow False 3 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	let row_4 = (_checkEqualityRow 2 (_ereaseIf0AtRow 2 (_ereaseIfNotFitting (_columnToRow False 4 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_columnToRow False 4 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	let row_5 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow False 5 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_columnToRow False 5 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	let concatRows = row_0 ++ row_1 ++ row_2 ++ row_3 ++ row_4 ++ row_5
	
	-- First step of project in columns
	let col_0 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 0 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_columnToRow True 0 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	let col_1 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 1 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_columnToRow True 1 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	let col_2 = (_checkEqualityRow 2 (_ereaseIf0AtRow 2 (_ereaseIfNotFitting (_columnToRow True 2 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_columnToRow True 2 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	let col_3 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 3 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_columnToRow True 3 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	let col_4 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 4 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_columnToRow True 4 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	let col_5 = (_checkEqualityRow 1 (_ereaseIf0AtRow 1 (_ereaseIfNotFitting (_columnToRow True 5 (_placeHousesOnBoard (_numberBoard 36) (_increaseListByFittingCells (_makeHousesIndexList (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0'))) 0)) 0)) (_columnToRow True 5 (reverse (_changeAtBasic (map _processNo [(0,1),(3,2),(3,4),(4,0),(4,4),(5,2),(5,5)]) 35 (_board 36 '0')))))))
	let concatCols = col_0 ++ col_1 ++ col_2 ++ col_3 ++ col_4 ++ col_5
	let concatColsProcessed = ((_procColChoice 0 concatCols) ++ (_procColChoice 1 concatCols) ++ (_procColChoice 2 concatCols) ++ (_procColChoice 3 concatCols) ++ (_procColChoice 4 concatCols) ++ (_procColChoice 5 concatCols))
	
	
	print concatRows
	print concatColsProcessed 
	
	print (_impositionRowsCols concatRows concatColsProcessed)
	{-putStrLn "Normal board view:"
	putStrLn ""
	print row_0 
	print row_1
	print row_2
	print row_3
	print row_4 
	print row_5	

	putStrLn "" 
	putStrLn "Pivoted board view:"
	putStrLn ""
	print col_0
	print col_1
	print col_2
	print col_3
	print col_4
	print col_5-}
	
	
	
	
	
	
	