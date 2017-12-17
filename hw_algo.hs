import System.IO  
import System.Directory  
import Data.List  
  
main = do        
    handle <- openFile "input_file.txt" ReadMode  
    contents <- hGetContents handle  
    let houseParams = lines contents     
        numberedParams = zipWith (\n line -> show n ++ " - " ++ line) [0..] houseParams     
    putStrLn "These are your house params:"  
    putStr $ unlines numberedParams --(_takeElementAt 2 numberedParams)    
 
    hClose handle  
	
{-main = do  
    let list1 = []
    handle <- openFile "input_file.txt" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        list1 = _listToInt singlewords
		--let lLeft
		

    --putStrLn $ show listLeft 
    hClose handle-}
	
	
--0 3 3 4 4 5 5
--1 2 4 0 4 2 5

_takeElementAt :: Int -> [a] -> a
_takeElementAt _ [] = error "No elements on generated list!"
_takeElementAt n (x:xs) 
                       | n == 0 = x 
					   | otherwise  = _takeElementAt (n-1) xs
	
_listToInt :: [String] -> [Int]
_listToInt = map read

_listLeft :: [Int] -> [Int]
_listLeft [] = []
_listLeft xs = _take 6 xs

_listTop :: [Int] -> [Int]
_listTop [] = []
_listTop xs = drop 6 xs

_take :: (Num i, Ord i) => i -> [a] -> [a]
_take n _ 
  | n <= 0 = []
_take _ [] = []
_take n (x:xs) = x : _take (n-1) xs

_drop :: (Num i, Ord i) => i -> [a] -> [a]
_drop _ [] = []
_drop n (x:xs) 
  | n > 0 = _drop (n-1) xs
  | otherwise = x : _drop n xs