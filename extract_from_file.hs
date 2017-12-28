import System.IO  
import System.Directory  
import Data.List  
import Data.Char
  
main = do        
    handle <- openFile "input_file.txt" ReadMode  
    contents <- hGetContents handle  
    let houseParams = lines contents     
        numberedParams = zipWith (\n line -> line) [0..] houseParams  

    putStrLn "These are your house params:"  
    putStr $ (_takeElementAt 2 numberedParams)
	
    hClose handle  
	
--0 3 3 4 4 5 5
--1 2 4 0 4 2 5

_takeElementAt :: Int -> [a] -> a
_takeElementAt _ [] = error "No elements on generated list!"
_takeElementAt n (x:xs) 
                       | n == 0 = x 
					   | otherwise  = _takeElementAt (n-1) xs
