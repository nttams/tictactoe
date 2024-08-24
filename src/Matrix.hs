{-# LANGUAGE DeriveGeneric #-}

module Matrix where

import Text.Read (readMaybe)
import qualified Data.Map
import qualified Data.Aeson
import qualified GHC.Generics

data Mark = X | O deriving (Eq, Show, GHC.Generics.Generic)

instance Data.Aeson.ToJSON Mark where
instance Data.Aeson.FromJSON Mark where

type Position = (Int, Int)
type Matrix = Data.Map.Map Position Mark

getMatrixSize :: Matrix -> Int
getMatrixSize = findMaxIndex . Data.Map.keys

findMaxIndex :: [Position] -> Int
findMaxIndex [] = 0
findMaxIndex [(row, col)] = max row col
findMaxIndex ((row, col):xs) = max (max row col) (findMaxIndex xs)

printLine :: Matrix -> Int -> Int -> Int -> String
printLine _ size currentIndex _ | currentIndex > size = ""
printLine m size currentColIndex rowIndex = 
    let lookupResult = Data.Map.lookup (rowIndex, currentColIndex) m 
    in case lookupResult of
        Nothing -> ". " ++ printLine m size (currentColIndex + 1) rowIndex
        Just v -> show v ++ " " ++ printLine m size (currentColIndex + 1) rowIndex

printMatrixBackend :: Matrix -> Int -> Int -> IO ()
printMatrixBackend m size currentRowIndex =
    if currentRowIndex > size then putStrLn ""
    else do
        putStrLn $ show (currentRowIndex `mod` 10) ++ ": " ++ printLine m size 0 currentRowIndex
        printMatrixBackend m size (currentRowIndex + 1)

printMatrix :: Matrix -> IO()
printMatrix m = do 
    let size = getMatrixSize m
    putStrLn $ "   " ++ unwords (map (\x -> show (x `mod` 10)) [0..size])
    printMatrixBackend m size 0

data Direction = RowNext | ColNext | Cross1Next | Cross2Next |
                 RowBack | ColBack | Cross1Back | Cross2Back deriving Eq

shift :: Position -> Direction -> Position
shift (row, col) way =
    case way of 
        RowNext -> (row, col + 1) -- left -> right
        ColNext -> (row - 1, col) -- top -> bottom
        Cross1Next -> (row - 1, col + 1) -- upper left --> bottom right
        Cross2Next -> (row - 1, col - 1) -- upper right --> bottom left
        RowBack -> (row, col - 1)
        ColBack -> (row + 1, col)
        Cross1Back -> (row + 1, col - 1)
        Cross2Back -> (row + 1, col + 1)

countMark :: Matrix -> Position -> Direction -> Mark -> Int -> Int
countMark m pos direction mark count = case Data.Map.lookup pos m of 
    Nothing -> count
    Just v -> if v == mark then 1 + countMark m (shift pos direction) direction mark count else count

-- matrix check winner
checkWin :: Matrix -> Position -> Mark -> Int -> Bool
checkWin m pos mark winSize = countMark m pos RowBack  mark (-1) + countMark m pos RowNext  mark (-1) + 1 >= winSize ||
                              countMark m pos ColBack mark (-1) + countMark m pos ColNext mark (-1) + 1 >= winSize ||
                              countMark m pos Cross1Back mark (-1) + countMark m pos Cross1Next mark (-1) + 1 >= winSize ||
                              countMark m pos Cross2Back mark (-1) + countMark m pos Cross2Next mark (-1) + 1 >= winSize

isValidInput :: Position -> Bool
isValidInput (row, col) | row < 0 || col < 0 = False
isValidInput _ = True

isPositionEmpty :: Matrix -> Position -> Bool
isPositionEmpty m p = case Data.Map.lookup p m of
    Nothing -> True
    _ -> False

parseInput :: String -> Maybe Position
parseInput s = 
    let items = words s
    in if length items < 2 then Nothing
       else let row = readMaybe $ head items
                col = readMaybe $ last items
            in case (row, col) of
                (Nothing, _) -> Nothing
                (_, Nothing) -> Nothing
                (Just r, Just c) -> Just (r, c)

updateMatrix :: Matrix -> Position -> Mark -> Matrix
updateMatrix matrix pos mark = Data.Map.insert pos mark matrix
