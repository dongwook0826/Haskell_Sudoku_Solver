-- https://github.com/HansBlackCat/Haskell/blob/master/practice-haskell/src/Sudoku/Sudoku.hs
-- Thanks a lot <3

import Sudoku
import Data.List
import Data.Char

main =
    let solveMain = do
        putStr ">>> "
        rawSdkLine <- getLine
        if length rawSdkLine /= 81 then do
            putStrLn "Invalid length : try again!"
            solveMain
        else
            let charToInt = (\c -> let c0 = ord c - ord '0' in
                                   if c0 >= 0 && c0 <= 9 then c0
                                   else 0)
                rawBoard = splitEvery 9 $ map charToInt rawSdkLine
                sudoku = toSudoku rawBoard in do
            putStrLn "\nParsing complete : \n"
            printBoard $ board sudoku
            putStrLn "\nShow the solution? (Y/N)"
            putStr ">>> "
            choiceLine <- getLine
            if length choiceLine /= 0 && (toUpper $ head choiceLine) == 'Y' then do
                putStrLn "\nNow solving...\n"
                printBoard $ board $ solveSudoku sudoku
                putStrLn "\nHave another puzzle to parse?"
                putStr ">>> "
                choiceLine1 <- getLine
                if length choiceLine1 /= 0 && (toUpper $ head choiceLine1) == 'Y' then do
                    putStrLn "\nInput the Sudoku puzzle to be parsed, in one 81-letter string!"
                    solveMain
                else do
                    putStrLn "\nNo more input : thanks for playing!\n"
            else do
                putStrLn "\nHave another puzzle to parse?"
                choiceLine2 <- getLine
                if length choiceLine2 /= 0 && (toUpper $ head choiceLine2) == 'Y' then do
                    putStrLn "Input the Sudoku puzzle to be parsed, in one 81-letter string!"
                    solveMain
                else do
                    putStrLn "\nNo more input : thanks for playing!\n"
    in do
        putStrLn "[ Each letters can be numbers(1-9) and blanks(any other letters) ]"
        putStrLn "Input the Sudoku puzzle to be parsed, in one 81-letter string!"
        solveMain
