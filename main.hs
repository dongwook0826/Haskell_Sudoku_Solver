-- https://github.com/HansBlackCat/Haskell/blob/master/practice-haskell/src/Sudoku/Sudoku.hs
-- Thanks a lot :)

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
            printSudoku sudoku
            putStrLn "\nShow the solution? (Y/N)"
            putStr ">>> "
            choiceLine <- getLine
            if length choiceLine /= 0 && (toUpper $ head choiceLine) == 'Y' then do
                putStrLn "\nNow solving...\n"
                let solution = solveSudoku sudoku in do
                    printSudoku solution
                    putStrLn "\nLook for multiple solution? (Y/N)"
                    putStr ">>> "
                    choiceLine_ <- getLine
                    if length choiceLine /= 0 && (toUpper $ head choiceLine) == 'Y' then do
                        putStrLn "\nFar searching...\n"
                        printSudoku $ multSolveSudoku solution
                    else putStrLn "Stopped searching"
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

{-
000000000000003085001020000000507000004000100090000000500000073002010000000040009
800000000003600000070090200050007000000045700000100030001000068008500010090000400

043000280250090004800000009000782000080109070000345000500000007700050028068000590 (1451)
000003096000007403000460010008000062009000100450000300070016000605200000140500000 (1055)
000100000005040900023005004000001029070080060490500000100300870008090300000008000 (756)
100700405000060020007005609700000500030090040002000003406200300070010000203008006 (754)
072006000040090072000100003200080600060209010007010008500001000710050080000700350 (1453)
050807020300000007000534000804000709030080060706000205000358000500000002070201040 (752)
001860020000001409700004080900000860200000003016000007040200001607100000020096300 (1253)
030000040008030500400702001009107300800000002007208400600503004003020100090000060 (553)
987300005003001007000700034050090103000105000801030050370002000400600300100003726 (449)
026010050900200001800049000050100200402000109003002060000350007500001006030070510 (851)
000100007030706540000040020670009010009060400040300086090010000063208090800004000 (952)
030070004500400300000903010065000800400000001002000790020804000004005007100030020 (855)
076009000400010006100506900000000720000865000035000000001208009200050007000600580 (1254)
003009056006530002000061030010000300300107009005000080070310000500076200860900700 (751)
000000146000500208000010035020190000003804700000065090260080000509001000831000000 (753)
007802400000105000900070008010000080708050906030000070600010005000509000004603700 (954)
007240060900006000006009207045000009700080006200000780604500800000100005020068100 (552)
040008010910040083000300000800437100030601075006892005000005000560080031080200050 (549)
-}
