-- main idea source :
-- https://dlbeer.co.nz/articles/sudoku.html
module Sudoku where

-- {-# LANGUAGE LambdaCase #-}

import Data.List

{- several test values -}

testRawBoard1 :: [[Int]]
testRawBoard1 = [[0,0,8,4,0,0,0,0,6],[0,1,6,8,0,7,0,0,0],[9,4,0,0,6,0,0,0,0],[8,6,0,0,0,4,0,5,0],[0,0,3,0,0,0,6,0,0],[0,5,0,3,0,0,0,9,2],[0,0,0,0,3,0,0,8,5],[0,0,0,7,0,8,4,6,0],[1,0,0,0,0,5,3,0,0]]

testBoard1 :: Board
testBoard1 = toBoard testRawBoard1

sweepedTestBoard1 :: Board
sweepedTestBoard1 = sweepPencilMark testBoard1

testRawBoard2 :: [[Int]]
testRawBoard2 = [[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,3,0,8,5],[0,0,1,0,2,0,0,0,0],[0,0,0,5,0,7,0,0,0],[0,0,4,0,0,0,1,0,0],[0,9,0,0,0,0,0,0,0],[5,0,0,0,0,0,0,7,3],[0,0,2,0,1,0,0,0,0],[0,0,0,0,4,0,0,0,9]]

testRawBoard3 :: [[Int]]
testRawBoard3 = [[8,0,0,0,0,0,0,0,0],[0,0,3,6,0,0,0,0,0],[0,7,0,0,9,0,2,0,0],[0,5,0,0,0,7,0,0,0],[0,0,0,0,4,5,7,0,0],[0,0,0,1,0,0,0,3,0],[0,0,1,0,0,0,0,6,8],[0,0,8,5,0,0,0,1,0],[0,9,0,0,0,0,4,0,0]]


testRawBoard1Solved :: [[Int]]
testRawBoard1Solved = [[7,2,8,4,1,9,5,3,6],[3,1,6,8,5,7,2,4,9],[9,4,5,2,6,3,7,1,8],[8,6,7,9,2,4,1,5,3],[2,9,3,5,8,1,6,7,4],[4,5,1,3,7,6,8,9,2],[6,7,4,1,3,2,9,8,5],[5,3,2,7,9,8,4,6,1],[1,8,9,6,4,5,3,2,7]]

testBoard1Solved :: Board
testBoard1Solved = toBoard testRawBoard1Solved

sweepedTestBoard1Solved :: Board
sweepedTestBoard1Solved = sweepPencilMark testBoard1Solved -- no change


testRawBoard1Hole :: [[Int]]
testRawBoard1Hole = [[7,2,8,4,1,9,5,3,6],[3,1,6,8,5,7,2,4,9],[9,4,5,2,6,3,7,1,8],[8,6,7,9,2,4,1,5,3],[2,9,3,5,8,1,6,7,4],[4,5,1,3,7,6,8,9,2],[6,7,4,1,3,2,9,8,5],[5,3,2,7,9,8,4,6,1],[1,8,9,6,4,5,3,2,0]]

testBoard1Hole :: Board
testBoard1Hole = toBoard testRawBoard1Hole

sweepedTestBoard1Hole :: Board
sweepedTestBoard1Hole = sweepPencilMark testBoard1Hole -- no change


invalidTestRawBoard :: [[Int]]
invalidTestRawBoard = [[1,2,3,4,5,6,7,8,0],[0,0,0,0,0,0,0,0,9],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0]]

invalidTestBoard :: Board
invalidTestBoard = toBoard invalidTestRawBoard

sweepedInvalidTestBoard :: Board
sweepedInvalidTestBoard = sweepPencilMark invalidTestBoard


{- Data type & constants definition -}

data Area = OneCell | Row | Column | Box deriving (Show, Eq, Ord)

type Cell = [Bool] -- length-10; isAlreadyFilled : possibleCandidates
type Board = [[Cell]] -- includes cases of being a part of whole board
data Step = Fixed (Int, (Int, Int)) -- (candNum, (row, col))
          | Branching (Int, Area, (Int, Int), Cell) deriving (Show, Eq, Ord) -- (candNum, area, (areaInd, cellInd), cell)

getStepNum :: Step -> Int
getStepNum step = case step of Fixed (n,_) -> n
                               Branching (n,_,_,_) -> n

getStepArea :: Step -> Area
getStepArea step = case step of Fixed (_,_) -> OneCell
                                Branching (_,area,_,_) -> area

getStepIx :: Step -> (Int, Int)
getStepIx step = case step of Fixed (_,ixTup) -> ixTup
                              Branching (_,_,ixTup,_) -> ixTup

data Sudoku = Sudoku { board :: Board
                     , process :: [Step]
                     , valid :: Bool
                     , solutionCnt :: Int } deriving (Show, Eq, Ord)

cellBox :: (Int, Int) -> Int
cellBox (r, c)
    | r<3       = if      c<3 then 0
                  else if c<6 then 1
                  else             2
    | r<6       = if      c<3 then 3
                  else if c<6 then 4
                  else             5
    | otherwise = if      c<3 then 6
                  else if c<6 then 7
                  else             8


-- inverse of cellBox
boxCell :: [[(Int, Int)]]
boxCell = [[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)],[(0,3),(0,4),(0,5),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)],[(0,6),(0,7),(0,8),(1,6),(1,7),(1,8),(2,6),(2,7),(2,8)],[(3,0),(3,1),(3,2),(4,0),(4,1),(4,2),(5,0),(5,1),(5,2)],[(3,3),(3,4),(3,5),(4,3),(4,4),(4,5),(5,3),(5,4),(5,5)],[(3,6),(3,7),(3,8),(4,6),(4,7),(4,8),(5,6),(5,7),(5,8)],[(6,0),(6,1),(6,2),(7,0),(7,1),(7,2),(8,0),(8,1),(8,2)],[(6,3),(6,4),(6,5),(7,3),(7,4),(7,5),(8,3),(8,4),(8,5)],[(6,6),(6,7),(6,8),(7,6),(7,7),(7,8),(8,6),(8,7),(8,8)]]
-- and this needed to be in line...

emptyCell :: Cell
emptyCell = [False, True, True, True, True, True, True, True, True, True]

{- [[Int]] -> Sudoku parsing function a.k.a. generator -}

toSudoku :: [[Int]] -> Sudoku
toSudoku rawl = Sudoku {board = sweepPencilMark $ toBoard rawl, process = [],
                        valid = True, solutionCnt = 0 }

toBoard :: [[Int]] -> Board -- any non-1~9 ints for empty cells
toBoard []       = []
toBoard (row:rows) = toBoardRowwise row : toBoard rows

toBoardRowwise :: [Int] -> [Cell]
toBoardRowwise [] = []
toBoardRowwise (n:ns)
    | n >= 1 && n <= 9 = (True : [k == n | k<-[1..9]]) : toBoardRowwise ns
    | otherwise        = emptyCell : toBoardRowwise ns


{----------- Strict solving & supporting functions -----------}

solveSudoku :: Sudoku -> Sudoku
solveSudoku sdk
    | solutionCnt sdk >= 1 = sdk
    | otherwise = solveSudoku $ takeStep sdk

takeStep :: Sudoku -> Sudoku
takeStep sdk
    | valid sdk   = let setInfo = minCandInfo $ board sdk
                        candCnt = (\(a,_,_,_) -> a) $ setInfo
                        area = (\(_,a,_,_) -> a) $ setInfo in
                    if candCnt == 0 then undoStep sdk
                    else if candCnt >= 10 then
                        Sudoku { board = board sdk
                               , process = process sdk
                               , valid = True
                               , solutionCnt = 1 }
                    else
                        let brd = board sdk in
                        if area == OneCell then
                            let ixTup = (\(_,_,r,c) -> (r,c)) setInfo
                                tgCell = brd !! fst ixTup !! snd ixTup
                                candNum = getFirstNum $ tgCell in
                            if candCnt == 1 then
                                Sudoku { board = fixCell brd candNum ixTup
                                       , process = Fixed (candNum, ixTup) : (process sdk)
                                       , valid = True
                                       , solutionCnt = solutionCnt sdk }
                            else -- candCnt > 1
                                Sudoku { board = fixCell brd candNum ixTup
                                       , process = Branching (candNum, OneCell, ixTup, tgCell) : (process sdk)
                                       , valid = True
                                       , solutionCnt = solutionCnt sdk }
                        else {- area `elem` [Row, Column, Box] -}
                            let candNum = (\(_,_,a,_) -> a) $ setInfo
                                aix = (\(_,_,_,a) -> a) $ setInfo
                                ixTup' = (aix, getNextCell brd aix area candNum $ negate 1)
                                ixTup = if area == Row then ixTup'
                                        else if area == Column then (\(c,r) -> (r,c)) ixTup'
                                        else {- area == Box -} boxCell !! aix !! snd ixTup'
                                tgCell = brd !! fst ixTup !! snd ixTup in
                            if candCnt == 1 then
                                Sudoku { board = fixCell brd candNum ixTup
                                       , process = Fixed (candNum, ixTup) : (process sdk)
                                       , valid = True
                                       , solutionCnt = solutionCnt sdk }
                            else -- candCnt > 1
                                Sudoku { board = fixCell brd candNum ixTup
                                       , process = Branching (candNum, area, ixTup', tgCell) : (process sdk)
                                       , valid = True
                                       , solutionCnt = solutionCnt sdk }
    | otherwise   = undoStep sdk

undoStep :: Sudoku -> Sudoku
undoStep sdk
    | process sdk /= [] = let brd = board sdk
                              recentStep = head $ process sdk in
                          case recentStep of
                                Fixed (_,_) ->
                                     Sudoku { board = unfixCell brd recentStep
                                            , process = tail $ process sdk
                                            , valid = False
                                            , solutionCnt = solutionCnt sdk }
                                Branching (n,area,(aix,cix),cell) ->
                                     case area of
                                        OneCell ->
                                            let newNum = getNextNum n cell in
                                            if newNum > 9 then
                                                Sudoku { board = unfixCell brd recentStep
                                                       , process = tail $ process sdk
                                                       , valid = False
                                                       , solutionCnt = solutionCnt sdk }
                                            else
                                                Sudoku { board = fixCell (sweepPencilMark $ unfixCell brd recentStep)
                                                                 newNum (aix,cix)
                                                       , process = Branching (newNum, OneCell, (aix,cix), cell)
                                                                   : (tail $ process sdk)
                                                       , valid = True
                                                       , solutionCnt = solutionCnt sdk }
                                        _ ->
                                            let newTup = (aix, getNextCell brd aix area n cix) in
                                            if snd newTup >= 9 then
                                                Sudoku { board = unfixCell brd recentStep
                                                       , process = tail $ process sdk
                                                       , valid = False
                                                       , solutionCnt = solutionCnt sdk }
                                            else
                                                let newTup' = if area == Row then newTup
                                                              else if area == Column then (\(c,r) -> (r,c)) newTup
                                                              else {- area == Box -} boxCell !! aix !! snd newTup
                                                    newCell = brd !! fst newTup' !! snd newTup'
                                                    newStep = Branching (n, area, newTup, newCell) in
                                                Sudoku { board = fixCell (sweepPencilMark $ unfixCell brd recentStep)
                                                                 n newTup'
                                                       , process = newStep : (tail $ process sdk)
                                                       , valid = True
                                                       , solutionCnt = solutionCnt sdk }
    | otherwise = sdk 
    

---------------

-- get fixed num for cell, or smallest possible candidate
getFirstNum :: Cell -> Int
getFirstNum cell = (length $ takeWhile not $ tail cell) + 1 -- = getNextNum 0 cell

getNextNum :: Int -> Cell -> Int
getNextNum num cell = (length $ takeWhile not $ drop (num+1) cell) + num + 1

-- get fixed cell for num at given area, or nearest possible cell
getNextCell :: Board -> Int -> Area -> Int -> Int -> Int -- board, ?'th ?area, scan for num ?, start fm next of cell ind ?
getNextCell board aix area num cix 
    | cix >= 9       = 9
    | area == Row    = getNextNum cix $ transpose (board !! aix) !! num
    | area == Column = getNextNum cix $ transpose (transpose board !! aix) !! num
    | area == Box    = getNextNum cix $ transpose (dismantleBoxes board !! aix) !! num
    | otherwise      = getNextNum num $ board !! aix !! cix -- = getNextNum
-- when getting first cell, then cix must be negate 1 (= -1)

---------------

minCandInfo :: Board -> (Int, Area, Int, Int)
minCandInfo board = globalMin $ applyEach board [minCandidateInd, minAreaInfo]

minCandidateInd :: Board -> (Int, Area, Int, Int) -- ( candCnt, OneCell, row, col )
minCandidateInd board = (\(a,(b,c)) -> (a,OneCell,b,c)) $ globalMin $ zipWith minCandidateIndRowwise board $ [0..8]

minCandidateIndRowwise :: [Cell] -> Int -> (Int, (Int, Int))
minCandidateIndRowwise cls r = globalMin $ zip (map candidateCnt cls) [(r,c) | c <- [0..8]]

---------------

minAreaInfo :: Board -> (Int, Area, Int, Int) -- (candCnt, which area, candNum, i'th area)
minAreaInfo board = globalMin $ applyEach board [minRowCandInfo, minColumnCandInfo, minBoxCandInfo]

minRowCandInfo :: Board -> (Int, Area, Int, Int) -- ( candCnt, Row, candNum, rowi )
minRowCandInfo board = (\((a,b),c) -> (a,Row,b,c)) $ globalMin $ zip (map minRowCandInfoRowwise board) [0..8]

minRowCandInfoRowwise :: [Cell] -> (Int, Int) -- (candCnt, candNum)
minRowCandInfoRowwise row = globalMin $
                            [(10,fix) | fix <- fixedNumList]
                            ++ [ntup | ntup <- candCntList, snd ntup `notElem` fixedNumList]
    where fixedNumList = [getFirstNum cl | cl <- row, head cl]
          candCntList = zip (map candCnt $ tail $ transpose row) [1..9]
          candCnt cndr = foldl (\n b -> if b then n+1 else n) 0 cndr

-- transpose, then get minRowCandInfo; ( cnt, Col, num, coli )
minColumnCandInfo :: Board -> (Int, Area, Int, Int)
minColumnCandInfo board = (\((a,b),c) -> (a,Column,b,c)) $ globalMin
                          $ zip (map minRowCandInfoRowwise $ transpose board) [0..8]

-- dismantleBoxes, then get minRowCandInfo; ( cnt, Box, num, boxi )
minBoxCandInfo :: Board -> (Int, Area, Int, Int)
minBoxCandInfo board = (\((a,b),c) -> (a,Box,b,c)) $ globalMin
                       $ zip (map minRowCandInfoRowwise $ dismantleBoxes board) [0..8]

dismantleBoxes :: Board -> Board
dismantleBoxes board = concat $ map (map concat . transpose . map (splitEvery 3)) $ splitEvery 3 board

rebuildBoxes :: Board -> Board -- do we need it...?
rebuildBoxes = dismantleBoxes

---------------

fixingCell :: Int -> Cell
fixingCell n
    | n >= 1 && n <= 9 = take 10 $ True : (take (n-1) $ repeat False) ++ True : (repeat False)
    | otherwise        = emptyCell

toBeFixed :: Cell -> Bool -- do we need it...? 2
toBeFixed cell = if candidateCnt cell == 1 then True
                 else                           False

candidateCnt :: Cell -> Int
candidateCnt cell = if head cell then 10 -- catches cells already fixed
                    else foldl (\n b -> if b then n+1 else n) 0 $ tail cell

sweepPencilMark :: Board -> Board
sweepPencilMark board = foldl sweepPencilMarkRowwise board [0..8]

sweepPencilMarkRowwise :: Board -> Int -> Board
sweepPencilMarkRowwise board r = foldl (\brd c -> if head $ brd!!r!!c then erasePencilMark brd (r,c) else brd)
                                       board [0..8]
-- sweepPencilMarkCellWise = erasePencilMark

---------------

fixCell :: Board -> Int -> (Int, Int) -> Board
fixCell brd n (r,c) = erasePencilMark' brd n (r,c) (cellsConnected (r,c))

-- erase marks of the number fixed at (r, c), for all members of connectedCells (r, c);
-- ePM and rPM auto-fixes(or unfixes) cell (r, c)
erasePencilMark :: Board -> (Int, Int) -> Board
erasePencilMark board (r,c) = erasePencilMark' board (getFirstNum $ board !! r !! c) (r,c) (cellsConnected (r,c))

erasePencilMark' :: Board -> Int -> (Int, Int) -> [[Bool]] -> Board -- board, ix, n, erasing cells info
erasePencilMark' [] _ _ _ = []
erasePencilMark' _ _ _ [] = []
erasePencilMark' (row:rows) n (r,c) (cnrow:cnrows) = erasePencilMarkRowwise row n (r,c) cnrow
                                                     : erasePencilMark' rows n (r-1,c) cnrows

erasePencilMarkRowwise :: [Cell] -> Int -> (Int, Int) -> [Bool] -> [Cell]
erasePencilMarkRowwise [] _ _ _ = []
erasePencilMarkRowwise _ _ _ [] = []
erasePencilMarkRowwise (cl:cls) n (r,c) (cn:cns) = renewedCell : erasePencilMarkRowwise cls n (r,c-1) cns
    where renewedCell = if      cn           then erasePencilMarkCellwise cl n
                        else if (r,c)==(0,0) then fixingCell n -- auto-fix
                        else                      cl

erasePencilMarkCellwise :: Cell -> Int -> Cell
erasePencilMarkCellwise cell n
    | head cell = cell
    | otherwise = zipWith (&&) cell ((take n $ repeat True) ++ False:(repeat True))

---------------

unfixCell :: Board -> Step -> Board
unfixCell brd step   = case step of Fixed (n,(r,c)) ->
                                        retakePencilMark brd n (r,c) emptyCell (cellsConnected (r,c))
                                    Branching (n,area,(aix,cix),cell) -> -- cell refilling process for branch change
                                        let posIx = case area of OneCell -> (aix,cix)
                                                                 Row -> (aix,cix)
                                                                 Column -> (cix,aix)
                                                                 Box -> boxCell !! aix !! cix
                                        in retakePencilMark brd n posIx cell (cellsConnected posIx)

retakePencilMark :: Board -> Int -> (Int, Int) -> Cell -> [[Bool]] -> Board
retakePencilMark [] _ _ _ _ = []
retakePencilMark _ _ _ _ [] = []
retakePencilMark (row:rows) n (r,c) pvCell (cnrow:cnrows) = retakePencilMarkRowwise row n (r,c) pvCell cnrow
                                                            : retakePencilMark rows n (r-1,c) pvCell cnrows

retakePencilMarkRowwise :: [Cell] -> Int -> (Int, Int) -> Cell -> [Bool] -> [Cell]
retakePencilMarkRowwise [] _ _ _ _ = []
retakePencilMarkRowwise _ _ _ _ [] = []
retakePencilMarkRowwise (cl:cls) n (r,c) pvCell (cn:cns) = renewedCell : retakePencilMarkRowwise cls n (r,c-1) pvCell cns
    where renewedCell = if      cn           then retakePencilMarkCellwise cl n
                        else if (r,c)==(0,0) then pvCell
                        else                      cl

retakePencilMarkCellwise :: Cell -> Int -> Cell
retakePencilMarkCellwise cell n
    | head cell = cell
    | otherwise = zipWith (||) cell ((take n $ repeat False) ++ True:(repeat False))

---------------

-- search for all cell indices influenced by given cell index
cellsConnected :: (Int, Int) -> [[Bool]]
cellsConnected = cellsConnected' . connectedCellsGrouped

cellsConnected' :: [[(Int, Int)]] -> [[Bool]]
cellsConnected' [] = []
cellsConnected' (ix:ixs) = cellsConnectedRowwise ix : cellsConnected' ixs

cellsConnectedRowwise :: [(Int, Int)] -> [Bool]
cellsConnectedRowwise rcs = take 9 $ (cellsConnectedRowwise' rcs 0) ++ repeat False

cellsConnectedRowwise' :: [(Int, Int)] -> Int -> [Bool]
cellsConnectedRowwise' [] _ = []
cellsConnectedRowwise' (rc:rcs) ci = if snd rc < ci then []
                                     else if snd rc == ci then
                                         True : cellsConnectedRowwise' rcs (ci+1)
                                     else
                                         False : cellsConnectedRowwise' (rc:rcs) (ci+1)

connectedCellsGrouped :: (Int, Int) -> [[(Int, Int)]]
connectedCellsGrouped (r, c) = groupBy (\(x1,_) (x2,_) -> x1==x2) $
                               sort. delete (r, c) . nub $
                               [(r, ci) | ci <- [0..8]] ++ [(ri, c) | ri <- [0..8]] ++ boxCell !! cellBox (r, c)


{- utility functions -}

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n ls = fls : splitEvery n sls
    where (fls,sls) = splitAt n ls

globalMin :: (Ord a) => [a] -> a
globalMin list = foldl1 min $ list

applyEach :: a -> [a -> b] -> [b]
applyEach _ [] = []
applyEach x (f:fs) = (f x) : applyEach x fs

applyTimes :: a -> (a -> a) -> Int -> a
applyTimes x f n
    | n <= 0    = x
    | otherwise = applyTimes (f x) f (n-1)
