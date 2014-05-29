-- Patrick Sheehan
-- CSCE 420 - Artificial Intelligence
-- PA1 - Enumerating State Spaces
--		 - Cracker Barrell Solitaire Game
-- Resources:
-- haskell.org
-- stackoverflow.com
-- http://www.danobrien.ws/PegBoard.html - referenced the concepts, not the code

module Main where

import Prelude
import Data.List

-- NOTE: solA - a solution where a single peg remains anywhere
--			 solC - a solution where a single peg remains in a corner


type Peg = (Integer, Integer, Integer, Integer) -- (row, col, pegNo, isFull)
type Board = [Peg] -- a single board of 15 pegs
type State = (Board, [Board], Wins)
type Wins = (Integer, Integer) -- (solA, solC)


data StateTree = Wins | State

-- This and the following helper function (evalChildBoards) form the following 
-- recursive pattern: a board is evaluated; if the board has possible moves that 
-- can be reached, evaluate all of those child boards, else return information 
-- about the board indicating if it is a winning state.
eval :: Board -> Wins
eval brd = 
	if cbs == [] then isWinner brd else evalChildBoards cbs
		
		where
			cbs = getMovesFromBoard brd


evalChildBoards :: [Board] -> Wins
evalChildBoards (b:bs) = addWins (eval b) (evalChildBoards bs)
evalChildBoards [] = (0,0)


addWins :: Wins -> Wins -> Wins
addWins (sa1, sc1) (sa2, sc2) = (sa1 + sa2, sc1 + sc2)

removeEmptyLists :: [Board] -> [Board]
removeEmptyLists (b:bs)
			| b == []   = removeEmptyLists bs
			| otherwise = [b] ++ removeEmptyLists bs
removeEmptyLists [] = []

getMovesFromBoard :: Board -> [Board]
getMovesFromBoard brd = removeEmptyLists (concat [getMovesAtPeg brd p | p <- brd])


getMovesAtPeg :: Board -> Peg -> [Board]
getMovesAtPeg brd p = [upRight]++[right]++[downRight]++[downLeft]++[left]++[upLeft]
		where 
			r = getRow p
			c = getCol p
			
			upRight   = makeJump p  (getPeg (r - 1) (c)     brd)  (getPeg (r - 2) (c)     brd)  brd
			right     = makeJump p  (getPeg (r)     (c + 1) brd)  (getPeg (r)     (c + 2) brd)  brd
			downRight = makeJump p  (getPeg (r + 1) (c + 1) brd)  (getPeg (r + 2) (c + 2) brd)  brd
			downLeft  = makeJump p  (getPeg (r + 1) (c)     brd)  (getPeg (r + 2) (c)     brd)  brd
			left      = makeJump p  (getPeg (r)     (c - 1) brd)  (getPeg (r)     (c - 2) brd)  brd
			upLeft    = makeJump p  (getPeg (r - 1) (c - 1) brd)  (getPeg (r - 2) (c - 2) brd)  brd


isWinner :: Board -> Wins
isWinner brd = (solA, solC)
		where
			solA = if (sum [getFull p | p <- brd, isValid p]) == 1 then 1 else 0
			solC = if ((getFull (getPegByNum 1 brd) == 1) || (getFull (getPegByNum 11 brd) == 1) || (getFull (getPegByNum 15 brd) == 1)) && solA == 1
							then 1 else 0

isJumpValid :: Peg -> Peg -> Peg -> Board -> Bool
isJumpValid startP midP endP brd = 
		(getFull startP == 1 && getFull midP == 1 && getFull endP == 0) -- full/empty is valid


isValid :: Peg -> Bool
isValid (x, y, pegNo, full) = 
		(0 <= x && x <= 4) && (0 <= y && y <= 4)
		&& (1 <= pegNo && pegNo <= 15) && (full == 1 || full == 0)

makeJump :: Peg -> Peg -> Peg -> Board -> Board
makeJump startP midP endP brd = 
		if isJumpValid startP midP endP brd then fillSlot endP (clearSlot midP (clearSlot startP brd))
		else []


clearSlot :: Peg -> Board -> Board
clearSlot p (p1:ps) = 
		if pegsEqual p p1 then ((clearPeg p1):ps)
		else (p1:(clearSlot p ps))
clearSlot _ [] = []

clearPeg :: Peg -> Peg
clearPeg (x, y, pegNo, _) = (x, y, pegNo, 0)


fillSlot :: Peg -> Board -> Board
fillSlot p (p1:ps) = 
		if pegsEqual p p1 then ((fillPeg p1):ps)
		else (p1:(fillSlot p ps))
fillSlot _ [] = []

fillPeg :: Peg -> Peg
fillPeg (x, y, pegNo, _) = (x, y, pegNo, 1)

pegsEqual :: Peg -> Peg -> Bool
pegsEqual (x1, y1, _, _) (x2, y2, _, _) = (x1==x2 && y1==y2)



getPegByNum :: Integer -> Board -> Peg
getPegByNum pegNo (p:ps) = 
		if getNum p == pegNo then p 
		else getPegByNum pegNo ps
getPegByNum _ [] = (-1,-1,-1,-1)

getPeg :: Integer -> Integer -> Board -> Peg
getPeg x y (peg_i:pegs) = 
		if (getRow peg_i == x) && (getCol peg_i == y) then peg_i
		else getPeg x y pegs 
getPeg x y [] = (-1, -1, -1, -1)	-- invalid peg if not found

getRow :: Peg -> Integer
getRow (x,_,_,_) = x

getCol :: Peg -> Integer
getCol (_,y,_,_) = y

getNum :: Peg -> Integer
getNum (_,_,n,_) = n

getFull :: Peg -> Integer -- 1 if full, 0 if empty, -1 if invalid
getFull (_, _, _, full) = full


main = do 
		let peg1  = (0,0,1,0)

		let peg2  = (1,0,2,1)
		let peg3  = (1,1,3,1)

		let peg4  = (2,0,4,1)
		let peg5  = (2,1,5,1)
		let peg6  = (2,2,6,1)

		let peg7  = (3,0,7,1)
		let peg8  = (3,1,8,1)
		let peg9  = (3,2,9,1)
		let peg10 = (3,3,10,1)

		let peg11 = (4,0,11,1)
		let peg12 = (4,1,12,1)
		let peg13 = (4,2,13,1)
		let peg14 = (4,3,14,1)
		let peg15 = (4,4,15,1)

		let fullboard = [peg1, peg2, peg3, peg4, peg5, peg6, peg7, peg8, peg9, peg10, peg11, peg12, peg13, peg14, peg15]
		print (eval fullboard)
