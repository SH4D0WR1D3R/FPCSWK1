--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Large Arithmetic Collider                                    --
--------------------------------------------------------------------------------

module Game where

--------------------------------------------------------------------------------

-- | Represents different actions (including their parameters) that a cell can 
-- have on a row or column total.
data Action
    = Add Int
    | Sub Int
    deriving (Eq, Ord, Show)

-- | Represents a cell including whether it is enabled and its action.
data Cell = MkCell Bool Action
    deriving (Eq, Show)

-- | A row has a target number and consists of zero or more cells.
data Row = MkRow Int [Cell]
    deriving (Eq, Show)

-- | A grid is comprised of the target numbers for all columns and the rows.
data Grid = MkGrid [Int] [Row]
    deriving (Eq, Show)

-- | Enumerates directions in which lists can be rotated.
data Direction = L | R
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | `eval` @action total@ applies @action@ to the running @total@. 
-- For example:
--
-- >>> eval (Add 5) 3
-- 8
--
-- >>> eval (Sub 1) 3
-- 2
--
---------------------------------------
-- If the Action value is Add x, then we need to add x to the other value passed into eval as an argument
-- If the Action value is Sub x, then we need to sub x from the other value passed into eval as an argument
---------------------------------------
eval :: Action -> Int -> Int
eval (Add f) x = f + x
eval (Sub f) x = x - f

-- | `apply` @cell total@ applies the action of @cell@ to the running @total@ 
-- if @cell@ is enabled. For example:
--
-- >>> apply (MkCell True (Add 5)) 3
-- 8
--
-- >>> apply (MkCell False (Add 5)) 3
-- 3
--
---------------------------------------
-- If the cell is not enabled, then we can just return the value passed in
-- If the cell is enabled, we can add the current value to the value passed in based on using eval with the function passed in
---------------------------------------
apply :: Cell -> Int -> Int
apply (MkCell False _) n = n
apply (MkCell True f) n = eval f n

-- | `result` @cells@ calculates the total produced by the actions of all 
-- enabled cells in @cells@ starting from 0. For example:
--
-- >>> result []
-- 0
--
-- >>> result [MkCell True (Add 5), MkCell False (Add 5), MkCell True (Sub 1)]
-- 4
--
---------------------------------------
-- Runs through the cell list. If empty, return 0
-- Else, run through the list, and apply the cell value to the accumulator function
---------------------------------------
result :: [Cell] -> Int
result [] = 0
result xs = foldr (\x acc -> apply x acc) 0 xs

-- | `states` @cell@ is a function which returns a list with _exactly_ two
-- elements that represent the two different states @cell@ can be in. For
-- example:
--
-- >>> states (MkCell False (Add 5))
-- [MkCell True (Add 5), MkCell False (Add 5)]
--
---------------------------------------
-- There are only two states, True and False. Both instances of this cell can be added to the list by taking the cell as an argument and adding both instances to the list
---------------------------------------
states :: Cell -> [Cell]
states (MkCell _ f) = [MkCell True f, MkCell False f]

-- | `candidates` @cells@ is a function which, given a list of cells in a row,
-- produces all possible combinations of states for those cells. For example:
-- 
-- >>> candidates [MkCell False (Add 5), MkCell False (Sub 1)]
-- [ [MkCell False (Add 5), MkCell False (Sub 1)]
-- , [MkCell False (Add 5), MkCell True (Sub 1)]
-- , [MkCell True (Add 5), MkCell False (Sub 1)]
-- , [MkCell True (Add 5), MkCell True (Sub 1)]
-- ]
--
---------------------------------------
-- Here, the base case is the empty list. We run through the states of x - the head of the function - and for each state, we call the candidates function recursively until we reach the base case,
-- where we then move on to the next state for x. 
---------------------------------------
candidates :: [Cell] -> [[Cell]]
candidates [] = [[]]
candidates (x:xs) = [y:ys | y <- states x, ys <- candidates xs]

-- | `solveRow` @row@ finds solutions for @row@. For example:
--
-- >>> solveRow (MkRow 5 [MkCell False (Add 5), MkCell False (Sub 1)])
-- [ MkRow 5 [MkCell True (Add 5), MkCell False (Sub 1)]]
--
-- >>> solveRow (MkRow 5 [MkCell False (Add 5), MkCell False (Add 5)])
-- [ MkRow 5 [MkCell True (Add 5), MkCell False (Add 5)] 
-- , MkRow 5 [MkCell False (Add 5), MkCell True (Add 5)]
-- ]
--
---------------------------------------
-- This takes a row and outputs a list of rows with different states for each cell. Each row output is a different solution for the row. We apply candidates to the [Cell] to find all possible combinations
-- of states. We then run through this result of the candidates function application, and apply result to each element of the list. If this result equals n, then we add it to the final output of the function
---------------------------------------
solveRow :: Row -> [Row]
solveRow (MkRow n xs) = [MkRow n y | y <- candidates xs, result y == n]

-- | `solve` @grid@ finds all solutions for @grid@. For example:
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
-- >>> solve (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5,2] [ MkRow 3 [MkCell True (Add 3), MkCell False (Add 5)]
--                , MkRow 4 [MkCell True (Add 2), MkCell True (Add 2)]
--                ]
-- ]
-- getColumns [5, 2] [ MkRow 3 [MkCell True (Add 3), MkCell False (Add 5)], MkRow 4 [MkCell True (Add 2), MkCell True (Add 2)]
---------------------------------------
-- Here, I've implemented the solve function with multiple helper functions. Looking at solve, it takes a grid as an argument. We then use list comprehension to run through a list of all the candidate grids, and 
-- return the ones which are valid solutions. Looking at candidateGrids, this takes a list of a list of rows, and returns the same data type. The edge cases are an empty list, and a list with one element. If there 
-- is one element, we run through the list passed in, and returned a list of each element in it. Otherwise, we run through the list passed in, taking the head and tail. This acts as a recursive function, adding the 
-- head to the output, and running candidateGrids on the tail, running through each element of that. This ends up generating a list of cells for each potential grid, their states varying for each one. 
-- For example, we could have the following grid
--    5   2
-- 3  +3  +5
-- 4  +2  +2
-- Different grids can be generated by running candidateGrids. For example
--    5   2            5   2          5   2
-- 3  _   _        3   _  +5      3  +3  +5
-- 4  +2  +2       4  +2  +2      4  +2   _
-- We then run through all of these grids, and check which ones are valid solutions that help us reach the targets. This check is done using gridCorrect. Looking at gridCorrect, it takes a grid and returns a bool depending 
-- on if the grid is a valid solution or not. There is a list comprehension to run through the columns of the grid, since we know the rows in the grid are valid solution (come back to this later). getColumns runs the getColumn
-- function for each column in the grid, rearranging it so the grid can be sorted by columns rather than by rows. For each column, we then check if it is a valid row (i.e. a valid solution) back in the grid correct. This 
-- checks if the result for the row is equal to the target. This will create a list of bools for the gridCorrect, which can then be translated into a single bool using and. This combines all the bools in the list. We want all
-- of them to be valid solutions, hence, true will only be returned if every column works. 
-- Back in solve, we run candidateGrids on allRows xs. allRows xs will generate all the possible solutions for each of the rows, hence why we know that all the rows will be valid solutions no matter what, and we only need to
-- worry about the columns working.
---------------------------------------
allRows :: [Row] -> [[Row]] -- solves each row in the list of rows
allRows xs = [solveRow y | y <- xs]

getCell :: Int -> Row -> Cell -- gets a cell from a row
getCell n (MkRow _ xs) = xs!! n

getColumn :: Int -> Int -> [Row] -> Row -- gets one column based on a list of rows
getColumn n c xs = MkRow c [getCell n y | y <- xs] -- n is the column, c is the aim for the total of the column, xs is the row

getColumns :: [Int] -> [Row] -> [Row] -- gets all columns for a list of rows
getColumns x xs = [getColumn y (x!! y) xs | y <- [0.. (length x) - 1]]

candidateGrids :: [[Row]] -> [[Row]]
candidateGrids [] = []
candidateGrids [xs] = [[y] | y <- xs]
candidateGrids (x:xs) = [z:y | z <- x, y <- candidateGrids xs]

validRow :: Row -> Bool
validRow (MkRow n xs) = result xs == n

gridCorrect :: Grid -> Bool
gridCorrect (MkGrid n xs) = and [validRow y | y <- getColumns n xs]

solve :: Grid -> [Grid]
solve (MkGrid [] []) = [MkGrid [] []]
solve (MkGrid n xs) = [MkGrid n y | y <- candidateGrids (allRows xs), gridCorrect (MkGrid n y)]

-- | `rotate` @direction list@ rotates the items in @list@ to the left or
-- right depending on the value of @direction@. For example:
--
-- >>> rotate L [1,2,3]
-- [2,3,1]
--
-- >>> rotate R [1,2,3]
-- [3,1,2]
--
-- >>> rotate L []
-- []
--
-- >>> rotate R [1]
-- [1]
--
---------------------------------------
-- If empty list, return empty list
-- If rotating left, left most element should go to end of list
-- If rotating right, right most element should go to start of list
---------------------------------------
rotate :: Direction -> [a] -> [a]
rotate _ [] = []
rotate L (x:xs) = xs ++ [x]
rotate R xs = (last xs):(init xs)

-- | `rotations` @grid@ returns a list of grids containing all possible ways 
-- to rotate @grid@. This means the resulting list should normally have 
-- rows + columns many elements. For example:
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
-- >>> rotations (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5,2] [ MkRow 3 [MkCell False (Add 5), MkCell False (Add 3)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 2), MkCell False (Add 5)]
--                , MkRow 4 [MkCell False (Add 3), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 3), MkCell False (Add 2)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 5)]
--                ]
-- ]
--
---------------------------------------
-- rotations as a function calls the same method on the columns and rows, but converts the columns so they are demonstrated in the correct format as well
-- convert makes sure the grid is displayed according to rows rather than columns
-- rotateRows is used to run through each of the rows and rotate them once, generating a new grid each time based on the original grid
-- It calls rotateGrid which splits the rows at a specific index and inserts the rotated row. 
-- rotate' is a helper function which makes it easier when working with rows, since rotate as a function only accepts lists of cells

---------------------------------------
rotate' :: Row -> Row
rotate' (MkRow n xs) = MkRow n (rotate L xs)

rotateGrid :: Int -> Grid -> Grid
rotateGrid _ (MkGrid [] []) = MkGrid [] []
rotateGrid _ (MkGrid [x] [y]) = MkGrid [x] [y]
rotateGrid n (MkGrid x xs) = MkGrid x (fst(splitAt n xs) ++ ((rotate' (xs!! n)):(tail(snd(splitAt n xs)))))

rotateRows :: Grid -> [Grid]
rotateRows (MkGrid x xs) = [rotateGrid n (MkGrid x xs) | n <- [0..((length xs)-1)]]

getRowTargets :: [Row] -> [Int]
getRowTargets [] = []
getRowTargets ((MkRow n _):xs) = n:(getRowTargets xs)

convert :: Grid -> Grid
convert (MkGrid [] []) = MkGrid [] []
convert (MkGrid n xs) = MkGrid (getRowTargets xs) (getColumns n xs)

rotations :: Grid -> [Grid]
rotations xs = (rotateRows xs) ++ [convert y | y <- rotateRows (convert xs)]


-- | `steps` @grid@ finds the sequence of rotations that lead to a solution 
-- for @grid@ in the fewest number of rotations. The resulting list includes 
-- the solution as the last element. You may assume that this function will
-- never be called on a @grid@ for which there are solutions returned by
-- `solve`. The states of intermediate grids in the resulting list
-- are irrelevant - only the ones of the final grid need to be set correctly.
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 2), MkCell False (Add 3)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 5), MkCell False (Add 2)]
-- >>> steps (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5, 2] [ MkRow 3 [ MkCell False (Add 5), MkCell False (Add 3)] 
--                 , MkRow 4 [ MkCell False (Add 2), MkCell False (Add 2)]
--                 ]  
-- , MkGrid [5, 2] [ MkRow 3 [ MkCell True (Add 3), MkCell False (Add 5)] 
--                 , MkRow 4 [ MkCell True (Add 2), MkCell True (Add 2)]
--                 ] 
-- ]
--
---------------------------------------
-- This function implements breadth first search to complete the aim. steps calls steps' which is defined to take more arguments to work with
-- steps' takes a queue which holds the paths to all nodes visited. This is a list of list of grids. E.g.
-- Let's say we start with a grid, 1. Running rotations on it would generate 3 other grids, 2, 3 and 4. The queue would now be [[2], [3], [4]]. For the sake of this example,
-- no rotations from 1 generate a solution.
-- The function then moves on to the next node in the queue - [2]. Rotations is run on this again. It generates 5, 6, 7. Let's say 5 is the same as 1. 5 is then ignored and 
-- not added to the queue - this check is done by checkGrids which is run in rotations'. So now the queue is [[2], [3], [4], [2, 6], [2, 7]]
-- Then rotations is run on [3] as we've moved to the next path in the queue. Let's say it generates a solution - 8. This is checked for in the guard statements of steps'
-- and the path [3, 8] is output as a result of finding a solution
---------------------------------------
checkGrids :: Grid -> [Grid] -> [Grid]
checkGrids _ [] = []
checkGrids g visited = [y | y <- rotations g, z <- visited, y /= z]

rotations' :: [[Grid]] -> [Grid] -> [Grid]
rotations' g = checkGrids (head (head g))

steps' :: [[Grid]] -> [Grid] -> [Grid]
steps' queue visited
    | not (null (concatMap solve (rotations' queue visited))) = head (head (filter (not.null) (map solve (rotations' queue visited)))):head queue
    | otherwise = steps' (tail (queue ++ [r:head queue | r <- rotations' queue visited])) visited

steps :: Grid -> [Grid]
steps g = drop 1 (reverse (steps' [[g]] [g]))
--------------------------------------------------------------------------------
