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
-- [Add your explanation of how your implementation works here]
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
-- [Add your explanation of how your implementation works here]
---------------------------------------
rotate :: Direction -> [a] -> [a]
rotate = undefined

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
-- [Add your explanation of how your implementation works here]
---------------------------------------
rotations :: Grid -> [Grid]
rotations = undefined

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
-- [Add your explanation of how your implementation works here]
---------------------------------------
steps :: Grid -> [Grid]
steps = undefined

--------------------------------------------------------------------------------
