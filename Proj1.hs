{- Code for the all the functions for Project 1 Chord Probe Game
   COMP30020 Project 1
   Wei How Ng (828472)
-}
-------------------------------------------------------------------------------
--Initial module includes
module Proj1 (initialGuess, nextGuess, GameState) where
-------------------------------------------------------------------------------
--Standard Library Importations
import Data.List
--Type Declarations
--GameState used to store allGuess (remaining guesses)
type GameState = [[String]]
--Constant for a null number
_NULLNUM = (-1)
-------------------------------------------------------------------------------
--combThreefunction to turn list into combinations of 3 for each guess
--Recursively call itself on smaller subsets of combinations of the lists
--Inputs: A list to make the combinations and an integer for the comb number
--Outputs: all possible combinations of n combinations from the list
combThree :: [a] -> Int -> [[a]]
--base cases
combThree _ 0 = [[]]
combThree [] _ = []
--recursive calls to map each element with the rest and string them together
combThree (x:xs) k = (map (x:) (combThree xs (k-1)) ++ (combThree xs k))
-------------------------------------------------------------------------------

{-=============================================================================
===============================GUESSING FUNCTIONS==============================
=============================================================================-}
--initialGuess function
--guess the pitches initially, returns the guesses and gamestate
--Outputs: guess responses and GameState of allGuess
initialGuess :: ([String], GameState)
initialGuess = (["A1","B1","C2"], allGuess)
    --initialising the list of all possible pitches
    where pitches = ["A1","A2","A3","B1","B2","B3","C1","C2","C3","D1","D2","D3"
                    ,"E1","E2","E3","F1","F2","F3","G1","G2","G3"]
          allGuess = combThree pitches 3
-------------------------------------------------------------------------------
--nextGuess function to guess the pitches with received information
--afterwards by reducing allGuess and using the minimax method
--Inputs: response and previous GameState, tuple score
--Outputs: nextguess and updated GameState
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (response, allGuess) score = output
    where reducedAllGuess = reduceAllGuess allGuess response score
          output = ((minimax reducedAllGuess), reducedAllGuess)
-------------------------------------------------------------------------------

{-=============================================================================
===============================MINIMAX FUNCTIONS==============================
=============================================================================-}
--minimax function which takes the GameState and derives the next best guess
--the GameState contains the reduced list of allGuess
--Inputs: GameState (allGuess)
--Outputs: the nextGuess
minimax :: [[String]]-> [String]
minimax allGuess = output
    where minList = minListBuild allGuess allGuess
          indexOfMax = maybeNot(elemIndex (maximum minList) minList)
          output = allGuess !! indexOfMax
-------------------------------------------------------------------------------
--minListBuild function to build a list of scores for each guess in allGuess
--list would be the same length of allGuess
--Inputs: two list of allGuess (reduced)
--Ouputs: List of scores pertaining to each guess in allGuess
minListBuild :: [[String]] -> [[String]] -> [Int]
minListBuild _ [] = []
minListBuild allGuess (c:cs) = (minRemove c allGuess):(minListBuild allGuess cs)
-------------------------------------------------------------------------------
--maybeNot function which takes the Maybe Int that elemIndex ouputs and turns
--it into an Int
--Inputs: Maybe Int
--Outputs: Int for the index
maybeNot :: (Maybe Int) -> Int
maybeNot Nothing = _NULLNUM
maybeNot (Just n) = n
-------------------------------------------------------------------------------
--countScore function to deduce the number of matches each score has with others
--Inputs: score to match, rest of the scores
--Outputs: value representing the number of instances of the single tuple score
--         in the list of all tuple scores
countScore :: (Int,Int,Int) -> [(Int,Int,Int)] -> Int
countScore _ [] = 0
countScore tuple (x:xs)
    | tuple == x = 1 + countScore tuple xs
    | otherwise = countScore tuple xs
-------------------------------------------------------------------------------
--minScore function to derive the minimum scores of each guess
--creates a list of scores for each guess in allGuess
--Inputs: two lists of scoreList, tuple scores of one guess against allGuess
--Outputs: List of scores matching the the guess
minScore :: [(Int,Int,Int)] -> [(Int,Int,Int)]-> [Int]
minScore [] _ = []
minScore (x:xs) scoreList = score:(minScore xs scoreList)
    --score would be the minimum guesses this one guess can eliminate
    --countScore would produce the largest subset of tuple scores, the
    --tuples which would not be eliminated if that were the correct tuple score
    where score = (length scoreList) - (countScore x scoreList)
-------------------------------------------------------------------------------
--minRemove function to produce the minimum score of one guess in allGuess
--returns the  minimum number of possibilities the guess can remove in allGuess
--Inputs: single guess in allGuess and allGuess
--Outputs: score of a guess in allGuess
minRemove :: [String] -> [[String]] -> Int
minRemove _ [] = 0
minRemove target allGuess = minRemovable
    where --creates score list of tuples for allGuess against target in allGuess
          scoreList = generateMatch allGuess target
          --find the minimum number of removeable of this one guess
          minRemovable = minimum (minScore scoreList scoreList)
-------------------------------------------------------------------------------
--Count guess function to deduce the tuple matches of each guess
--Using the minimax technique
--Takes an input of a list of strings for guess and the score tuple and the
--set of allGuess to produce a minimum number of guesses this guess would
--eliminate in allGuess
countGuess :: Eq a => [[a]] -> [[[a]]] -> (Int,Int,Int) -> Int
countGuess _ [] _ = 0
countGuess guess (x:xs) scores = if tupleMatch scores (scoreMark guess x)
                                    then  1 + (countGuess guess xs scores)
                                    else countGuess guess xs scores
-------------------------------------------------------------------------------
--Score Match Function takes two tuples of scores and determines if
--they are the same
--Inputs: Target score tuple and guess score tuple
--Output: Returns a boolean variable
tupleMatch :: (Int,Int,Int) -> (Int, Int, Int) -> Bool
tupleMatch (x,y,z) (a,b,c) = if (x == a) && (y == b)  && (z == c)
                                then True
                                else False

-------------------------------------------------------------------------------

{-=============================================================================
==========================TUPLE SCORING FUNCTIONS==============================
=============================================================================-}
-------------------------------------------------------------------------------
--scoreMark function using the response function from Proj1Test
--returns the tuple score of a guess against the target
--Inputs: target list of strings and a guess list of strings
--Outputs: tuple of integers for the tuple score
scoreMark :: Eq a => [[a]] -> [[a]] -> (Int,Int,Int)
scoreMark target guess = (right, rightNote, rightOctave)
  where guess'      = nub guess
        right       = length $ intersect guess' target
        num         = length guess'
        rightNote   = num - (length $ deleteFirstsBy (eqNth 0) guess' target)
                    - right
        rightOctave = num - (length $ deleteFirstsBy (eqNth 1) guess' target)
                    - right


-- | eqNth n l1 l2 returns True iff element n of l1 is equal to
--   element n of l2.
eqNth :: Eq a => Int -> [a] -> [a] -> Bool
eqNth n l1 l2 = (l1 !! n) == (l2 !! n)

-------------------------------------------------------------------------------
{-=============================================================================
========================REMOVE WRONG GUESSES FUNCTIONS=========================
=============================================================================-}
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--reduceAllGuess function to reduce the size of allGuess
--removes the guesses which do not give the same response as the previous guess
--Inputs: allGuess, the previous guess, tuple score of guess
--Outputs: reduced list of allGuess
reduceAllGuess :: [[String]] -> [String] -> (Int,Int,Int) -> [[String]]
reduceAllGuess [] _ _ = []
reduceAllGuess allGuess prevGuess score = reducedGuesses
    where --create a score list of tuples for allGuess against prevGuess
          scoreList = generateMatch allGuess prevGuess
          --derive the list of combinations to eliminate from allGuess
          elimList = generateElims allGuess scoreList score
          --eliminate the ones which do not have the same score
          reducedGuesses = allGuess \\ elimList

-------------------------------------------------------------------------------
--generateElims function to take note of the guesses we want to remove
--outputs a list of guesses that do not have the same tuple score
--Inputs: allGuess, scoreList of allGuess onto correct score, correct score
--Outputs: List of guesses which scores do not match with the correct score
generateElims :: [[String]] -> [(Int,Int,Int)] -> (Int,Int,Int) -> [[String]]
generateElims [] _ _ = []
generateElims _ [] _ = []
generateElims (x:xs) (t:ts) target
    | tupleMatch target t = generateElims xs ts target
    | otherwise = x:(generateElims xs ts target)
-------------------------------------------------------------------------------
--generateMatch function to generate the scores of allGuess against prevGuess
--creates a list of score tuples to determine if the ones to take out
--Inputs: allGuess, previous guess treated as correct one
--Outputs: List of tupleScore for all guess
generateMatch :: [[String]] -> [String] -> [(Int,Int,Int)]
generateMatch [] _ = []
generateMatch _ [] = []
generateMatch guesses target = map (target `scoreMark`) guesses
-------------------------------------------------------------------------------
