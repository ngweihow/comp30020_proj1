import Data.List
data GameState  = GameState [[String]] [[String]]
data MinGuess = MinGuess [String] Int
--Takes a list and turns them into combinations of 3 for each guess
--Recursively call itself on smaller subsets of combinations of the lists
combThree :: [a] -> Int -> [[a]]
combThree _ 0 = [[]]
combThree [] _ = []
combThree (x:xs) k = (map (x:) (combThree xs (k-1)) ++ (combThree xs k))
{-------------------------------------------------------------------------------
--Remove function to reduce the size of possible guesses
--Used to reduce the size of allGuess
--Inputs: List to remove from, list to remove
--Outputs: List that is 'cleaned'
remove :: Ord a => [[a]] -> [a] -> [[a]]
remove ls [] = ls
remove [[]] _ = [[]]
remove (x:xs) y | sort x == sort y = xs
                | sort x /= sort y = remove xs y
--Elimination function to get rid of guesses in allGuess which produce the
--the same response as the previous guess
--Inputs: Input set of the list of guesses to remove and set of allGuess
--Outputs: The 'cleaned out' allGuess set
elimGuess :: Eq a => [[[a]]] -> [[[a]]] -> [[[a]]]
elimGuess guesses allGuess = filter (not . (`elem` guesses)) allGuess
-------------------------------------------------------------------------------
--Score Match Function takes two tuples of scores and determines if
--they are the same
--First one is the target score and second one is the guess score
tupleMatch :: (Int,Int,Int) -> (Int, Int, Int) -> Bool
tupleMatch (x,y,z) (a,b,c) = if (x == a) && (y == b)  && (z == c)
                             then True
                             else False
-------------------------------------------------------------------------------
match :: [Int] -> [Int] -> Int
match [] _ = 0
match _ [] = 0
match (x:xs) (y:ys) | x == y = 1 + match xs ys
                    | x /= y = match xs ys
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--Count guess function to deduce the tuple matches of each guess
--Using the minimax technique
--Takes an input of a list of strings for guess and the score tuple and the
--set of allGuess to produce a minimum number of guesses this guess would
--eliminate in allGuess
countGuess :: Eq a => [[a]] -> [[[a]]] -> (Int,Int,Int) -> Int
countGuess _ [[]] _ = 0
countGuess guess (x:xs) scores = if tupleMatch scores (scoreMark guess x)
                                    then  1 + countGuess guess xs scores
                                    else countGuess guess xs scores
-------------------------------------------------------------------------------
{-}--Score function to give a score to each (remaining) guess
--Used to tally up which guesses have similar scores with the previous
--Takes the target and the guess and outputs the tuple scores
--Taken from the Proj1 test file
scoreGuess :: [a] -> [a] -> (Int,Int,Int)
scoreGuess target guess = (right, rightNote, rightOctave)
  where guess'      = nub guess
        right       = length $ intersect guess' target
        num         = length guess'
        rightNote   = num - (length $ deleteFirstsBy (eqNth 0) guess' target)
                    - right
        rightOctave = num - (length $ deleteFirstsBy (eqNth 1) guess' target)
                    - right


-------------------------------------------------------------------------------
scoreCount :: Int -> Int -> [a] -> [a] -> Int
scoreCount 0 _ _ _ = 0
scoreCount n headOrTail target guess =
    if  eqNth headOrTail (target !! n) (guess !! n)
        then 1 + scoreCount (n-1) headOrTail (target !! n) (guess !! n)
        else scoreCount (n-1) headOrTail (target !! n) (guess !! n)


-------------------------------------------------------------------------------
-}

-------------------------------------------------------------------------------
--Elimination function to get rid of guesses in allGuess which produce the
--the same response as the previous guess
--Inputs: Input set of the list of guesses to remove and set of allGuess
--Outputs: The 'cleaned out' allGuess set
elimGuess :: Eq a => [[[a]]] -> [[[a]]] -> [[[a]]]
elimGuess guesses allGuess = filter (not . (`elem` guesses)) allGuess
-------------------------------------------------------------------------------
--minimax function which takes the GameState and derives the next best guess
--the GameState contains the reduced list of allGuess and constant allComb
--Inputs: GameState (allGuess,allComb)
--Outputs: the nextGuess
minimax :: GameState -> [String]
minimax (allGuess, allComb) = nextGuess
    where minList = minRemove (allGuess,allGuess)
          indexOfMax = maybeNot(elemIndex(maximum minList))
          nextGuess = allComb !! indexOfMax
-------------------------------------------------------------------------------
--minListBuild function to build a list of scores for each guess in allComb
--list would be the same length of allComb
--Inputs: allGuess (reduced) and allComb (fixed)
--Ouputs: List of scores pertaining to each guess in allComb
minListBuild :: [[String]] -> [[String]] -> [Int]
minListBuild [] _ = []
minListBuild allGuess (c:cs) = (minRemove c allGuess):(minListBuild allGuess cs)
-------------------------------------------------------------------------------
--maybeNot function which takes the Maybe Int that elemIndex ouputs and turns
--it into an Int
--Inputs: Maybe Int
--Outputs: Int for the index
maybeNot :: (Maybe Int) -> Int
maybeNot Nothing = -1
maybeNot (Just n) = n
-------------------------------------------------------------------------------
--countScore function to deduce the number of matches each score has with others
--Inputs: score to match, rest of the scores
--Outputs: value representing the
countScore :: (Int,Int,Int) -> [(Int,Int,Int)] -> Int
countScore x = length . filter (tupleMatch x)
-------------------------------------------------------------------------------
--minScore function to derive the minimum scores of each guess
--creates a list of scores for each guess in allComb
--Inputs: two lists of scoreList, tuple scores of one guess against allGuess
--Outputs: List of scores matching the the guess
minScore :: [(Int,Int,Int)] -> [(Int,Int,Int)]-> [Int]
minScore [] _ = []
minScore (x:xs) scoreList = score:(minScore xs scoreList)
    where score = (length scoreList) - (countScore x scoreList)
-------------------------------------------------------------------------------
--minRemove function to produce the minimum score of one guess in allComb
--returns the  minimum number of possibilities the guess can remove in allGuess
--Inputs: single guess in allComb and allGuess
--Outputs: score of a guess in allComb
minRemove :: [String] -> [[String]] -> Int
minRemove _ [[]] = 0
minRemove target allGuess = minRemovable
    where --create a score list of tuples for allGuess against target in allComb
          scoreList = generateMatch allGuess target
          minRemovable = minimum (minScore scoreList scoreList)
-------------------------------------------------------------------------------
--reduceAllGuess function to reduce the size of allGuess
--removes the guesses which do not give the same response as the previous guess
--Inputs: allGuess, the previous guess, tuple score of guess
--Outputs: reduced list of allGuess
reduceAllGuess :: Eq a => [[[a]]] -> [[a]] -> (Int,Int,Int) -> [[[a]]]
reduceAllGuess [[[]]] _ _ = [[[]]]
reduceAllGuess allGuess prevGuess score = reducedGuesses
    where --create a score list of tuples for allGuess against prevGuess
          scoreList = generateMatch allGuess prevGuess
          --derive the list of combinations to eliminate from allGuess
          elimList = generateElims allGuess scoreList score
          --eliminate the ones which do not have the same score
          reducedGuesses = elimGuess elimList allGuess

-------------------------------------------------------------------------------
--generateElims function to take note of the guesses we want to remove
--outputs a list of guesses that do not have the same tuple score
--Inputs: allGuess, scoreList of allGuess onto correct score, correct score
--Outputs: List of guesses which scores do not match with the correct score
generateElims :: Eq a => [[[a]]] -> [(Int,Int,Int)] -> (Int,Int,Int) -> [[[a]]]
generateElims [[[]]] _ _ = []
generateElims _ [] _ = []
generateElims (x:xs) (t:ts) target
    | tupleMatch target t = generateElims xs ts target
    | otherwise = x:(generateElims xs ts target)
-------------------------------------------------------------------------------
--generateMatch function to generate the scores of allGuess against prevGuess
--creates a list of score tuples to determine if the ones to take out
--Inputs: allGuess, previous guess treated as correct one
--Outputs: List of tupleScore for all guess
generateMatch :: Eq a => [[[a]]] -> [[a]] -> [(Int,Int,Int)]
generateMatch [[[]]] _ = []
generateMatch _ [[]] = []
generateMatch guesses target = map (target `scoreMark`) guesses
-------------------------------------------------------------------------------
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
{-
scoreMark :: Eq a => [[a]] -> [[a]] -> (Int,Int,Int)
scoreMark target guess =
    do
    let right = length (intersect target guess)
        targetNew = target  \\ guess
        rightNote = check (concat targetNew) (listBuild 0 (concat guess))
        rightOctave = check (concat targetNew) (listBuild 1 (concat guess))
        in (right, rightNote, rightOctave)
-------------------------------------------------------------------------------
--List Building function to keep a 'bank' of either Notes of Octaves of guess
--Used for matching guesses for scores of notes and octaves
--Inputs: index to skip by 0 for Notes/ 1 for Octaves, length of guess set and
--guess set as a single string
--Outputs: Single String of all Notes or Octaves in that guess set in a string
listBuild :: Int -> String -> String
listBuild _ [] = []
listBuild n (l:ls)
    | (n == 0) && (l `elem` ['A'..'G']) = l:(listBuild n ls)
    | (n == 1) && (l `elem` ['1'..'G']) = l:(listBuild n ls)
    | otherwise = listBuild n ls

-------------------------------------------------------------------------------
--Check function to cycle a guess set through a target set to count occurances
--Used for scoring
--Inputs: [right] Lists of target and guess, [Note/Octave] Strings of target
--and guess
--Outputs: Count of matches
check :: Eq a => [a] -> [a] -> Int
check _ [] = 0
check target (y:ys) = if elem y target
                            then 1 + check target (removeString y ys)
                            else check target ys
-------------------------------------------------------------------------------
--removeString string function to remove out extra pitches/notes/octaves
--which might clash with the next guesses for the other notes/octaves
--Inputs : String/Char to remove along with the original String/List
--Outputs: Reduced String/List
removeString :: Eq a => a -> [a] -> [a]
removeString _ [] = []
removeString clash (x:xs)
    | clash == x = xs
    | otherwise  = x:(removeString clash xs)
-------------------------------------------------------------------------------
--Check two lists for similar elements and removes the similarities
--Inputs: Target list and single guess list
--Outputs: The 'cleaned out' target list
checkAndRemove :: Eq a => [[a]] -> [[a]] -> [[a]]
checkAndRemove target guess = filter (not . (`elem` guess)) target
-}
-------------------------------------------------------------------------------
-}
