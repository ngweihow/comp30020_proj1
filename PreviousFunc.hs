-------------------------------------------------------------------------------
--Delete function to remove an element from list
delete :: Eq String => String -> [String] -> [String]
delete t [] = []
delete t (y:ys) | t == y = ys
                | t /= y = y:delete t ys
-------------------------------------------------------------------------------
--cut down the number of remaining guesses
--takes the list of wrong strings weeds it out of the list of possibilities
weedOut :: [String] -> [String] -> [String]
weedOut [] ys = ys
weedOut (x:xs) ys = if elem x ys
                    then delete x ys
                    else weedOut xs ys
-------------------------------------------------------------------------------
--Notes and Octaves are all correct (0,3,3) except pitches are all wrong
--Function to swap around all the available notes and octaves
swap3 :: Eq String => [String] -> [String] -> [String]
swap3 [] gs = []
swap3 (l:ls) gs =


-------------------------------------------------------------------------------
--Lookup function, check if the how many Octaves each note has remaining
--takes a wrongly guessed pitch and lookup remaining octaves of the same notes
--takes a list of remaining
searchUp :: String -> [[Int]] -> CellContent
searchUp s (x:xs) | filter
where A = 0
      B = 1
      C = 2
      D = 3
      E = 4
      F = 5
      G = 6

-------------------------------------------------------------------------------
--Match function to check each pitch with another
matchPitch :: String -> Int -> Bool
matchPitch s i
