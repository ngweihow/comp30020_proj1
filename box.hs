import Data.List
getIndex :: Eq a => a -> [a] -> Int
getIndex i list = maybeNot(elemIndex i list)

maybeNot :: (Maybe Int) -> Int
maybeNot Nothing = -1
maybeNot (Just n) = n
