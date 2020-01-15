module DataStructures.Queue where

data Queue a = Queue !Int [a] deriving (Eq, Show)

fromList :: [a] -> Queue a
fromList xs = Queue (length xs) xs

enqueue :: Queue a -> a -> Queue a
enqueue (Queue size' xs) x = Queue (size' + 1) (reverse $ x : reverse xs)

dequeue :: Queue a -> Maybe (Queue a, a)
dequeue (Queue _ []) = Nothing
dequeue (Queue size' xs) = Just (Queue (size' - 1) (tail xs), head xs)

empty :: Queue a
empty = Queue 0 []

isEmpty :: Queue a -> Bool
isEmpty (Queue size' _) = size' == 0

size :: Queue a -> Int
size (Queue l _) = l
