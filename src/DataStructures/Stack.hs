module DataStructures.Stack where

data Stack a = Stack !Int [a] deriving (Eq, Show)

fromList :: [a] -> Stack a
fromList xs = Stack (length xs) xs

empty :: Stack a
empty = Stack 0 []

push :: Stack a -> a -> Stack a
push (Stack size' xs) x = Stack (size' + 1) $ x : xs

pop :: Stack a -> (Stack a, Maybe a)
pop (Stack _ []) = (empty, Nothing)
pop (Stack size' xs) = (Stack (size' - 1) (tail xs), Just $ head xs)

isEmpty :: Stack a -> Bool
isEmpty (Stack size' _) = size' == 0

size :: Stack a -> Int
size (Stack l _) = l
