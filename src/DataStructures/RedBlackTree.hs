{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module DataStructures.RedBlackTree where

data NodeColor = Red | Black deriving (Eq, Show)

data RedBlackTree a where
  Node :: (Ord a, Show a) =>
    { _left :: Maybe (RedBlackTree a)
    , _key :: a
    , _color :: NodeColor
    , _right :: Maybe (RedBlackTree a)
    } -> RedBlackTree a

deriving instance Eq (RedBlackTree a)
deriving instance Show (RedBlackTree a)

search :: RedBlackTree a -> Maybe a
search (Node Nothing key _ Nothing) key' = if key == key' then Just key else Nothing
search (Node Nothing key _ (Just right)) key' = if key == key' then Just key else search right key'
search (Node (Just left) key _ Nothing) key' = if key == key' then Just (key, value) else search left key'
search (Node (Just left) key _ (Just right)) key' =
  if key == key'
  then Just key
  else (if key' < key then search left key' else search right key')

min :: RedBlackTree a -> a
min (Node Nothing key _ _) = key
min (Node (Just left) key _ _) = min left

max :: RedBlackTree a -> a
max (Node _ key _ Nothing) = key
max (Node _ key _ (Just right)) = max right
