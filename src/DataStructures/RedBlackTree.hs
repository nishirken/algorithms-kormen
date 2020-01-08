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
search = undefined
