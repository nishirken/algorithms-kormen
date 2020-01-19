{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module DataStructures.BinaryTree where

data BinaryTree a b where
  Node :: (Ord a, Show a, Eq b, Show b) =>
    { _left :: Maybe (BinaryTree a b)
    , _key :: a
    , _value :: b
    , _right :: Maybe (BinaryTree a b)
    } -> BinaryTree a b

deriving instance Eq (BinaryTree a b)
deriving instance Show (BinaryTree a b)

toList :: BinaryTree a b -> [(a, b)]
toList (Node Nothing key value Nothing) = [(key, value)]
toList (Node Nothing key value (Just right)) = [(key, value)] <> (toList right)
toList (Node (Just left) key value Nothing) = (toList left) <> [(key, value)]
toList (Node (Just left) key value (Just right)) = (toList left) <> [(key, value)] <> (toList right)

search :: BinaryTree a b -> a -> b -> Maybe (a, b)
search (Node Nothing key value Nothing) key' value' =
  if key == key' && value == value' then Just (key, value) else Nothing
search (Node Nothing key value (Just right)) key' value' =
  if key == key' && value == value' then Just (key, value) else search right key' value'
search (Node (Just left) key value Nothing) key' value' =
  if key == key' && value == value' then Just (key, value) else search left key' value'
search (Node (Just left) key value (Just right)) key' value' =
  if key == key' && value == value'
  then Just (key, value)
  else (if key' < key then search left key' value' else search right key' value')

size :: BinaryTree a b -> Int
size (Node Nothing _ _ Nothing) = 1
size (Node (Just left) _ _ Nothing) = 1 + size left
size (Node Nothing _ _ (Just right)) = 1 + size right
size (Node (Just left) _ _ (Just right)) = 1 + size left + size right

max' :: BinaryTree a b -> b
max' (Node Nothing key value Nothing) = value
max' (Node (Just left) key value Nothing) = value
max' (Node Nothing key value (Just right)) = max' right
max' (Node (Just left) key value (Just right)) = max' right

min' :: BinaryTree a b -> b
min' (Node Nothing key value Nothing) = value
min' (Node (Just left) key value Nothing) = min' left
min' (Node Nothing key value (Just right)) = value
min' (Node (Just left) key value (Just right)) = min' left

insert :: BinaryTree a b -> a -> b -> BinaryTree a b
insert (Node Nothing key value Nothing) key' value' =
  let newNode = Just (Node Nothing key' value' Nothing) in
    if key' < key then Node newNode key value Nothing else Node Nothing key value newNode
insert (Node (Just left) key value Nothing) key' value' =
  let newNode = Just (Node Nothing key' value' Nothing) in
    if key' < key then Node (Just $ insert left key' value') key value Nothing else Node (Just left) key value newNode
insert (Node Nothing key value (Just right)) key' value' =
  let newNode = Just (Node Nothing key' value' Nothing) in
    if key' < key then Node newNode key value (Just right) else Node Nothing key value (Just $ insert right key' value')
insert (Node (Just left) key value (Just right)) key' value' =
  if key' < key
  then Node (Just $ insert left key' value') key value (Just right)
  else Node (Just left) key value (Just $ insert right key' value')

join :: BinaryTree a b -> BinaryTree a b -> BinaryTree a b
join xs (Node Nothing key value Nothing) = insert xs key value
join xs (Node (Just left) key value Nothing) = join (insert xs key value) left
join xs (Node Nothing key value (Just right)) = join (insert xs key value) right
join xs (Node (Just left) key value (Just right)) = join (join (insert xs key value) left) right

delete :: BinaryTree a b -> a -> Maybe (BinaryTree a b)
delete xs@(Node Nothing key value Nothing) key' =
  if key' == key
  then Nothing
  else Just xs
delete (Node (Just left) key value Nothing) key' = if key' == key then Just left else Just $ Node (delete left key') key value Nothing
delete (Node Nothing key value (Just right)) key' = if key' == key then Just right else Just $ Node Nothing key value (delete right key')
delete (Node (Just left) key value (Just right)) key' =
  if key' == key
  then Just (join left right)
  else (if key' < key
    then Just $ Node (delete left key') key value (Just right)
    else Just $ Node (Just right) key value (delete right key'))

invert :: BinaryTree a b -> BinaryTree a b
invert xs@(Node Nothing _ _ Nothing) = xs
invert (Node (Just left) key value Nothing) = Node Nothing key value (Just $ invert left)
invert (Node Nothing key value (Just right)) = Node (Just $ invert right) key value Nothing
invert (Node (Just left) key value (Just right)) = Node (Just $ invert right) key value (Just $ invert left)

findCommonParent :: a -> a -> BinaryTree a b -> Maybe a
findCommonParent _ _ (Node Nothing key _ Nothing) = Nothing
findCommonParent key1 key2 (Node left key _ right) =
  if key == key1 || key == key2
  then Just key
  else (if l /= Nothing && r /= Nothing then Just key else leftOrRight l r)
    where
      l = findCommonParent key1 key2 <$> left
      r = findCommonParent key1 key2 <$> right
      leftOrRight (Just x) Nothing = x
      leftOrRight Nothing (Just x) = x
