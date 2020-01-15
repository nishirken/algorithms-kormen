module DataStructures.LinkedList where

import Control.Exception.Base (ArrayException (IndexOutOfBounds), throw)
-- TODO add wrapper for the head of the list, so list can be empty 
data LinkedList a = LinkedItem
  { _next :: Maybe (LinkedList a)
  , _key :: Int
  , _value :: a
  } deriving (Eq, Show)

search :: LinkedList a -> Int -> Maybe a
search (LinkedItem Nothing key' value') key = if key' == key then Just value' else Nothing
search (LinkedItem (Just item') key' value') key = if key' == key then Just value' else search item' key

first :: LinkedList a -> a
first (LinkedItem _ _ v) = v

extractFirst :: LinkedList a -> LinkedList a
extractFirst (LinkedItem _ k v) = LinkedItem Nothing k v

rest :: LinkedList a -> Maybe (LinkedList a)
rest (LinkedItem next _ _) = next

insert :: LinkedList a -> Int -> a -> LinkedList a
insert x@(LinkedItem _ key _) key' value' = LinkedItem (Just x) key' value'

join :: LinkedList a -> LinkedList a -> LinkedList a
join (LinkedItem Nothing key value) ys = LinkedItem (Just ys) key value
join (LinkedItem (Just xs) key value) ys = LinkedItem (Just $ join xs ys) key value

delete :: LinkedList a -> Int -> LinkedList a
delete xs@(LinkedItem Nothing key' _) key =
  if key' == key then throw (IndexOutOfBounds "Can't delete last item in list") else xs
delete xs@(LinkedItem (Just ys) key' value') key = if key == key' then ys else f (LinkedItem Nothing key' value') (Just ys)
  where
    f passedAcc remained = case remained of
      (Just zs) -> let (LinkedItem zs' key' _) = zs in
        if key == key' then joinMaybe passedAcc zs' else f (join passedAcc $ extractFirst zs) (rest zs)
      Nothing -> delete xs key
    joinMaybe :: LinkedList a -> Maybe (LinkedList a) -> LinkedList a
    joinMaybe xs ys = case ys of
      (Just ys') -> join xs ys'
      Nothing -> xs
