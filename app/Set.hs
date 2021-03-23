module Set
  ( Set (..),
    empty,
    null,
    singleton,
    union,
    fromList,
    member,
    toList,
    toAscList,
    elems,
  )
where

import qualified Data.List as L
import Prelude hiding (null)

data Set a
  = Empty
  | Singleton a
  | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _ = False

member :: Eq a => a -> Set a -> Bool
member x xs = x `elem` toList xs

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList = foldr insert Empty

toList :: Set a -> [a]
toList Empty = []
toList (Singleton x) = [x]
toList (Union left right) = toList left ++ toList right

toAscList :: Ord a => Set a -> [a]
toAscList = map L.head . L.group . L.sort . toList

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union Empty x = x
union x Empty = x
union t1 t2 = Union t1 t2

insert :: a -> Set a -> Set a
insert x Empty = Singleton x
insert x right = Union (Singleton x) right

instance Ord a => Eq (Set a) where
  t1 == t2 = toAscList t1 == toAscList t2

instance Semigroup (Set a) where
  xs <> ys = xs `union` ys

instance Monoid (Set a) where
  mempty = Empty
  mappend = (<>)

instance Show a => Show (Set a) where
  show Empty = "Empty"
  show (Singleton a) = show a
  show (Union left right) = show left ++ " " ++ show right

instance Functor Set where
  fmap f = fromList . map f . toList
