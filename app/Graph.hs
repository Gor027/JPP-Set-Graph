module Graph where

import Set (Set)
import qualified Set as Set

class Graph g where
  empty :: g a
  vertex :: a -> g a
  union :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation {domain :: Set a, relation :: Set (a, a)}
  deriving (Eq, Show)

data Basic a
  = Empty
  | Vertex a
  | Union (Basic a) (Basic a)
  | Connect (Basic a) (Basic a)

instance Graph Relation where
  empty = Relation {domain = Set.Empty, relation = Set.Empty}
  vertex a = Relation {domain = Set.Singleton a, relation = Set.Empty}
  union g1 g2 =
    Relation
      { domain = Set.Union (domain g1) (domain g2),
        relation = Set.Union (relation g1) (relation g2)
      }
  connect g1 g2 =
    Relation
      { domain = Set.Union (domain g1) (domain g2),
        relation = Set.Union (Set.Union (relation g1) (relation g2)) (vertexProd (domain g1) (domain g2))
      }
    where
      vertexProd s1 s2 = Set.fromList $ [(v1, v2) | v1 <- Set.toList s1, v2 <- Set.toList s2]

instance (Ord a, Num a) => Num (Relation a) where
  fromInteger = vertex . fromInteger
  (+) = union
  (*) = connect
  signum = const empty
  abs = id
  negate = id

instance Graph Basic where
  empty = Empty
  vertex = Vertex
  union = Union
  connect = Connect

instance Ord a => Eq (Basic a) where
  g1 == g2 =
    Set.toAscList (domain relationalG1) == Set.toAscList (domain relationalG2)
      && Set.toAscList (relation relationalG1) == Set.toAscList (relation relationalG2)
    where
      relationalG1 = fromBasic g1
      relationalG2 = fromBasic g2

instance (Ord a, Num a) => Num (Basic a) where
  fromInteger = vertex . fromInteger
  (+) = union
  (*) = connect
  signum = const empty
  abs = id
  negate = id

instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
  mempty = Empty

fromBasic :: Graph g => Basic a -> g a
fromBasic Empty = empty
fromBasic (Vertex a) = vertex a
fromBasic (Union left right) = fromBasic left `union` fromBasic right
fromBasic (Connect left right) = fromBasic left `connect` fromBasic right

instance (Ord a, Show a) => Show (Basic a) where
  show g =
    "edges " ++ show sortedRelations
      ++ " + vertices "
      ++ show isolatedVertices
    where
      relationalG = fromBasic g
      sortedRelations = Set.toAscList (relation relationalG)
      isolatedVertices = [v | v <- Set.toAscList (domain relationalG), notElem v (map fst sortedRelations) == True, notElem v (map snd sortedRelations) == True]

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]
example34 :: Basic Int
example34 = 1 * 2 + 2 * (3 + 4) + (3 + 4) * 5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot = undefined

instance Functor Basic where
  fmap _ Empty = Empty
  fmap f (Vertex a) = Vertex (f a)
  fmap f (Union left right) = Union (fmap f left) (fmap f right)
  fmap f (Connect left right) = Connect (fmap f left) (fmap f right)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]
mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV = undefined

instance Applicative Basic where
  pure = Vertex
  Empty <*> _ = Empty
  Vertex f <*> g1 = fmap f g1
  Union left right <*> Vertex a = Union (fmap ($ a) left) (fmap ($ a) right)
  Union left1 right1 <*> Union left2 right2 = Union (left1 <*> left2) (right1 <*> right2)
  Connect left right <*> Vertex a = Connect (fmap ($ a) left) (fmap ($ a) right)
  Connect left1 right1 <*> Connect left2 right2 = Connect (left1 <*> left2) (right1 <*> right2)

instance Monad Basic where
  return = Vertex
  Empty >>= _ = Empty
  Vertex a >>= k = k a
  Union left right >>= f = Union (left >>= f) (right >>= f)
  Connect left right >>= f = Connect (left >>= f) (right >>= f)

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]
splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV = undefined
