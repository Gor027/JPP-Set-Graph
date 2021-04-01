module Graph where

import qualified Data.List as L
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
  empty = Relation {domain = Set.empty, relation = Set.empty}
  vertex a = Relation {domain = Set.singleton a, relation = Set.empty}
  union g1 g2 =
    Relation
      { domain = Set.union (domain g1) (domain g2),
        relation = Set.union (relation g1) (relation g2)
      }
  connect g1 g2 =
    Relation
      { domain = Set.union (domain g1) (domain g2),
        relation = Set.union (Set.union (relation g1) (relation g2)) (vertexProd (domain g1) (domain g2))
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
  g1 == g2 = toRelation g1 == toRelation g2
    where
      toRelation :: Basic a -> Relation a
      toRelation = fromBasic

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

sortedRelations :: (Ord a) => Basic a -> [(a, a)]
sortedRelations g = Set.toAscList (relation $ fromBasic g)

isolatedVertices :: (Ord a) => Basic a -> [a]
isolatedVertices g = (domainList L.\\ fst pairOfLists) L.\\ snd pairOfLists
  where
    pairOfLists = foldr (\(a, b) (as, bs) -> (a : as, b : bs)) ([], []) $ sortedRelations g
    domainList = Set.toAscList (domain $ fromBasic g)

instance (Ord a, Show a) => Show (Basic a) where
  show g =
    "edges " ++ show (sortedRelations g)
      ++ " + vertices "
      ++ show (isolatedVertices g)

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]
example34 :: Basic Int
example34 = 1 * 2 + 2 * (3 + 4) + (3 + 4) * 5 + 17

appendEdges :: (Show a) => [(a, a)] -> String
appendEdges [] = ""
appendEdges (x : xs) = show (fst x) ++ " -> " ++ show (snd x) ++ ";\n" ++ appendEdges xs

appendVertices :: (Show a) => [a] -> String
appendVertices [] = ""
appendVertices (x : xs) = show x ++ ";\n" ++ appendVertices xs

todot :: (Ord a, Show a) => Basic a -> String
todot g =
  "digraph {\n" ++ appendEdges (sortedRelations g)
    ++ appendVertices (isolatedVertices g)
    ++ "}\n"

instance Functor Basic where
  fmap _ Empty = Empty
  fmap f (Vertex a) = Vertex (f a)
  fmap f (Union left right) = Union (fmap f left) (fmap f right)
  fmap f (Connect left right) = Connect (fmap f left) (fmap f right)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]
mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV v1 v2 newV g = mergePartial v2 newV (mergePartial v1 newV g)

mergePartial :: Eq a => a -> a -> Basic a -> Basic a
mergePartial v = mergeVertices (== v)

mergeVertices :: (a -> Bool) -> a -> Basic a -> Basic a
mergeVertices p v = fmap (\u -> if p u then v else u)

instance Applicative Basic where
  pure = Vertex
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  Vertex f <*> g1 = fmap f g1
  Union fLeft fRight <*> g1 = Union (fLeft <*> g1) (fRight <*> g1)
  Connect fLeft fRight <*> g1 = Connect (fLeft <*> g1) (fRight <*> g1)

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
splitV oldV v1 v2 g = g >>= (\u -> if u == oldV then Union (Vertex v1) (Vertex v2) else Vertex u)
