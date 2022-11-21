module BST (BSTree (Nil, Node), bstInsert, bstErase, bstFind, bstFilter, (==), makeTree) where

import Data.Array

-- BST structure declaration
data BSTree v =  
    Node v (BSTree v) (BSTree v)  
    | Nil
    deriving (Show)

-- convert a Binary Search Tree into a sorted list
bstToList :: Ord v => BSTree v -> [v]
bstToList Nil = []
bstToList (Node v left right) = bstToList left ++ v : bstToList right

-- merge Binary Search Trees into a list
mergeBSTToList :: Ord v => BSTree v -> BSTree v -> [v]
mergeBSTToList tree1 tree2 = merge (bstToList tree1) (bstToList tree2)
    where 
        merge n [] = n
        merge [] n = n
        merge (x:xs) (y:ys) 
            | x < y = x : merge xs (y:ys)
            | otherwise = y : merge (x:xs) ys

-- convert sorted list to BST 
makeTree :: [v] -> BSTree v
makeTree l = makeTree' (listArray (0, length l - 1) l) 0 (length l)
    where
        makeTree' v low high 
            | low Prelude.== high = Nil
            | otherwise = Node (v!split) (makeTree' v low split) (makeTree' v (split+1) high)
            where
                split = low + (high - low) `div` 2


-- binary associative operation - join trees
instance Ord v => Semigroup (BSTree v) where
    Node k1 l1 r1 <> Node k2 l2 r2 = makeTree $ Node k1 l1 r1 `mergeBSTToList` Node k2 l2 r2 
    Nil <> Node k2 l2 r2 = makeTree $ Nil `mergeBSTToList` Node k2 l2 r2 
    Node k1 l1 r1 <> Nil = makeTree $  Node k1 l1 r1 `mergeBSTToList` Nil
    Nil <> Nil = makeTree $  Nil `mergeBSTToList` Nil

-- neutral element in monoid - empty tree (Nil)
instance Ord v => Monoid (BSTree v) where
    mempty = Nil
    mappend = (<>)

-- provide Functor type functionality (use fmap)
instance Functor BSTree where
    fmap _ Nil = Nil
    fmap func (Node key left right) = Node (func key) (fmap func left) (fmap func right) 

-- provides folds, max/min, size
instance Foldable BSTree where
    foldr _ z Nil = z
    foldr f z (Node v left right) = foldr f (v `f` foldr f z right) left

instance Eq v => Eq (BSTree v) where
 Nil == Nil = True
 Node{}  == Nil = False
 Nil == Node{} = False
 (Node v1 left1 right1) == (Node v2 left2 right2) = (==) v1 v2 && (==) left1 left2 && (==) right1 right2 


-- bst opeartions  find, insert, erase, filter

bstFind :: Ord v => v -> BSTree v -> Bool
bstFind _ Nil = False
bstFind key (Node v left right) = 
    case compare key v of
        EQ -> True
        LT -> bstFind key left
        GT -> bstFind key right


bstInsert :: Ord v => v -> BSTree v -> BSTree v
bstInsert key Nil = Node key Nil Nil
bstInsert key n@(Node v left right) = 
    case compare key v of
        EQ -> n
        LT -> Node v (bstInsert key left) right
        GT -> Node v left (bstInsert key right) 


bstErase :: Ord v => v -> BSTree v -> BSTree v
bstErase _ Nil = Nil
bstErase key (Node v left right) = 
    case compare key v of
        EQ -> deleteNode (Node v left right)
        LT -> Node v (bstErase key left) right
        GT -> Node v left (bstErase key right)

deleteNode :: (Ord v) => BSTree v -> BSTree v 
deleteNode (Node _ Nil t2) = t2
deleteNode (Node _ t1 Nil) = t1
deleteNode Nil = Nil
deleteNode (Node  _ t1 t2) = Node  v2 t1 t2 
    where 
          v2 = minimum t2

-- function for filtering BST
bstFilter :: Ord v => (v -> Bool) -> BSTree v -> BSTree v
bstFilter _ Nil = Nil
bstFilter f tree@Node{} = makeTree $ filter f $ bstToList tree
