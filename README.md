Министерство науки и высшего образования Российской Федерации федеральное государственное автономное образовательное учреждение высшего образования

«Национальный исследовательский университет ИТМО»

---
__ФПИиКТ, Системное и Прикладное Программное Обеспечение__

__Лабораторная работа №2__

по Функциональному программированию

Выполнил: Лавлинский Михаил Сергеевич

Группа: P34112

Преподаватель: Пенской Александр Владимирович

###### Санкт-Петербург
###### 2022 г.

---

## Требования к разработанному ПО

Реализовать Set (tree based).

1. Функции:
  * добавление и удаление элементов;
  * фильтрация;
  * отображение (map);
  * свертки (левая и правая);
  * структура должна быть моноидом.
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства монойда).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования.

## Ключевые элементы реализации с минимальными комментариями

Нейтральным элементом выбрано пустое дерево.
Соответсвующая бинарная ассоциативная операция - объединение деревьев

### Код структуры

```
module BST (BSTree (Nil, Node), bstInsert, bstErase, bstFind, bstFilter, (==)) where

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

```

## Тесты, отчет инструмента тестирования, метрики

```
{-# Language ScopedTypeVariables, GADTs #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main)   where

import BST (BSTree (Nil, Node), bstInsert, bstErase, bstFind, bstFilter, (==))

import Test.Tasty
--import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU

simpleBinaryTree :: BSTree Int
simpleBinaryTree = Node 10 (Node 5 Nil Nil) (Node 15 (Node 12 Nil Nil) Nil)

treeWithoutNode :: BSTree Int
treeWithoutNode = bstErase 15 simpleBinaryTree

treeWithAddedNode :: BSTree Int
treeWithAddedNode = bstInsert 18 simpleBinaryTree

treeFiltered :: BSTree Int
treeFiltered = bstFilter (\x -> x `mod` 2 Prelude.== 0) simpleBinaryTree

--check insertions 
checkInsertsOne :: TestTree
checkInsertsOne = testCase "test: Insert in non empty tree"  $ assertEqual [] True (bstFind 18 treeWithAddedNode)

checkInsertsTwo :: TestTree
checkInsertsTwo = testCase "test: Check that other elements exists" $ assertEqual [] True (bstFind 5 treeWithAddedNode && bstFind 5 treeWithAddedNode && bstFind 15 treeWithAddedNode)


--check erase
checkEraseOne :: TestTree
checkEraseOne = testCase "test: cannot find element after it's erasing" $ assertEqual [] True (bstFind 15 simpleBinaryTree && not (bstFind 15 treeWithoutNode))

checkEraseTwo :: TestTree
checkEraseTwo = testCase "test: all other elements exists" $ assertEqual [] True (bstFind 10 treeWithoutNode && bstFind 5 treeWithoutNode && bstFind 12 treeWithoutNode)

--check filter bst
checkFilterOne :: TestTree
checkFilterOne = testCase "test: exists even numbers" $ assertEqual [] True (bstFind 10 treeFiltered && bstFind 12 treeFiltered)

checkFilterTwo :: TestTree
checkFilterTwo = testCase "test: odd numbers doesn't exists" $ assertEqual [] True (not (bstFind 5 treeFiltered) && not (bstFind 15 treeFiltered))

--check folds bst
checkFoldOne :: TestTree
checkFoldOne = testCase "test: check foldr" $ assertEqual [] 42 (foldr1 (+) simpleBinaryTree)

checkFoldTwo :: TestTree
checkFoldTwo = testCase "test: check foldr" $ assertEqual [] 42 (foldl1 (+) simpleBinaryTree)


-- check BST property
instance Arbitrary v => Arbitrary (BSTree v) where
    arbitrary = sized $ \n ->
        if n Prelude.== 0
            then return Nil
            else frequency [(1, return Nil), (n, resize (n-1) arbitrary)]



qcTestInsert :: TestTree
qcTestInsert = QC.testProperty "property based test: insert => you can find it" $ 
  \(n :: Int) t -> bstFind n (bstInsert n t)

qcTestBinaryAssociativeOperation :: TestTree
qcTestBinaryAssociativeOperation = QC.testProperty "property based test: monoid => u can join trees" $
 \(t :: BSTree Int, k :: BSTree Int) -> (k <> t <> simpleBinaryTree) BST.== (k <> simpleBinaryTree <> t)

qcTestBinaryIsBST :: TestTree
qcTestBinaryIsBST = QC.testProperty "property based test: check binary search tree property" $
 \(t :: BSTree Int, k :: Int)  -> t BST.== Nil ||
    (let 
        Node v l r = bstInsert k t
    in 
        if  | v == k -> True 
            | v < k -> bstFind k r
            | otherwise -> bstFind k l)


tests :: TestTree
tests = testGroup "Unit tests: Binary Search Tree" 
        [ 
            testGroup "Test Insertions" [checkInsertsOne, checkInsertsTwo],
            testGroup "Test Erasings" [checkEraseOne, checkEraseTwo],
            testGroup "Test Filtering" [checkFilterOne, checkFilterTwo],
            testGroup "Test Folds" [checkFoldOne, checkFoldTwo],
            testGroup "property based test: Inserting" [qcTestInsert],
            testGroup "property based test: Monoid property" [qcTestBinaryAssociativeOperation],
            testGroup "property based test: BST property" [qcTestBinaryIsBST]
        ]

main :: IO()
main = defaultMain tests
```

В отчет инструмента тестирования попало 3 вида property-based тестов по 100 в каждом тестовых случаем, и 8 unit-тестов. Итого 11
```
Unit tests: Binary Search Tree
  Test Insertions
    test: Insert in non empty tree:                         OK
    test: Check that other elements exists:                 OK
  Test Erasings
    test: cannot find element after it's erasing:           OK
    test: all other elements exists:                        OK
  Test Filtering
    test: exists even numbers:                              OK
    test: odd numbers doesn't exists:                       OK
  Test Folds
    test: check foldr:                                      OK
    test: check foldr:                                      OK
  property based test: Inserting
    property based test: insert => you can find it:         OK (0.01s)
      +++ OK, passed 100 tests.
  property based test: Monoid property
    property based test: monoid => u can join trees:        OK (0.01s)
      +++ OK, passed 100 tests.
  property based test: BST property
    property based test: check binary search tree property: OK (0.01s)
      +++ OK, passed 100 tests.

All 11 tests passed (0.01s)
```

## Выводы


## P.S. Инструкция по импорту
