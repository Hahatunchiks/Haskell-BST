{-# Language ScopedTypeVariables, GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main)   where

import BST (BSTree (Nil, Node), bstInsert, bstErase, bstFind, bstFilter, (==), makeTree, left', right', value', bstBalansed)

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
treeFiltered = bstFilter even simpleBinaryTree

bstLargeTree :: BSTree Int
bstLargeTree = makeTree [1..10000]

bstIsBst :: Ord v => BSTree v -> Bool
bstIsBst Nil = True
bstIsBst (Node v Nil Nil) = True
bstIsBst (Node v Nil r) = v < value' r && bstIsBst r
bstIsBst (Node v l Nil) = v > value' l && bstIsBst l
bstIsBst (Node v l r) = v < value' r && v > value' l && bstIsBst r && bstIsBst l

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
checkFoldOne = testCase "test: check foldr" $ assertEqual [] 42 (sum simpleBinaryTree)


-- check BST property
instance Arbitrary (BSTree Int) where
    arbitrary = sized $ \n ->
        if n Prelude.== 0 
            then return Nil
            else return $ makeTree [1..n]

shrink Nil = []
shrink (Node _ l r) = [l,r]


qcTestInsert :: TestTree
qcTestInsert = QC.testProperty "property based test: insert => you can find it" $ 
  \(n :: Int, t :: BSTree Int)  -> bstFind n (bstInsert n t)


qcTestErase :: TestTree
qcTestErase = QC.testProperty "property based test: erase => you can not find it" $ 
  \(n :: Int)  -> not (bstFind n (bstErase n bstLargeTree)) 


qcTestisBst :: TestTree
qcTestisBst = QC.testProperty "property based test: erase => is balansed bst" $ 
  \(n :: Int)  -> bstIsBst (bstErase n bstLargeTree) && bstBalansed (bstErase n bstLargeTree)

qcTestBinaryAssociativeOperation :: TestTree
qcTestBinaryAssociativeOperation = QC.testProperty "property based test: monoid => u can join trees in balances bst" $
 \(t :: BSTree Int, k :: BSTree Int) -> (((k <> t) <> simpleBinaryTree) BST.== (k <> (t <> simpleBinaryTree))) && bstIsBst (k <> (t <> simpleBinaryTree)) && bstBalansed (k <> (t <> simpleBinaryTree))

qcTestBinaryIsBST :: TestTree
qcTestBinaryIsBST = QC.testProperty "property based test: check balanced binary search tree property" $
 \(t :: BSTree Int, k :: Int)  -> t BST.== Nil ||
    (let 
        Node v l r = bstInsert k t
    in 
        if  | v == k -> bstBalansed (Node v l r)
            | v < k -> bstFind k r && bstBalansed (Node v l r)
            | otherwise -> bstFind k l && bstBalansed (Node v l r))


tests :: TestTree
tests = testGroup "Unit tests: Binary Search Tree" 
        [ 
            testGroup "Test Insertions" [checkInsertsOne, checkInsertsTwo],
            testGroup "Test Erasings" [checkEraseOne, checkEraseTwo],
            testGroup "Test Filtering" [checkFilterOne, checkFilterTwo],
            testGroup "Test Folds" [checkFoldOne],
            testGroup "property based test: Inserting, Erasing" [qcTestInsert, qcTestErase, qcTestisBst],
            testGroup "property based test: Monoid property" [qcTestBinaryAssociativeOperation],
            testGroup "property based test: BST property" [qcTestBinaryIsBST]
        ]

main :: IO()
main = defaultMain tests