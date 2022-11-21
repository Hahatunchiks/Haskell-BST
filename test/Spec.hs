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
 \(t :: BSTree Int, k :: BSTree Int) -> ((k <> t) <> simpleBinaryTree) BST.== (k <> (t <> simpleBinaryTree))

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