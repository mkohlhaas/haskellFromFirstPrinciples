module Bintree where

data BinaryTree a = Leaf
                  | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' a Leaf = Node a Leaf Leaf
insert' b tree@(Node a left right)
  | b == a = tree
  | b < a = Node a (insert' b left) right
  | b > a = Node a left (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node a left right) = Node (f a) (mapTree f left) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node a left right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node a left right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node a left right) = postorder right ++ [a] ++ postorder left

testTree :: BinaryTree Integer
testTree = Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

-- any traversal order is fine
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b = foldr f b . preorder
