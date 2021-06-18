module BinaryTree where

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert':: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a  = Node (insert' b left) a right
    | b > a  = Node left a (insert' b right)

-- map over a BinaryTree

mapTree :: (a->b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)


testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
    Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
    if mapTree (+1) testTree' == mapExpected
        then print "okaaaay"
        else error "test failed"


-- convert BinaryTrees to Lists

-- Depth First Traversals:
-- (a) Inorder (Left, Root, Right)
-- (b) Preorder (Root, Left, Right)
-- (c) Postorder (Left, Right, Root)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
    [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
    (preorder left) ++ [a] ++ (preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =
    (preorder left) ++ (preorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO()
testPreorder =
    if preorder testTree == [2,1,3]
        then putStrLn "Preorder fine!!!"
        else putStrLn "Bad news bears."

testInorder :: IO()
testInorder =
    if inorder testTree == [1,2,3]
        then putStrLn "Inorder Fine"
        else putStrLn "Bad news Bears."

testPostorder :: IO()
testPostorder =
    if postorder testTree == [1,3,2]
        then putStrLn "Postorder Fine"
        else putStrLn "Bad news Bears."

main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc t = foldr f acc (inorder t)  

-- *Newss> foldTree (+) 0 testTree 
-- 6

-- Using the foldTree you just wrote, rewrite mapTree using foldTree.
-- The absence of an Ord constraint is intentional, you don’t need to use
-- the insert function.
mapTree' :: (a -> b)
    -> BinaryTree a
    -> BinaryTree b
mapTree' f bt =
    foldTree undefined undefined undefined