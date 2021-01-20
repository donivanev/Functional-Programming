main :: IO()
main = do
    print ""
    print (size t)
    print (height t)
    print (sumTree t)
    print (sumLeaves t)
    print (inorder t)
    print (inorder t2)
    print (average t)
    print (getLevel 1 t)
    print (mirrorTree t)
    print (mapTree powerOf3 t)
    print (charTree1)
    print (containsWord charTree1 "abc")
    print (genWordsStartingFrom charTree1)
    print (traverseBF charTree1)

type Point = (Double, Double)

--getClosestPoint :: [Point] -> Point -> Point
--getClosestPoint [] center  = error "Empty list" 
--getClosestPoint [p] center = p
--getClosestPoint (p:ps) center = helper ps p
--   where
--        helper [] currClosest = currClosest
--        helper (p:ps) currClosest = if distance p center < distance currClosest center then helper ps p else helper ps currClosest

--getClosestPoint :: [Point] -> Point -> Point
--getClosestPoint [] c  = error "Empty list" 
--getClosestPoint ps c = foldl (\ p q -> if distance p c < distance q c then p else q) ps

data BTree = Empty | Node Int BTree BTree deriving Show

t :: BTree 
t = Node 4 (Node 0 Empty Empty) 
        (Node 2 (Node 1 Empty Empty) 
            Empty) 

t2 :: BTree                                                            
t2 = Node 5 (Node 3 Empty Empty)                      
                     (Node 4 (Node 5 Empty Empty)      
                                   (Node 7 Empty Empty))

size :: BTree -> Int
size Empty = 0
size (Node n left right) = 1 + size left + size right

height :: BTree -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

sumTree :: BTree -> Int
sumTree Empty = 0
sumTree (Node n left right) = n + sumTree left + sumTree right 

sumLeaves :: BTree -> Int
sumLeaves Empty = 0
sumLeaves (Node n Empty Empty) = n
sumLeaves (Node _ left right)  = sumLeaves left + sumLeaves right

inorder :: BTree -> [Int]
inorder Empty = []
inorder (Node n left right) = (inorder left) ++ [n] ++ (inorder right)

getLevel :: Int -> BTree -> [Int]
getLevel _ Empty = []
getLevel 0 (Node n _ _) = [n]
getLevel k (Node _ left right) = getLevel (k - 1) left ++ getLevel (k - 1) right

average :: BTree -> Double
average Empty = error "Empty tree"
average bt = fromIntegral (sumTree bt) / fromIntegral (size bt) 

mirrorTree :: BTree -> BTree
mirrorTree Empty = Empty
mirrorTree (Node n left right) = Node n (mirrorTree right) (mirrorTree left) 

--getLevelsTree :: BTree a -> BTree (a, Int)
--getLevelsTree bt = helper bt 0
--   where 
--        helper Empty               _   = Empty
--        helper (Node n left right) level = Node (n, level) (helper left (level + 1)) (helper right (level + 1))

powerOf3 :: Int -> Int
powerOf3 x = x * 3

mapTree :: (Int -> Int) -> BTree -> BTree 
mapTree _ Empty = Empty
mapTree f (Node n left right) = Node (f n) (mapTree f left) (mapTree f right) 

{-maxDepthBlueNode :: BTree Color -> Int
maxDepthBlueNode tree  = helper tree 1
    where 
        helper Empty                  _         = 0
        helper (Node Blue left right) currDepth = max currDepth (max (helper left (currDepth + 1)) (helper right (currDepth + 1)))
        helper (Node _    left right) currDepth = max (helper left (currDepth + 1)) (helper right (currDepth + 1))

maxDepthNode :: BTree Color -> Color -> Int
maxDepthNode tree color = helper tree 1
    where 
        helper Empty _ = 0
        helper (Node c left right) currDepth 
            | c == color = max currDepth (max (helper left (currDepth + 1)) (helper right (currDepth + 1)))
            | otherwise  = max (helper left (currDepth + 1)) (helper right (currDepth + 1)) -}

--areMirrored :: BTree Int -> BTree Int -> Bool
--areMirrored Empty Empty                     = True
--areMirrored (Node x1 l1 r1) (Node x2 l2 r2) = x1 == x2 && areMirrored l1 r2 && areMirrored r1 l2

--isSymmetric :: BTree Int -> Bool
--isSymmetric Empty = True
--isSymmetric (Node n left right) = areMirrored left right

--allContain :: [BTree Char] -> [String]

data BTreeCh = EmptyCh | NodeCh Char BTreeCh BTreeCh deriving Show

                                                       --     a
charTree1 = NodeCh 'a' (NodeCh 'c' (NodeCh 'f' EmptyCh EmptyCh)  --    / \
                               (NodeCh 'd' EmptyCh EmptyCh)) --   c   b
                     (NodeCh 'b' EmptyCh                   --  / \   \
                               (NodeCh 'e' EmptyCh EmptyCh)) -- f   d   e

--data BTree a = Empty | Leaf a | Node a (BTree a) (BTree a) deriving Show
--template binary treeS

rootWord :: BTree Char -> String -> Bool
rootWord Empty                _      = False
rootWord (Node x Empty Empty) [c]    = x == c
rootWord (Node x left right)  (c:cs) = x == c && (rootWord left cs || rootWord right cs)
rootWord _                    []     = False

containsWord :: BTree Char -> String -> Bool
containsWord Empty                    _    = False
containsWord tree@(Node _ left right) word = rootWord tree word || containsWord left word || containsWord right word 

genWordsStartingFrom :: BTreeCh -> [String]
genWordsStartingFrom EmptyCh                = []
genWordsStartingFrom (NodeCh c EmptyCh EmptyCh) = [[c]]
genWordsStartingFrom (NodeCh c left right)  = [c : cs | cs <- genWordsStartingFrom left ++ genWordsStartingFrom right]

genWords :: BTreeCh -> [String]
genWords EmptyCh             = []
genWords tree@(NodeCh _ left right) = genWordsStartingFrom tree ++ genWords left ++ genWords right

--DFS
traverseDF :: BTreeCh -> [String]
traverseDF EmptyCh        = []
traverseDF (NodeCh n left right) = [n] : (traverseDF left) ++ (traverseDF right)

--BFS
traverseBF :: BTreeCh -> [Char]
traverseBF tree = tbf [tree]
    where
        tbf []                               = []
        tbf xs                               = map n xs ++ tbf (concat (map treeNodes xs))
        n (NodeCh a _ _)                     = a
        treeNodes (NodeCh _ EmptyCh EmptyCh) = []
        treeNodes (NodeCh _ EmptyCh b)       = [b]
        treeNodes (NodeCh _ a EmptyCh)       = [a]
        treeNodes (NodeCh _ a b)             = [a, b]