module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Meta = M Int Int
  deriving (Show)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                   = 0
tsize (Branch (M a _) _ _ _) = a

tdepth :: Tree a -> Int
tdepth Leaf                   = 0
tdepth (Branch (M _ a) _ _ _) = a

maxTreeHeight :: Tree a -> Tree a -> Int
maxTreeHeight l r = max (tdepth l) (tdepth r)

makeBranch :: Tree a -> a -> Tree a -> Tree a
makeBranch l val r = Branch (M (tsize l + tsize r + 1) (maxTreeHeight l r + 1)) l val r

leftTree :: Tree a -> Tree a
leftTree (Branch _ left _ _) = left
leftTree Leaf                = error "leftTree called on Leaf"

rightTree :: Tree a -> Tree a
rightTree (Branch _ _ _ right) = right
rightTree Leaf                 = error "rightTree called on Leaf"


nodeValue :: Tree a -> a
nodeValue (Branch _ _ v _) = v
nodeValue Leaf             = error "nodeValue called on Leaf"

rightRotate :: Tree a -> Tree a
rightRotate y = makeBranch (leftTree x) (nodeValue x) (makeBranch t (nodeValue y) (rightTree y))
  where
    x = leftTree y
    t = rightTree x


leftRotate :: Tree a -> Tree a
leftRotate x = makeBranch (makeBranch (leftTree x) (nodeValue x) t2) (nodeValue y) (rightTree y)
  where
    y = rightTree x
    t2 = leftTree y


balanceTree :: Ord a => Tree a -> a -> Tree a
balanceTree tree key
  | balance > 1 && key < nodeValue left = rightRotate (makeBranch left val right)
  | balance < -1 && key > nodeValue right = leftRotate (makeBranch left val right)
  | balance > 1 && key > nodeValue left = rightRotate (makeBranch (leftRotate left) val right)
  | balance < -1 && key < nodeValue right = leftRotate (makeBranch left val (rightRotate right))
  | otherwise = makeBranch left val right
  where
    balance = tdepth left - tdepth right
    left = leftTree tree
    right = rightTree tree
    val = nodeValue tree

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ left value right)
  | x == value = True
  | x < value = tmember x left
  | x > value = tmember x right
tmember _ _ = undefined

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = Branch (M 1 1) Leaf x Leaf
tinsert x (Branch _ left value right)
  | x == value = balanceTree (makeBranch left value right) x
  | x < value = balanceTree (makeBranch (tinsert x left) value right) x
  | x > value = balanceTree (makeBranch left value (tinsert x right)) x
  where
    left' = tinsert x left
    right' = tinsert x right
tinsert _ _ = undefined

tFromList :: Ord a => [a] -> Tree a
tFromList = foldl (flip tinsert) Leaf
