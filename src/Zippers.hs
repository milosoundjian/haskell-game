module Zippers where

-- List Zipper Implementation taken straight from the lectures with the last two methods added.
class ListItem lp where
  headItem :: [a] -> lp a       -- create a pointer to the head of a (non-empty) list
  value :: lp a -> a            -- return the value stored at the pointer
  movL :: lp a -> lp a          -- move the pointer to the left
  movR :: lp a -> lp a          -- move the pointer to the right
  ovw :: a -> lp a -> lp a      -- overwrite item with new value
  
  -- these determine whether or not the pointer is pointing to the first/last element in the underlying list
  isFirst :: lp a -> Bool       
  isLast :: lp a -> Bool    
  

newtype ListZip a = LP ([a],a,[a]) deriving (Show)      -- first element contains the elements to the left in reverse order
                                                        -- second element is the value being pointed to
                                                        -- third element contains the elements to the right of the value

instance ListItem ListZip where
  headItem xs = LP ([],head xs,tail xs)     
  value (LP (ls,x,rs)) = x                  
  
  movL orig@(LP ([], x, rs)) = orig
  movL (LP (l:ls,x,rs)) = LP (ls,l,x:rs)  

  movR orig@(LP (ls, x, [])) = orig      
  movR (LP (ls,x,r:rs)) = LP (x:ls,r,rs)      
  
  ovw y (LP (ls,x,rs)) = LP (ls,y,rs)

  isFirst (LP (ls, x, rs)) = null ls
  isLast (LP (ls, x, rs)) = null rs


-- Zipper definition for binary trees from the project description (slightly adapted to work with labelled binary trees)

data BinTree a = Leaf | B a (BinTree a) (BinTree a)

data BinCxt a = Hole | B0 a (BinCxt a) (BinTree a) | B1 a (BinTree a) (BinCxt a)

type BinZip a = (BinCxt a, BinTree a)

-- Leaf and Hole cases left unimplemented because ideally they won't be encountered
go_left :: BinZip a -> BinZip a
go_left (c,B a t1 t2) = (B0 a c t2,t1)  -- focus on the left child

go_right :: BinZip a -> BinZip a
go_right (c,B a t1 t2) = (B1 a t1 c,t2) -- focus on the right child


go_down :: BinZip a -> BinZip a
go_down (B0 a c t2,t) = (c,B a t t2)    -- focus on parent *from* left child
go_down (B1 a t1 c,t) = (c,B a t1 t)    -- focus on parent *from* right child
