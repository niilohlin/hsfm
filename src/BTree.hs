--import System.Directory
import Data.List.PointedList

data BTree a = Leaf a | Branch (PointedList (BTree a)) deriving (Show, Eq)

instance Functor BTree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch as) = Branch (fmap fmap fmap f as)

data Cursor a = Cursor (Maybe (Cursor a)) (BTree a) deriving (Show, Eq)

down :: Cursor a -> Cursor a
down (c@(Cursor _ (Branch (PointedList _ p _)))) = Cursor (Just c) p
down a = a

up :: Cursor a -> Cursor a
up (Cursor (Just m) _) = m
up c = c


main = undefined
