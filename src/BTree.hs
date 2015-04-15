--import System.Directory
import Data.List.PointedList

data BTree a = Leaf a | Branch (PointedList (BTree a)) deriving (Show, Eq)

data Crumb a = Crumb [BTree a ] [BTree a] deriving (Show, Eq)

data FileCursor a = FileCursor [Crumb a] (BTree a) deriving (Show, Eq)


down :: FileCursor a -> Maybe (FileCursor a)
down (FileCursor cs (Branch (PointedList l p r))) = Just $ FileCursor ((Crumb l r):cs) p
down _ = Nothing

up :: FileCursor a -> Maybe (FileCursor a)
up (FileCursor ((Crumb l r):cs) p) = Just $ FileCursor cs (Branch (PointedList l p r))
up _ = Nothing

