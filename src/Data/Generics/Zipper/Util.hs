module Data.Generics.Zipper.Util
    (
      getHole
    , up
    , down
    , left
    , right
    ) where

import           Data.Generics.Zipper (Zipper)
import qualified Data.Generics.Zipper as Z (getHole, up, down, left, right)
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable)

getHole :: (Typeable b) => Zipper a -> b
getHole = fromMaybe (error "getHole") . Z.getHole

up :: Zipper a -> Zipper a
up = fromMaybe (error "up") . Z.up

down :: Zipper a -> Zipper a
down = fromMaybe (error "down") . Z.down

left :: Zipper a -> Zipper a
left = fromMaybe (error "left") . Z.left

right :: Zipper a -> Zipper a
right = fromMaybe (error "right") . Z.right
