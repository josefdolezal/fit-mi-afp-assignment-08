{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.QuadTree where

import Control.Lens hiding (Empty)

-- | Quadtree data structure
--
--  - Top left corner is x=0 y=0
--  - Top right quadrant can be bigger and bottom left smaller
--
-- See <https://en.wikipedia.org/wiki/Quadtree wikipedia>
data QuadTree a = QuadTree
                { _qtTree   :: Quadrant a  -- ^ Quad tree data
                , _qtWidth  :: Integer     -- ^ Width of the encoded rectange (or square)
                , _qtHeight :: Integer     -- ^ Width of the encoded rectange (or square)
                }
                deriving (Eq, Show, Read)

-- | Quadtree's quadrant
data Quadrant a = Empty                      -- ^ Empty quadrant
                | Leaf a                     -- ^ Quadrant filled with one element
                | Node { _qaTL :: Quadrant a -- ^ Top left quadrant
                       , _qaTR :: Quadrant a -- ^ Top right quadrant
                       , _qaBL :: Quadrant a -- ^ Bottom left quadrant
                       , _qaBR :: Quadrant a -- ^ Bottom right quadrant
                       }
                deriving (Eq, Show, Read)

makeLenses ''QuadTree
makeLenses ''Quadrant

instance Functor QuadTree where
  fmap = undefined

instance Foldable QuadTree where
  -- TODO: implement foldr or foldMap
  foldMap = undefined

instance Traversable QuadTree where
  -- TODO: implement traverse or sequenceA
  traverse = undefined

-- | Make new 'QuadTree' filled with given element
--
-- TODO: implement
mkQuadTree :: a -> Integer -> Integer -> QuadTree a
mkQuadTree fill width height = undefined

-- | Check whether 'QuadTree' has given x and y
--
-- TODO: implement
hasElementOn :: QuadTree a -> Integer -> Integer -> Bool
hasElementOn t x y = undefined

-- | Get element from 'QuadTree' from x and y
--
-- TODO: implement
getElement :: QuadTree a -> Integer -> Integer -> Maybe a
getElement t x y = undefined

-- | Convert 'QuadTree' to matrix
--
-- TODO: implement
toMatrix :: QuadTree a -> [[a]]
toMatrix = undefined

-- | Convert matrix to 'QuadTree'
--
-- Should return simplified quad tree
-- Law: toMatrix . fromMatrix == id
-- Law: fromMatrix . toMatrix == id
-- TODO: implement
fromMatrix :: Eq a => [[a]] -> QuadTree a
fromMatrix = undefined
