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

higher :: Integer -> Integer
higher i = ceiling $ (fromIntegral i ) / 2

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

decompressTree :: QuadTree a -> [[a]]
decompressTree (QuadTree q w h) = decompress w h q

-- Decompress Quadrant with given width and height
decompress :: Integer -> Integer -> Quadrant a -> [[a]]
decompress _ _ Empty              = [[]]
decompress w h (Leaf a)           = replicate (fromIntegral h) $ replicate (fromIntegral w) a
decompress w h (Node tl tr bl br) = tops ++ bottoms
  where dtl = decompress (w `div` 2) (higher h) tl
        dtr = decompress (higher w) (higher h) tr
        dbl = decompress (w `div` 2) (h `div` 2) bl
        dbr = decompress (higher w) (h `div` 2) br
        tops = zipWith (++) dtl dtr
        bottoms = zipWith (++) dbl dbr

instance Functor Quadrant where
  fmap _ Empty    = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node tl tr bl br) = Node (fmap f tl) (fmap f tr) (fmap f bl) (fmap f br)

instance Functor QuadTree where
  fmap f t@(QuadTree q w h) = QuadTree (fmap f q) w h

instance Foldable QuadTree where
  foldMap f t@(QuadTree q w h) = foldr mappend mempty $ (flatten (decompressTree t))
    where flatten = map flatRows
          flatRows xs = foldr mappend mempty $ map f xs

-- traverseQ :: (Applicative f) => (a -> f b) -> Quadrant a -> f (Quadrant b)
-- traverseQ _ Empty              = pure Empty
-- traverseQ f (Leaf a)           = Leaf <$> f a
-- traverseQ f (Node tl tr bl br) = do
--     ttl <- traverseQ f tl
--     ttr <- traverseQ f tr
--     tbl <- traverseQ f bl
--     tbr <- traverseQ f br
--     pure (Node ttl ttr tbl tbr)

instance Traversable QuadTree where
  traverse = undefined

-- | Make new 'QuadTree' filled with given element
mkQuadTree :: a -> Integer -> Integer -> QuadTree a
mkQuadTree fill width height
  | width > 0 && height > 0 = QuadTree (Leaf fill) width height
  | otherwise = QuadTree Empty 0 0

-- | Check whether 'QuadTree' has given x and y
hasElementOn :: QuadTree a -> Integer -> Integer -> Bool
hasElementOn t x y = x < (_qtWidth t) && y < (_qtHeight t) && x >= 0 && y >= 0

-- | Get element from 'QuadTree' from x and y
getElement :: QuadTree a -> Integer -> Integer -> Maybe a
getElement t x y
  | hasElementOn t x y = row >>= safeHead . drop (fromIntegral x)
  | otherwise = Nothing
      where row = safeHead . drop (fromIntegral y) $ decompressTree t

-- | Convert 'QuadTree' to matrix
toMatrix :: QuadTree a -> [[a]]
toMatrix (QuadTree q w h) = decompress w h q

-- | Convert matrix to 'QuadTree'
--
-- Should return simplified quad tree
-- Law: toMatrix . fromMatrix == id
-- Law: fromMatrix . toMatrix == id
-- TODO: implement
fromMatrix :: Eq a => [[a]] -> QuadTree a
fromMatrix = undefined
