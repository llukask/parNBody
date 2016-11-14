{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Vector
    ( vec2
    , add
    , sub
    , scale
    , Vec2
    , fromTuple
    , toTuple
    , distanceSquared
    , distance
    ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data Vec2 = Vec2 Double Double deriving (Eq, Show, Generic, NFData)

vec2 :: Double -> Double -> Vec2
vec2 = Vec2

getX :: Vec2 -> Double
getX (Vec2 x _) = x

getY :: Vec2 -> Double
getY (Vec2 _ y) = y

add :: Vec2 -> Vec2 -> Vec2
add (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

sub :: Vec2 -> Vec2 -> Vec2
sub (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)

scale :: Double -> Vec2 -> Vec2
scale s (Vec2 x y) = Vec2 (s * x) (s * y)

fromTuple :: (Double, Double) -> Vec2
fromTuple (x, y) = Vec2 x y

toTuple :: Vec2 -> (Double, Double)
toTuple (Vec2 x y) = (x, y)

distanceSquared :: Vec2 -> Vec2 -> Double
distanceSquared v1 v2 = (x*x) + (y*y)
  where
    (Vec2 x y) = sub v1 v2

distance :: Vec2 -> Vec2 -> Double
distance v1 v2 = sqrt $ distanceSquared v1 v2
