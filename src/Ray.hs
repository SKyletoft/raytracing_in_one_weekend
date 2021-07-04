module Ray where

import Types
import Vec3

at :: Ray -> Float -> Point3
at (Ray orig dir) t = orig + dir `Vec3.mul` t

colour :: Ray -> Colour
colour (Ray orig@(Point3 x y z) dir) = (1.0 - t) `Vec3.mul` one_cubed + t `Vec3.mul` magic_value
  where
    t = 0.5 * 