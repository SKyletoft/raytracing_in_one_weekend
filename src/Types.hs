module Types where

data Colour
  = Colour Int Int Int

-- | Ray origin direction
data Ray = Ray Point3 Vec3

data Vec3 = Vec3 Float Float Float

type Point3 = Vec3