module Vec3 where

import Types

add :: Vec3 -> Vec3 -> Vec3
add (Vec3 a b c) (Vec3 x y z) = Vec3 (a + x) (b + y) (c + z)

mul :: Vec3 -> Float -> Vec3
mul (Vec3 a b c) k = Vec3 (k * a) (k * b) (k * c)

vecMul :: Vec3 -> Vec3 -> Vec3
vecMul (Vec3 a b c) (Vec3 x y z) = Vec3 (a * x) (b * y) (c * z)

neg :: Vec3 -> Vec3
neg (Vec3 a b c) = Vec3 (- a) (- b) (- c)

sub :: Vec3 -> Vec3 -> Vec3
sub a b = a `add` neg b

lenSquared :: Vec3 -> Float
lenSquared v = v `dot` v

length :: Vec3 -> Float
length = sqrt . lenSquared

dot :: Vec3 -> Vec3 -> Float
dot (Vec3 a b c) (Vec3 x y z) = a * x + b * y + c * z

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a b c) (Vec3 x y z) =
  Vec3
    (b * z - c * y)
    (c * x - a * z)
    (a * y - b * x)

normalise :: Vec3 -> Vec3
normalise v = v `mul` (1.0 / Vec3.length v)

colour :: Vec3 -> Colour
colour (Vec3 a b c) = Colour (floor a) (floor b) (floor c)

instance Num Vec3 where
  (+) = add
  (*) = vecMul
  negate = neg
