
mutual
  data Blue : Type -> Type where
   B : a -> Inf (Red a) -> Blue a

  data Red : Type -> Type where
   R : a -> Inf (Blue a) -> Red a

mutual
  blue : Blue Nat
  blue = B 1 red

  red : Red Nat
  red = R 1 blue

mutual
  findB : (a -> Bool) -> Blue a -> a
  findB f (B x r) = if f x then x else findR f r

  findR : (a -> Bool) -> Red a -> a
  findR f (R x b) = if f x then x else findB f b

main : IO ()
main = do printLn $ findB (== 1) blue

