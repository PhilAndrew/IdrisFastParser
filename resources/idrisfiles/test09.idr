
data Stream2 : Type -> Type where
  (::) : (e : a) -> Inf (Stream2 a) -> Stream2 a

ones : Stream2 Nat
ones = 1 :: ones

