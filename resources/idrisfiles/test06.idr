
data Vect2 : Nat -> Type -> Type where
   Nil  : Vect2 Z a
   (::) : a -> Vect2 k a -> Vect2 (S k) a

(++) : Vect2 n a -> Vect2 m a -> Vect2 (n + m) a
(++) Nil       ys = ys
(++) (x :: xs) ys = x :: xs ++ ys

data Fin : Nat -> Type where
   FZ : Fin (S k)
   FS : Fin k -> Fin (S k)

index : Fin n -> Vect2 n a -> a
index FZ     (x :: xs) = x
index (FS k) (x :: xs) = index k xs

index2 : {a:Type} -> {n:Nat} -> Fin n -> Vect2 n a -> a
index2 FZ     (x :: xs) = x
index2 (FS k) (x :: xs) = index2 k xs

