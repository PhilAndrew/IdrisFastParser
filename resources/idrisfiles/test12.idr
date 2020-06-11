
data DPair : (a : Type) -> (p : a -> Type) -> Type where
   MkDPair : {p : a -> Type} -> (x : a) -> p x -> DPair a p

