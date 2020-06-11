
interface Show2 a where
    show2 : a -> String

Show2 Nat where
    show2 Z = "Z"
    show2 (S k) = "s" ++ show2 k

||| show (S (S (S Z)))


