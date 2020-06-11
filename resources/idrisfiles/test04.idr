
list_lookup : Nat -> List a -> Maybe a
list_lookup _     Nil         = Nothing
list_lookup Z     (x :: xs) = Just x
list_lookup (S k) (x :: xs) = list_lookup k xs

