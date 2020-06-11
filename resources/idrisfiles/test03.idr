
even : Nat -> Bool
even Z = True
even (S k) = odd k where
  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k

test : List Nat
test = [c (S 1), c Z, d (S Z)]
  where c : Nat -> Nat
        c x = 42 + x

        d : Nat -> Nat
        d y = c (y + 1 + z y)
              where z : Nat -> Nat
                    z w = y + w

