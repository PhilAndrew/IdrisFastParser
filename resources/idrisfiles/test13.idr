import Data.Vect

vec : (n : Nat ** Vect n Int)
vec = (2 ** [3, 4])

vec2 : DPair Nat (\n => Vect n Int)
vec2 = MkDPair 2 [3, 4]

vec3 : (n : Nat ** Vect n Int)
vec3 = (_ ** [3, 4])

vec4 : (n ** Vect n Int)
vec4 = (_ ** [3, 4])

filter2 : (a -> Bool) -> Vect n a -> (p ** Vect p a)
filter2 p Nil = (_ ** [])
filter2 p (x :: xs)
    = case filter2 p xs of
           (_ ** xs') => if p x then (_ ** x :: xs')
                                else (_ ** xs')

