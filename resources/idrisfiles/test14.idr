import Data.Vect

record Person where
    constructor MkPerson
    firstName, middleName, lastName : String
    age : Int

fred : Person
fred = MkPerson "Fred" "Joe" "Bloggs" 30

record Class where
    constructor ClassInfo
    students : Vect n Person
    className : String

addStudent : Person -> Class -> Class
addStudent p c = record { students = p :: students c } c

addStudent2 : Person -> Class -> Class
addStudent2 p c = record { students $= (p ::) } c


||| Nested record fields can be accessed using the prefix notation, too:
||| (c . b . a) x
||| map (c . b . a) xs

record Prod a b where
    constructor Times
    fst : a
    snd : b

record SizedClass (size : Nat) where
    constructor SizedClassInfo
    students : Vect size Person
    className : String

addStudent3 : Person -> SizedClass n -> SizedClass (S n)
addStudent3 p c = record { students = p :: students c } c
