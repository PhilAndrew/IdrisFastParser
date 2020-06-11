
foo : Int -> Int
foo x = case isLT of
            MyYes => x*2
            MyNo => x*4
    where
       data MyLT = MyYes | MyNo

       isLT : MyLT
       isLT = if x < 20 then MyYes else MyNo
