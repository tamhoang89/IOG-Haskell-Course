count :: Eq a => a -> [a] -> Int
count x bs = ((length .) . filter . (==)) x bs
count x bs = (  (length .)   .   (filter . (==))   )   x bs
            --      ( f       .         g ) x 
count x bs = (  (length .)    (  (filter . (==))  x)  )    bs
                                --  (f   .  g   ) x
count x bs = (  (length .)    (  (filter ((==)  x)))  )    bs

count x bs = (  (length .)    (  (filter (== x) ))  )    bs

count x bs = length (      filter (== x)    bs )

count x bs = length (filter (==x) bs)
