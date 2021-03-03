-- 1 --
solution_1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

-- 2 --
fib x y = if x + y < 4000000 then x:(fib y (x+y)) else [y]
solution_2 = sum [x | x <- (fib 1 2), x `mod` 2 == 0]
