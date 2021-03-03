-- 1 --
solution_1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

-- 2 --
fib x y = if x + y < 4000000 then x:(fib y (x+y)) else [y]
solution_2 = sum [x | x <- (fib 1 2), x `mod` 2 == 0]

-- 3 --
prime x = length [y | y <- [2..x-1], mod x y == 0] == 0
primefactors x y = if y <= x then -- @x = prime factors of n, @y = start testing from y
                        if mod x y == 0 && prime y then
                            y:(primefactors (x `div` y) (y+1))
                        else primefactors x (y+1)
                    else []
solution_3 = maximum (primefactors 600851475143 1)
