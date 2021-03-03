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

-- 4 --
solution_4 = maximum [a*b | a <- [999,998..100], b <- [999,998..100], show (a*b) == reverse (show (a*b))]

-- 5 --
-- inefficient brute force solution, takes 47s when compiled
divisibleByList l n = case l of {[x] -> (mod n x == 0); (x:xs) -> (mod n x == 0 && divisibleByList xs n)}
incUntilTrue x predicate = if predicate x then x else incUntilTrue (x+1) predicate
solution_5 = incUntilTrue 1 (divisibleByList [1..20])
-- alternative much better solution: foldl lcm 1 [1..20] (recursively find least common multiple)

-- 6 --
solution_6 = abs (sum [x*x | x <- [1..100]] - ((\ x -> x * x ) (sum [1..100])))

-- 7 --
-- brute_solution_7 = last (take 10001 [x | x <- [1..], prime x])
primeWithListOfPrimes ls x = length [y | y <- ls, mod x y == 0] == 0 -- more efficient than checking against all numbers
listOfPrimes ls x = if primeWithListOfPrimes ls x then x:(listOfPrimes (x:ls) (x+1)) else listOfPrimes ls (x+1)
solution_7 = last (take 10000 (listOfPrimes [] 2)) -- 10 000 bc 1 is not in the list but is the first prime

main = print (solution_7)
