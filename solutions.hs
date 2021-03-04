import Data.Char -- digitToInt

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
-- alternative much better solution: foldl lcm 1 [1..20] (recursively find least common multiple)

divisibleByList l n = case l of {[x] -> (mod n x == 0); (x:xs) -> (mod n x == 0 && divisibleByList xs n)}

satisfyPredicate predicate x = if predicate x then x else satisfyPredicate predicate (x+1)

solution_5 = satisfyPredicate (divisibleByList [1..20]) 1




-- 6 --
solution_6 = abs (sum [x*x | x <- [1..100]] - ((\ x -> x * x ) (sum [1..100])))



-- 7 --
-- more efficient to check against a prime list than against all numbers
-- using null xs instead of length xs == 0 (constant vs linear time) (reduced solution 7 by 6s)
-- only checking against first half of the numbers (up to sqrt x) (a prime conjecture) is a big optimization (reduced solution 7 time by 17s)

isPrimeWithListOfPrimes ls x = let cmp = ceiling (sqrt (fromIntegral x)) in null [y | y <- ls, y <= cmp, mod x y == 0]

listOfPrimes ls x = if isPrimeWithListOfPrimes ls x 
                       then x:(listOfPrimes (x:ls) (x+1))
                       else listOfPrimes ls (x+1)

-- takes 6.7 seconds when compiled 
solution_7 = last (take 10001 (listOfPrimes [] 2))



-- 8 --
number8 = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

-- convert number to int list
numberIntList = map (\x -> digitToInt x) number8

multiply13digits ls = product (take 13 ls)

-- calculate a set of products of 13 digits
thirteenDigitProducts ls res = if (length ls) < 13
                                  then res
                                  else let (x:xs) = ls in thirteenDigitProducts xs ((multiply13digits ls):res)

solution_8 = maximum (thirteenDigitProducts numberIntList [])



-- 9 --
solution_9 = head [a*b*c | c <- [1..], b <- [1..c], a <- [1..b], a*a + b*b == c*c && a+b+c == 1000]
