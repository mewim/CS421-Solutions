
module Mp1Solution where

data Cons a = Cons a (Cons a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Int
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

mytake :: Int -> [Int] -> [Int]
mytake 0 _ = []
mytake n [] = []
mytake n (x:xs)
  | n < 0 = []
  | otherwise = x : (mytake (n-1) xs)

mydrop :: Int -> [Int] -> [Int]
mydrop n [] = []
mydrop 0 xx = xx
mydrop n (x:xs)
  | n < 0 = (x:xs)
  | otherwise = (mydrop (n-1) xs)

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

app :: [t] -> [t] -> [t]
app xx yy = xx ++ yy

add :: Ord a => a -> [a] -> [a]
add n [] = [n]
add n (x:xs)
  | n < x = [n,x] ++ xs
  | n == x = x : xs
  | otherwise = x : (add n (xs))

union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union [] ys = ys
union xs [] = xs
union (x:xs) (y:ys)
  | x == y = x : (union xs ys)
  | x < y = x : (union xs (y:ys))
  | x > y = y : (union (x:xs) ys)

intersect :: Ord a => [a] -> [a] -> [a]
intersect [] [] = []
intersect (x:xs) (y:ys)
  | x == y = x : (intersect xs ys)
  | x < y = (intersect xs (y:ys))
  | otherwise = (intersect (x:xs) ys)
intersect xs [] = []
intersect [] ys = []

powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) =
  let p = powerset xs
    in union p [add x s | s <- p]

inclist :: (Num a) => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x + 1) : inclist xs

sumlist :: (Num a) => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

myzip :: [t] -> [t1] -> [(t, t1)]
myzip [] [] = []
myzip xs [] = []
myzip [] ys = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs [] [] = []
addpairs xs [] = []
addpairs [] ys = []
addpairs (x:xs) (y:ys) = (x+y) : addpairs xs ys

ones :: [Integer]
ones = 1 : ones

nats :: [Integer]
nats = [1..]

fib :: [Integer]
fib = 1 : 1 : (addpairs fib (tail fib))

list2cons :: [a] -> Cons a
list2cons [] = Nil
list2cons (x:xs) = (Cons x (list2cons xs))

cons2list :: Cons a -> [a]
cons2list Nil = []
cons2list (Cons n x) = n : cons2list x

eval :: Exp -> Int
eval (IntExp n) = n
eval (PlusExp is) = sum (map eval is)
eval (MultExp is) = foldr (*) 1 (map eval is)

inclist' :: (Num a) => [a] -> [a]
inclist' xs = map (+1) xs

sumlist' :: (Num t) => [t] -> t
sumlist' xs = foldr (+) 0 xs

list2cons' :: [a] -> Cons a
list2cons' xs = foldr Cons Nil xs
