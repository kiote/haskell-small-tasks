doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else x*2
doubleSmallNumber' x = (doubleSmallNumber x) + 1 
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <-xs, odd x]
addLists xs ys = [x+y|x<-xs, y<-ys]
rightTriangles = [(a,b,c)| c<-[1..10], a <- [1..c], b<-[1..a], a^2+b^2==c^2]
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)
head' :: [a] -> a
head' [] = error "The list is empty"
head' (x:_) = x
tell [] = "empty list"
tell (x:[]) = "One element: " ++ show x
tell (x:y:[]) = "Two elements: " ++ show x ++ show y
tell (x:_) = "Too many elements"
bmiTell height weight 
    | bmi <= 18.5 = "You are underweight, eat more"
    | bmi <= 25.0 = "Looking good"
    | bmi <= 30.0 = "You need to workout"
    | otherwise = "Go see the doctor" 
    where bmi = weight / height ^ 2
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname  
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2*pi*r*h
        topArea = pi*r^2
    in sideArea + 2*topArea
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
  where smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x ]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
