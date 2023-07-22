doubleSecondValue :: [Int] -> [Int]
doubleSecondValue [] = []
doubleSecondValue (a:[]) = [a]
doubleSecondValue (a:b:[]) = [a, b *2]
doubleSecondValue (a:(b:ls)) = a : b*2 : doubleSecondValue ls

doubleSecondValueRev :: [Int] -> [Int]
doubleSecondValueRev [] = []
doubleSecondValueRev (a:ls)
 | listLength (a:ls) `mod` 2 == 1 = doubleSecondValue (a:ls)
 |otherwise = [a * 2] ++ doubleSecondValue ls

addListEl :: [Int] -> Int
addListEl [] = 0
addListEl (a:[]) = a
addListEl (a:ls) = a + addListEl ls

addDoubled :: [Int] -> Int
addDoubled [] = 0
addDoubled (a:[]) = a
addDoubled (a:ls) = addListEl (doubleSecondValue (a:ls))

calcRemainder :: [Int] -> Bool
calcRemainder [] = False
calcRemainder (a:[])
    | a `mod` 10 == 0 = True
    | otherwise = False
calcRemainder (a:ls)
    | addDoubled (a:ls) `mod` 10 == 0 = True
    | otherwise = False

convertInt :: Int -> [Int]
convertInt a
    | a <= 0 = []
    | otherwise = convertInt (a `div` 10) ++ [a `mod` 10]

convertIntRev :: Int -> [Int]
convertIntRev a
    | a <= 0 = []
    | otherwise = (a `mod` 10) : convertIntRev (a `div` 10)

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (a:[]) = [a]
reverseList (a:ls) = reverseList ls ++ [a]

listLength :: [Int] -> Int
listLength [] = 0
listLength [a] = 1
listLength (_:ls) = 1 + listLength ls