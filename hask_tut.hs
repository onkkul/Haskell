--------------------Helper functions to get user input-------------------
getInt :: IO Int 
getInt = do 
    line <- getLine
    return (read line :: Int)

convert_to_list :: Int -> [Int] -> [Int]
convert_to_list x [] = [x]
convert_to_list x it = x:it

getIntList :: IO [Int]
getIntList = do 
    number <- getInt
    if number == -999 
        then return []
    else (do 
        it <- getIntList
        return (convert_to_list number it))



--------------------question 1 : Max of it---------------------------
maxlist :: [Int] -> Int
maxlist it = maximum it



-------------------question 2 : Delete K'th element------------------
delete :: Int -> [Int] -> [Int]
delete _ [] = []
delete 0 it = it
delete k it = do
    if (length it < k)
        then it
    else (take (k-1) it ++ delete k (drop k it))



-------------------question 3 : Insertion sort---------------------
insert :: Int -> [Int] -> [Int]
insert element [] = [element]
insert element unsorted = do
    let key = head unsorted
    let remaininglist = tail unsorted
    if element <= key
        then element : key : remaininglist
    else key : insert element remaininglist

isort :: [Int] -> [Int]
isort [] = []
isort it = do
    let element = head it
    let unsorted = tail it
    insert element (isort unsorted)



-------------------question 4 : rotate n position right-------------
shift :: Int -> [Int] -> [Int]
shift k it = zipWith const (drop k (cycle it)) it

rotate :: Int -> [Int] -> [Int]
rotate _ [] = []
rotate k it = do
    if length it == k
        then shift (k) it
    else if length it - k == 1
        then shift (k-1) it
    else shift (k+1) it



-------------------question 5 : it(single element)--------------
single :: [Int] -> [[Int]]
single [] = [[]]
single it = [[element] | element <- it]



-------------------question 6 : double(single element)--------------
multiply :: [Int] -> [Int]
multiply it = (take 1 (reverse it)) ++ [2*x | x <- (drop 1 (reverse it))]

double :: [Int] -> [Int]
double it = do
    if (length it < 1)
        then it
    else if (length it == 1)
        then [2*x | x <- it]
    else ((reverse (multiply (take 2 it))) ++ double (drop 2 it))



