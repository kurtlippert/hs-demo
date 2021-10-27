module Baby where

import Prelude (otherwise, IO, print, putStrLn, show, getLine)
import NriPrelude


firstThing :: Text
firstThing = "the first thing"

{- 
>>> doubleMe 30
60
-}
doubleMe :: Num a => a -> a
doubleMe x = x + x

{-
>>> doubleUs 2 6
12
-}
doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

{-
>>> doubleSmallNumber 10
20

>>> doubleSmallNumber 150
150
-}
doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x =
    if x > 100
        then x
        else x*2

{-
>>> len [1,2,4,5]
4

but of course, we can just use 
the standard bits from 'nri-prelude':

>>> List.length [1,2,3,4]
4

>>> Text.length "12345"
5
-}
len :: (Num b) => [a] -> b
len [] = 0
len (_:xs) = 1 + len xs


{-
>>> factorial 5
120
-}
factorial :: (Num a, Eq a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

{-
>>> lucky 7
"LUCKY NUMBER SEVEN!"

>>> lucky 3
"Sorry, you're out of luck, pal!"
-}
lucky :: (Num a, Eq a) => a -> Text
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

{-
>>> List.map numberGuess [0, 25, 30, 40]
["too low","just right","too high","waay too high"]
-}
numberGuess :: (Num a, Ord a) => a -> Text
numberGuess number
    | number <= 18 = "too low"
    | number <= 25 = "just right"
    | number <= 30 = "too high"
    | otherwise = "waay too high"


{-
>>> first (1,2,3)
1
-}
first :: (a, b, c) -> a
first (x, _, _) = x

{-
>>> second (1,2,3)
2
-}
second :: (a, b, c) -> b
second (_, y, _) = y

{-
>>> third (1,2,3)
3
-}
third :: (a, b, c) -> c
third (_, _, z) = z

{-
>>> lenThenAddTwo [1,2,3]
5
-}
lenThenAddTwo :: (Num b) => [a] -> b
lenThenAddTwo list = len list |> (+2)

{- 
>>> addFive [1,2,3]
[6,7,8]
-}
addFive :: List Int -> List Int
addFive =
    List.map (+3) >> List.map (+2)

{-
>>> addVectors (1,2) (3,4)
(4,6)
-}
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

{-
>>> capital "thing"
"The first letter of thing is t"
>>> capital ""
"Empty string, whoops!"
>>> capital "CAPS"
"The first letter of CAPS is C"
-}
capital :: Text -> Text
capital "" = "Empty string, whoops!"
capital all = "The first letter of " ++ all ++ " is " ++ Text.left 1 all

{- add five to the number
>>> addFive' 5
10
-}
addFive' :: Num a => a -> a
addFive' num =
    num
    |> (+1)
    |> (+4)

{- add five to the number (pointfree)
>>> addFive' 5
10
-}
addFive'' :: Num a => a -> a
addFive'' =
    (+1) >> (+4)

{- convert from string to int, then add 5
>>> castThenAddFive "5"
Just 10
-}
castThenAddFive :: Text -> Maybe Int
castThenAddFive =
    Text.toInt >> Maybe.map (+5)

{- get first digit for each in list.
Weird for negative numbers because
'Text.left' on '-1' drops the '1'
making it only '-'. We can make the negative
positive, which resolves the criteria for this
function I think.

>>> getFirstDigits [123, 456, 789] == [1, 4, 7]
True

>>> getFirstDigits [123, 456, 789]
[1,4,7]

>>> getFirstDigits [30, 20, 1]
[3,2,1]

>>> getFirstDigits [-12, -3]
[1,3]
-}
getFirstDigits :: List Int -> List Int
getFirstDigits nums =
    nums
    |> List.map abs
    |> List.map Text.fromInt
    |> List.map (Text.left 1)
    |> List.map Text.toInt
    |> List.map (Maybe.withDefault 0)

{- this was a difficult one, not sure I
implemented it correctly.

>>> firstDigitsAsNumber [123, 456, 789]
147

>>> firstDigitsAsNumber [0, 0, 0]
0

>>> firstDigitsAsNumber [-138, -38]
13
-}
firstDigitsAsNumber :: List Int -> Int
firstDigitsAsNumber nums =
    nums
    |> getFirstDigits
    |> List.map Text.fromInt
    |> List.foldr Text.append ""
    |> Text.toInt
    |> Maybe.withDefault 0

{- point free version of 'firstDigitsAsNumber'
works the same as above except we just remove
the parameter.

I think generally the non-pointfree versions
look a little better for certain things.

>>> firstDigitsAsNumber' [123, 456, 789]
147
-}
firstDigitsAsNumber' :: List Int -> Int
firstDigitsAsNumber' =
    getFirstDigits
    >> List.map Text.fromInt
    >> List.foldr Text.append ""
    >> Text.toInt
    >> Maybe.withDefault 0
