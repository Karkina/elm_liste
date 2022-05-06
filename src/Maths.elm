module Maths exposing (..)
import Html


type Bit
  = Zero
  | One

type alias Bits =
  List Bit

myBits = [One, Zero, One, One, Zero ]

-- Add all the number from 1 to n.
-- Change function body.
sum : Int -> Int
sum n = case n of 
    1->1
    _-> n + sum (n-1)

-- Multiply all the numbers from 1 to n.
-- Change function body.
fact : Int -> Int
fact n = case n of 
    0-> 1
    _ -> n * fact (n-1)

-- Compute the nth Fibonacci number.
-- Change function body.
fib : Int -> Int
fib n = case n of 
    0->0
    1->1
    _->fibTail(n-1)+fibTail(n-2)

-- Compute the nth Fibonacci number in a tail-rec way.
-- Change function body.
fibTail : Int -> Int
fibTail n = 
    fibHelp n 1 0


fibHelp: Int -> Int -> Int -> Int
fibHelp n acc1 acc2 =case n of 
    0-> 0
    1-> 1
    2-> acc1 + acc2
    _ -> fibHelp (n-1) (acc1+acc2) (acc1)



bitTostring : Bit -> String
bitTostring bit = case bit of 
    Zero -> "0"
    One -> "1"

bitsTostring bits = case bits of
    []-> ""
    head::tail -> bitTostring head ++ bitsTostring tail 

toInt : List Bit -> Int
toInt bits =
    toIntHelp bits (List.length bits-1) 0
toIntHelp :  Bits -> Int ->Int ->Int
toIntHelp bits acc1 acc2= case bits of 
    [] -> acc2
    head::tail -> if (bitTostring(head) == "1") then toIntHelp tail (acc1-1) (acc2 + 2^acc1)
                    else toIntHelp tail (acc1-1) (acc2)
fromInt num =case num of
    0->[Zero]
    1->[One]
    _->fromIntHelp num 16 []

fromIntHelp num acc acc2=
    let puissance = 2^(acc-1) in
    if acc == 0 then List.reverse acc2
    else if puissance == num then fromIntHelp 0 (acc-1) (One::acc2)
    else if puissance > num then fromIntHelp num (acc-1) (Zero::acc2)
    else fromIntHelp (num-puissance) (acc-1) (One::acc2)

notBits bits=
    notBitsHelp bits []

notBitsHelp bits acc= case bits of
    []-> List.reverse acc
    head::tail -> if head == Zero then notBitsHelp tail (One::acc)
                  else notBitsHelp tail (Zero::acc)
helloWorld = 0
main : Html.Html msg
main =
  Html.text (Debug.toString(notBits myBits))
