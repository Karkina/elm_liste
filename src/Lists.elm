module Lists exposing (..)

-- Compute the length of a List without using List.length.
-- Change function body.
length : List a -> Int
length list = case list of 
    [] -> 0
    head::tail -> 1 + length tail

-- Checks if a value is includes in the List without using List.member.
-- Change function body.
member : a -> List a -> Bool
member mem list = case list of
    [] -> False
    head::tail -> if head == mem then True
                    else member mem tail

-- Removes all elements at an even position in the List.
-- Change function body.
filterEven : List Int -> List Int
filterEven list = 
    filterHelp list 1 []

filterHelp : List Int -> Int -> List Int -> List Int
filterHelp list acc1 acc2=case list of
    []-> List.reverse acc2
    head ::tail ->if modBy 2 acc1 == 0 then filterHelp tail (acc1+1)  (head::acc2)
                    else filterHelp tail (acc1+1)  (acc2)

-- Removes all elements not satistfying the predicate.
-- Change function body.
filter : (a -> Bool) -> List a -> List a
filter predicate list = List.filter predicate list

-- Computes the sum of a list of Int.
-- Change function body.
sum : List Int -> Int
sum list = case list of 
    [] -> 0
    head::tail -> head + sum tail

-- Append the second list to the first without using List.append.
-- Change function body.
append : List a -> List a -> List a
append list1 list2 =
    appendHelper (List.reverse list1) list2

appendHelper : List a -> List a -> List a 
appendHelper list1 list2=case list1 of
    [] -> list2
    head::tail -> append tail (head::list2)
