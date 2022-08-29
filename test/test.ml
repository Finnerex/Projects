open Core;;

let bottles = function
  | 0 -> Core.printf "No more bottles of beer on the wall, no more bottles of beer. \n Go to the store and buy some more, 99 bottles of beer on the wall."
  | 1 -> Core.printf "1 bottle of beer on the wall, 1 bottle of beer. \n Take it down and pass it around, no more bottles of beer on the wall.\n\n"
  | 2 -> Core.printf "2 bottles of beer on the wall, 2 bottles of beer. \n Take one down and pass it around, 1 bottle of beer on the wall.\n\n"
  | n -> Core.printf "%d bottles of beer on the wall, %d bottles of beer. \n Take one down and pass it around, %d bottles of beer on the wall.\n\n" n n (n-1)

let rec print_song n =
  if n >= 0 then (
    bottles n;
    print_song (n-1))
  else -1

(*grade school problem*)

type students = (int * string) list


let rec is_ordered ~f l =
  match l with
  | [] -> true
  | _ :: [] -> true
  | a :: b :: xs -> if f a b then is_ordered ~f (b :: xs) else false

let rec sort_bubble ~f l =
  match l with
  | [] -> []
  | x :: [] -> x :: []
  | a :: b :: xs ->
    if f a b then a :: sort_bubble ~f (b :: xs)
    else b :: sort_bubble ~f (a :: xs)

(*sort students by grade number*)
let compare_grade a b = 
  match a, b with
  | (g1, _), (g2, _) -> g1 <= g2

let rec sort_by_grade (l: students) =
  if is_ordered ~f:compare_grade l then l
  else l |> sort_bubble ~f:compare_grade |> sort_by_grade

(*sort students by name, alphabetically*)
let compare_name a b =
  match a, b with
  | (_, n1), (_, n2) -> String.(<=) n1 n2

let rec sort_by_name (l: students) =
  if is_ordered ~f:compare_name l then l
  else l |> sort_bubble ~f:compare_name |> sort_by_name

(*ouput members of a grade*)
let rec members_of_grade grade (l: students) =
  match l with
  | [] -> []
  | (x, n) :: xs -> if x = grade then n :: members_of_grade grade xs else members_of_grade grade xs

