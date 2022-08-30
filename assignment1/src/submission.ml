(*

FPSE Assignment 1
 
Name                  : Finn Dyer
List of Collaborators :

Please make a good faith effort at listing people you discussed any 
problems with here, as per the course academic integrity policy.  
CAs/Prof need not be listed!

Fill in the function definitions below replacing the 

  unimplemented ()

with your code.  Feel free to add "rec" to any function listed to make
it recursive. In some cases, you will find it helpful to define
auxillary functions, feel free to.

You must not use any mutation operations of OCaml for any of these
questions (which we have not taught yet in any case): no arrays,
for- or while-loops, references, etc.

*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(* You are required to use the Core libraries, don't remove the following line.
   If the editor is not recognizing Core (red squiggle under it for example),
   run a "dune build" from the shell -- the first time you build it will create some
   .merlin files which tells the editor where the libraries are.
*)
open Core

(* Here is a simple function which gets passed unit, (), as argument
   and raises an exception.  It is the initial implementation below. *)

let unimplemented () =
	failwith "unimplemented"
	
(*
	Part I Section 1: simple numeric recursions.
	
	All functions must be total for the specified domain;
	overflow is excluded from this restriction but should be avoided.
	
*)

(*
	Given a non-negative integer `n`, compute `0+1+2+ ... +n` using recursion
	(don't use the closed-form solution, do the actual addition).
*)
let rec summate (n: int): int =
	if n <= 0 then 0
	else n + summate (n-1)

(*
	Given non-negative integers `n` and `m`, compute their least common multiple.
*)
let rec gcf (n: int) (m: int): int =
	if n > m then gcf (n-m) m
	else if n < m then gcf n (m-n)
	else n

	let lcm (n: int) (m: int): int =
		(n * m) / (gcf n m)

(*
	Given a non-negative integer `n`, compute the n-th fibonacci number.
	Give an implementation that does not take exponential time; the naive 
	version from lecture is exponential	since it has two recursive calls for each call.
*)
let fibonacci (n: int): int =
  int_of_float (((((1. +. sqrt(5.)) /. 2.) ** (float_of_int n)) -. (((1. -. sqrt(5.)) /. 2.) ** (float_of_int n))) /. (sqrt 5.))


(*
	Part I Section 2: building lists. The List module functions may NOT be used (yet).
*)
	
(*
	Given a non-negative integer `n`, produce a list [n; n-1; ...; 2; 1].
*)
let rec iota1 (n: int): int list =
  let list = [n]
  in if n <= 1 then list
  else list @ iota1 (n-1)
	
(*
	Given a non-negative integer `n`, produce a list [1; 2; ...; n-1; n],
	without taking O(n^2) time.
*)
let rec iota2 (n: int): int list =
  let list = [n] in
  if n <= 1 then list
  else iota2 (n-1) @ list
	
(*
	Given a positive integer `n`, produce the list of integers in the range (0, n]
	which it is divisible by, in ascending order.
*)

let rec factors_help list n = 
  match list with
  | [] -> []
  | x :: xs -> (if n mod x = 0 then [x] else []) @ factors_help xs n

let factors (n: int): int list =
	factors_help (iota2 n) n
	
(*
	Part I Section 3: strings, lists, and sorting.  The List module functions 
	cannot be used.	String comparisons operations such as String.(<=) can be used, but
	no other String module functions.
*)

(*
	Given a list of strings, check to see if it is ordered, i.e. whether earlier 
	elements are less than or equal to later elements.
*)

let rec is_ordered (ls: string list): bool =
	match ls with
  | [] -> true
  | _ :: [] -> true
  | x :: y :: z -> if String.(<) x y then is_ordered (y :: z) else false

(*
	Given a string and an ordered list of strings, insert the string into the list so
	that the list remains ordered.  Return the list with the string inserted.

	Note this is an example of a *functional data structure*, instead of mutating
	you return a fresh copy with the element added.
*)

let rec insert_string (s: string) (l: string list): string list =
	match l with
	| [] -> []
	| [x] -> if String.(<) s x then s :: [x] else x :: [s]
	| x :: xs -> if String.(<) s x then s :: x :: xs else x :: (insert_string s xs)


(*
	Define a variation on the previous function which before inserting the element verifies 
	that the input list is indeed sorted.  Use the built-in `Base` function invalid_arg
	which will raise an exception
	Note that invalid_arg is a function that raises the Invalid_argument exception

	Important: Your invalid_arg function must take this exact string (without quotes):
	"List not sorted" 

	e.g. invalid_arg "List not sorted"

	This is because OUnit2 tests check not only for exception type but also the message.
	If you don't have the same error message as us, the autograder will fail you. 
*)

let rec insert_string_exn (s: string) (l: string list): string list =
	match l with
	| [] -> []
	| _ :: [] -> []
	| x :: y :: tl -> if String.(>) x y then invalid_arg "List not sorted" else
		if String.(<) x s then x :: s :: y :: tl else insert_string_exn s (y :: tl)


(*
	Define a function to sort a list of strings by a functional version of the 
	insertion sort method: repeatedly invoke insert_string to add elements one by one 
	to an initially empty list.
*)

let insertion_sort (l: string list): string list =
	unimplemented ()
	(* match l with
	| [] -> []
	| x :: [] -> [x]
	| x :: y :: tl -> if String.(<) x y then x :: (insertion_sort (y :: tl)) else y :: (insertion_sort (x :: tl)) *)

(*
	Define a function to remove the lexicographically maximum string in a list of strings.
	Return
	 Error("empty list") if the input list is empty (and has no max)
	 OK(s,s_list) for s the maximum string and s_list the list with s removed, 
	  if the list is not empty.
*)

let remove_max (l: string list): (string * string list, string) result =
	unimplemented ()

(*
	Write a sort routine by repeated invocations of remove_max to pull out the largest
	elements one-by-one.  You should never need to invoke `remove_max` on an empty
	list, and you can thus `assert false` (an invariant failure) if the `Error`
	case is ever returned from `remove_max`.  
	This problem shows how we can manually encode the exceptional condition in `
	remove_max` with `Ok`/`Error` but convert it to an actual side effect here
	(the `assert false` will raise an exception if hit).
*)

let max_sort (l: string list): string list =
	unimplemented ()

(* ***********************************************************************
    END PART I
************************************************************************ *)		

(* Part II Section 1: More error handling *)

(*
	Consider a student record as a list of Results of scores.
	Each entry will be Ok if the entry was retrieved, or an Error otherwise.
	The scores may be None, if the student did not complete the assignment.
	
	Given a student record, compute the mean of the existing scores, or None.
	In the case that one of the entries is an Error, throw an exception.
*)
type student_record = (float option, string) result list

let rec some_only (es: student_record) =
	match es with
	| [] -> []
	| x :: xs ->
		match x with
		| Error x -> failwith x
		| Ok (Some x) -> (Some x) :: (some_only xs)
		| Ok (None) -> some_only xs

let rec mean_help es : float option =
	match es with
	| [] -> None
	| x :: xs ->
		match x with
		| Some y -> Some (y +. match (mean_help xs) with | Some x -> x | None -> 0.)
		| None -> mean_help xs

let mean_of_record (es: student_record): float option =
	let so = (some_only es) in
	match mean_help so with
	| Some x -> Some (x /. float_of_int (List.length so))
	| None -> None

(*
	Given a student record as before, compute the mean of the scores.
	Assume that the scores do exist, and throw an exception otherwise.
	Collect each of the Error branches into a list of errors and return this too.
*)
let mean_of_record' (es: student_record): float * string list =
	unimplemented ()

(*
	Part II Section 2: for selected functions in Part I, provide
	a reimplementation of your previous code by refactoring
	the definition to use combinators provided by the List module.
	
	Care should be taken to use a concise, elegant combination
	of these provided functions to best express the task.
	
	These new implementations should not be explicitly recursive.
	Note that the autograder is not aware if you cheated and used recursion;
	we will manually inspect your code and give you negative points if 
	you used recursion.

*)

let iota1' (n: int): int list = 
	unimplemented ()

let iota2' (n: int): int list =
	unimplemented ()

let factors' (n: int): int list =
	unimplemented ()

let insert_string' (s: string) (l: string list): string list =
	unimplemented ()

(*
	Part II  Section 3: novel problems using List and String modules
	
	Use the List combinators, pipelining, etc. when possible
	and whenever it improves the readability of the solution.
*)

(*
	Given a list of integers, produce a list which pairs repeated integers
	with the number of times they repeated.
	E.G.
		run_length_encode [1; 1; 25; 4; 4; 4; -3; 1; 1] =
			[ (1, 2); (25, 1); (4, 3); (-3, 1); (1, 2) ]
*)
let run_length_encode (ns: int list): (int * int) list = 
	unimplemented ()

(*
	Given a list of values paired with repetition counts,
	produce the list which contains each value repeated the
	correct number of times.
	I.E.
		invert the previous function's transformation.
*)
let run_length_decode (ps: (int * int) list): int list = 
	unimplemented ()

(*
	Given a list of positive integers, compute the smallest integer which is evenly
	divisible by each of the integers in the list.
*)
let smallest_divisible_by (ns: int list): int = 
	unimplemented ()
	
(* 
	Given a string, check to see if the underlying sentence is a pangram, namely
	that it uses all 26 letters of the alphabet at least once.  Ignore all characters 
	outside of the alphabet and also ignore case.  See Base.String for helpful
	filtering and conversion functions.  Use pipes `|>` to make an elegant solution!
*)

let is_pangram (s: string): bool = 
	unimplemented ()
