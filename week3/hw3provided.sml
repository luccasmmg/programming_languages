(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

val only_capitals = List.filter (fn x => Char.isUpper(String.sub(x, 0)))

val longest_string1 = foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) ""

val longest_string2 = foldl (fn (x, y) => if String.size(x) < String.size(y) then y else x) ""

fun longest_string_helper f = foldl (fn (x, y) => if f(String.size(x), String.size(y)) then x else y) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_anwser f = (fn x => case x of SOME(SOME x) => x
                                      | NONE => raise NoAnswer) o List.find isSome o List.map f

fun all_answers f xs =
    let fun aux(f, [], acc) = SOME acc
          | aux (f, x::xs, acc) = case f(x) of
                                    NONE => NONE
                                    | SOME x => aux(f, xs, x @ acc)
    in
      aux(f, xs, [])
    end
