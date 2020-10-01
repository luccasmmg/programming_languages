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

fun first_answer f = (fn x => case x of SOME(SOME x) => x
                                      | _ => raise NoAnswer) o List.find isSome o List.map f

fun all_answers f xs =
    let fun aux(f, [], acc) = SOME acc
          | aux (f, x::xs, acc) = case f(x) of
                                    NONE => NONE
                                    | SOME x => aux(f, xs, x @ acc)
    in
      aux(f, xs, [])
    end

val count_wildcards = g (fn () => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)

fun count_some_var (str, pattern) = g (fn () => 0) (fn x => if x=str then 1 else 0) pattern

fun check_pat pattern =
    let fun aux(Variable x) = [x]
          | aux (Wildcard) = []
          | aux (TupleP ps) = List.foldl(fn (p,i) => (aux p) @ i) [] ps
          | aux (ConstructorP(_, p)) = aux p
          | aux _ = []
        fun all_unique [] = true
          | all_unique (x::xs) = case List.exists (fn y => y=x) xs of
                                   true => false
                                  | false => all_unique(xs)
    in
      all_unique(aux pattern)
    end

fun match (_, Wildcard) = SOME []
  | match (v, Variable s) = SOME [(s, v)]
  | match (Unit, UnitP) = SOME []
  | match (Const i, ConstP i') = if i=i' then SOME [] else NONE
  | match (Tuple vs, TupleP ps) = if List.length vs = List.length ps then all_answers (fn x => match x) (ListPair.zip (vs, ps)) else NONE
  | match (Constructor(s,v), ConstructorP(s',p)) = if s=s' then match(v,p) else NONE
  | match _ = NONE

fun first_match p xs = first_answer (fn x => case match(p, x) of SOME y => SOME(SOME y)
                                                              | NONE => NONE) xs handle NoAnswer => NONE
