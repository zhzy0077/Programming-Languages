(* Dan Grossman, CSE341 Spring 2013, HW3 Provided Code *)

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

(**** you can put all your code here ****)

val only_capitals = 
	List.filter (fn x => (Char.isUpper o String.sub) (x, 0))

val longest_string1 = 
	foldl (
		    fn (a, b) => if String.size a > String.size b
						 then a
						 else b
		   ) ""

val longest_string2 = 
	foldl (
		    fn (a, b) => if String.size a >= String.size b
						 then a
						 else b
		   ) ""

fun longest_string_helper f = 
	foldl (
			fn (a, b) => if f (String.size a, String.size b)
						 then a
						 else b
		  ) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer f xs = 
	case xs of
		[] => raise NoAnswer
      | x::xs' => case f x of
                       NONE => first_answer xs' 
                     | SOME v => v   		

fun all_answers f xs = 
    let fun find_answer xs acc = 
            case xs of
                [] => SOME acc
              | x::xs' => case f x of
                               NONE => NONE
                             | SOME v => find_answer xs' (v @ acc)
    in
        find_answer xs []
    end

val count_wildcards = g (fn () => 1) (fn x => 0)
