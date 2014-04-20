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



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals sl = List.filter (fn s => Char.isUpper (String.sub (s,0))) sl 

fun longest_string1 sl = foldl (fn (a,b) => if String.size a > String.size b then a else b) "" sl
fun longest_string2 sl = foldl (fn (a,b) => if String.size a >= String.size b then a else b) "" sl

fun longest_string_helper exprBool sl = foldl (fn (a,b) => if exprBool (String.size a,String.size b) then a else b) "" sl

val longest_string3 = longest_string_helper (fn (a,b) => a > b) 
val longest_string4 = longest_string_helper (fn (a,b) => a >= b) 

val longest_capitalized = (longest_string1 o only_capitals)
fun rev_string s = implode (rev (explode s))

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

fun first_answer f al =
    case al of
	[] => raise NoAnswer
      | x::al' => case (f x) of
		      NONE => first_answer f al'
		    | SOME i => i

fun all_answers f al =
    let fun foo (al,acc) =
	    case al of
		[] => SOME acc
	      | x::al' => case (f x) of
			      NONE => NONE
			    | SOME i => (foo (al', (i@acc)))
    in foo (al,[])
    end

fun count_wildcards p = g (fn x => 1) (fn s => 0) p
fun count_wild_and_variable_lengths p = g (fn x => 1) (String.size) p
fun count_some_var (s,p) = g (fn x => 0) (fn str => if str = s then 1 else 0) p

fun check_pat p =
    let 
	fun foo p =
	    case p of
		Wildcard          => []
	      | Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (p,i) => (foo p)@i ) [] ps
	      | ConstructorP(_,p) => foo p
	      | _                 => []
	fun bar ls =
	    case ls of
		x::y::ls' => if not (List.exists (fn a => x=a ) (y::ls') ) then bar (y::ls') else false
	      | _ => true
    in
	bar (foo p)
    end

fun match (va,p) =
    case (va,p) of
	(Unit, UnitP) => SOME []
      | (Tuple vl, TupleP pl) => if length vl = length pl then all_answers (fn (a,b) => match(a,b)) (ListPair.zip(vl,pl) ) else NONE
      | (Constructor (sv,v1), ConstructorP (sp,p1)) => if (sp=sv) then match (v1,p1) else NONE
      | (Const iv, ConstP ip) => if ip=iv then SOME [] else NONE
      | (v, Variable s) => SOME [(s,v)]
      | (_, Wildcard) => SOME []
      | _ => NONE

fun first_match v pl =
    SOME (first_answer (fn x => match(v,x))  pl) handle NoAnswer => NONE
