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

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals str_list =
    List.filter (fn x => Char.isUpper(String.sub(x,0))) str_list

(* 2 *)
fun longest_string1 str_list =
    foldl (fn (str,i) => if (String.size i) < (String.size str) then str else i) "" str_list

(* 3 *)
fun longest_string2 str_list =
    List.foldl (fn (str,i) => if (String.size i) <= (String.size str) then str else i) "" str_list

(* 4a *)
fun longest_string_helper f str_list =
    List.foldl (fn (str,i) => if f(String.size i, String.size str) then str else i) "" str_list

(* 4b *)
val longest_string3 = longest_string_helper (fn (str,i) => i > str)
(* 4c *)
val longest_string4 = longest_string_helper (fn (str,i) => i >= str)

(* 5 *)
val longest_capitalized = longest_string3 o only_capitals

(* 6 *)
val rev_string = String.implode o List.rev o String.explode

(* 7 *)
fun first_answer f lst =
    case lst of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME v => v
		    | _ => first_answer f xs'

(* 8 *)
fun all_answers f lst =
    let fun commence lst acc =
	    case lst of
		[] => SOME acc
	      | x::xs' => case f x of
			      SOME v => commence xs' (v @ acc)
			    | NONE => NONE
    in
	commence lst []
    end

(* 9a *)
fun count_wildcards p =
    g (fn () => 1) (fn _ => 0) p

(* 9b *)
fun count_wild_and_variable_lengths p =
    g (fn () => 1) (fn v => String.size v) p

(* 9c *)
fun count_some_var (str, p) =
    g (fn () => 0) (fn v => if v = str then 1 else 0) p

(* 10 => Not very efficient, but it looks nice. *)
fun check_pat p =
    (g (fn () => 0) (fn v => if count_some_var(v,p) <= 1 then 0 else 1) p) < 1

(* 11 *)
fun match (v,p) =
    case (v,p) of
	(_,Wildcard) => SOME []
      | (_,Variable s) => SOME [(s,v)]
      | (Unit,UnitP) => SOME []
      | (Const n1,ConstP n2) => if n1 = n2
				then SOME []
				else NONE
      | (Tuple lst,TupleP plst) => if List.length lst = List.length plst
				   then all_answers match (ListPair.zip(lst,plst))
				   else NONE
      | (Constructor(s1,cv),ConstructorP(s2,cp)) => if s1 = s2
						    then match(cv,cp)
						    else NONE
      | _ => NONE

(* 12 *)
fun first_match v plst =
    case plst of
	[] => NONE
      | _  => SOME (first_answer (fn p => match(v,p)) plst) handle NoAnswer => NONE
