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

fun only_capitals xs = List.filter (fn x => (Char.isUpper o String.sub) (x,0)) xs

fun longest_string1 xs = foldl (fn (x,y) => if String.size x > String.size y then x else y) "" xs

fun longest_string2 xs = foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" xs

fun longest_string_helper f xs = foldl ( fn (x,y) => if f (String.size x, String.size y) then x else y) "" xs

fun longest_string3 xs = 
    longest_string_helper (fn (x,y) => x > y) xs

fun longest_string4 xs = 
    longest_string_helper (fn (x,y) => x >= y) xs

fun longest_capitalized xs = (longest_string1 o only_capitals) xs

fun rev_string x = (String.implode o rev o String.explode) x

fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
     |  x::xs' => case f x of
                      SOME v => v
                    | NONE  => first_answer f xs'

fun all_answers f xs =
    let fun aux (xs, acc) =
            case xs of
                [] => SOME acc
              | x::xs' => case f x of
                              NONE => NONE
                            | SOME lst => aux(xs', acc@lst)
    in
        aux(xs,[])
    end

fun count_wildcards p =
    g (fn x => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
    g (fn x => 1) (fn x => String.size x) p

fun count_some_var (v, p) = 
    g (fn x => 0) (fn x => if x = v then 1 else 0) p

fun check_pat p = 
    let fun get_vars p =
            case p of
	            Wildcard          => []
	          | Variable x        => [x]
	          | TupleP ps         => List.foldl ((fn (p,xs) => xs@(get_vars p))) [] ps
	          | ConstructorP(_,p) => get_vars p
	          | _                 => []
        fun no_duplicates xs =
            case xs of
                [] => true
              | x::[] => true
              | x::xs' =>  (not (List.exists (fn y => y = x) xs')) andalso no_duplicates xs'
    in
        (no_duplicates o get_vars) p
    end

fun match (v, p) = 
    case (v, p) of 
        (_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs <> List.length ps
                                then NONE
                                else all_answers (fn (v', p') => match(v',p')) (ListPair.zip(vs,ps))
      | (Constructor (s1,v'), ConstructorP (s2,p')) => if s1 = s2
                                                       then match(v',p')
                                                       else NONE
      | _ => NONE

fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps)
    handle NoAnswer => NONE
