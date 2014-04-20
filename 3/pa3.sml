val filter = List.filter
val zip = ListPair.zip

fun only_capitals strs =
    filter (fn str => Char.isUpper(String.sub(str, 0))) strs

fun longest_string1 strs =
    foldl (fn (candidate, longest) =>
              if String.size(candidate) > String.size(longest)
              then candidate
              else longest)
          "" strs

fun longest_string2 strs =
    foldl (fn (candidate, longest) =>
              if String.size(candidate) >= String.size(longest)
              then candidate
              else longest)
          "" strs


fun longest_string_helper cmp strs =
    foldl (fn (candidate, longest) =>
              if cmp(String.size(candidate), String.size(longest))
              then candidate
              else longest)
          "" strs

val longest_string3 = longest_string_helper op>
val longest_string4 = longest_string_helper op>=

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

exception NoAnswer

fun first_answer getter items =
    let val maybe_answer = foldl (fn (item, acc) => case (acc, getter(item)) of
                                                        (SOME v, _) => SOME v
                                                      | (_, SOME v) => SOME v
                                                      | _           => NONE)
                                 NONE items
    in case maybe_answer of
           SOME a => a
         | _ => raise NoAnswer
    end

fun all_answers trans items =
    foldl (fn (item, acc) => case(acc, trans(item)) of
                                 (SOME xs, SOME vs) => SOME (vs @ xs)
                               | _                  => NONE)
          (SOME []) items

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
	      | TupleP ps         => foldl (fn (p,i) => (r p) + i) 0 ps
	      | ConstructorP(_,p) => r p
	      | _                 => 0
    end

val count_wildcards = g (fn () => 1) (fn _ => 0)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size(x))
fun count_some_var (varname, p) = g (fn _ => 0) (fn x => if x = varname then 1 else 0) p

fun check_pat p =
    let fun helper (Variable x) = [x]
          | helper (ConstructorP(_, p)) = helper p
          | helper (TupleP ps) = foldl (fn (p, acc) => acc @ (helper p)) [] ps
          | helper _ = []
        fun distinct_checker []      = true
          | distinct_checker (v::vs) = not(List.exists (fn x => v = x) vs)
                                       andalso distinct_checker vs
    in
        distinct_checker(helper p)
    end

fun match (v, Variable name)                      = SOME [(name, v)]
  | match (Constructor(_, v), ConstructorP(_, p)) = match(v, p)
  | match (_, Wildcard)                           = SOME []
  | match (Const(v), ConstP(p))                   = if v = p then SOME [] else NONE
  | match (Unit, UnitP)                           = SOME []
  | match (Tuple vs, TupleP ps)                   = 
    if List.length(vs) = List.length(ps) then
        all_answers match (zip(vs, ps))
    else NONE
  | match _                                       = NONE

fun first_match v ps =
    (SOME (first_answer (fn p => match (v, p)) ps))
    handle NoAnswer => NONE

(* type checking *)

datatype typ = Anything
	         | UnitT
	         | IntT
	         | TupleT of typ list
	         | Datatype of string

fun typecheck_patterns (ctors, patterns) =
    let fun get_ctor_type ctor_type =
            let val matches = filter (fn (ctor, dtype, typ_) =>
                                         ctor = ctor_type)
                                     ctors
                val types = map (fn (ctor, dtype, typ_) => typ_) matches
            in case types of
                   t::ts => SOME t
                 | _ => NONE
            end
        fun pattern_to_type UnitP = SOME UnitT
          | pattern_to_type Wildcard = SOME Anything
          | pattern_to_type (ConstP _) = SOME IntT
          | pattern_to_type (Variable _) = SOME Anything
          | pattern_to_type (TupleP parts) = 
            foldl (fn (part, acc) =>
                      case (acc, pattern_to_type(part)) of
                          (SOME (TupleT acc), SOME t) =>
                          SOME (TupleT (rev(t :: rev(acc))))
                        | _ => NONE)
                  (SOME (TupleT [])) parts
          | pattern_to_type (ConstructorP (name, pattern)) = get_ctor_type name
        fun unify_pair (SOME IntT, SOME _) = SOME Anything
          | unify_pair (SOME UnitT, SOME _) = SOME Anything
          | unify_pair (SOME (TupleT ts), SOME Anything) =
            SOME (TupleT (map (fn _ => Anything) ts))
          | unify_pair (SOME Anything, SOME (TupleT ts)) =
            unify_pair ((SOME (TupleT ts)), (SOME Anything))
          | unify_pair (SOME (TupleT t1s), SOME (TupleT t2s)) =
            if length t1s = length t2s then
                foldl (fn ((t1, t2), acc) =>
                          case (unify_pair(SOME t1, SOME t2), acc) of
                              (SOME new_type, SOME (TupleT old_types)) =>
                              SOME (TupleT (rev(new_type::rev(old_types))))
                            | _ => NONE)
                      (SOME (TupleT [])) (zip(t1s, t2s))
            else NONE
          | unify_pair (SOME Anything, SOME Anything) = SOME Anything
          | unify_pair _ = NONE
    in
        case patterns of
            p::ps => foldl (fn (t, acc) => unify_pair(t, acc))
                           (pattern_to_type p)
                           (map pattern_to_type ps)
          | _ => NONE
    end
