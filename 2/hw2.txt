(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* Problem 1a. Ugly but it works. *)
fun all_except_option (str, str_list) =
    let fun multi_lst (str, str_list, acc) =
	    case str_list of
		x::[] => if same_string(x,str) then SOME acc else NONE
	      | x::xs' => if same_string(x,str)
			  then SOME (acc @ xs')
			  else multi_lst(str, xs', (acc @ [x]))
    in
	case str_list of
	    [] => NONE
	  | x::[] => if same_string(x,str) then SOME [] else NONE
	  | x::xs' => if same_string(x,str) then SOME xs' else multi_lst(str,xs',[x])
    end 

(* Problem 1b. *)
fun get_substitutions1 (str_list_list, str) =
    case str_list_list of
	[] => []
      | xs'::ys' => case all_except_option(str, xs') of
			  NONE => get_substitutions1(ys',str)
			| SOME l => l @ (get_substitutions1(ys',str))

(* Problem 1c. Let fun begin. *)
fun get_substitutions2 (str_list_list, str) =
    let fun begin (str_list_list, str, acc) =
	    case str_list_list of
		[] => acc
	      | xs'::ys' => case all_except_option(str, xs') of
				NONE => begin(ys',str,acc)
			      | SOME l => (begin(ys',str,acc@l))
    in begin(str_list_list, str, [])
    end

(* Problem 1d. *)
fun similar_names (str_list_list, {first=f,middle=m,last=l}) =
    let	fun return_records (sub_list, m, l, acc) =
	    case sub_list of
		[] => acc
	      | x::xs' => return_records(xs', m, l, (acc @ [{first=x,middle=m,last=l}]))
    in
	return_records(get_substitutions2(str_list_list,f), m, l, [{first=f,middle=m,last=l}])
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* Problem 2a. *)
fun card_color card =
    case card of
	(s,_) => if (s = Clubs orelse s = Spades) then Black else Red

(* Problem 2b. *)
fun card_value card =
    case card of
	(_,Num(n)) => n
      | (_,Ace) => 11
      | (_,_) => 10

(* Problem 2c. *)
fun remove_card (cs, c, e) =
    let fun start (cs, c, e, acc) =
	    case cs of
		[] => raise e
	      | x::[] => if x = c then acc else raise e
	      | x::xs' => if x = c then (acc @ xs') else start(xs', c, e, (acc @ [x]))
    in start(cs, c, e, [])
    end

(* Problem 2d. *)
fun all_same_color cs =
    case cs of
	[] => true
      | x::[] => true
      | head::(neck::rest) => (card_color(head) = card_color(neck) andalso all_same_color(neck::rest))

(* Problem 2e. *)
fun sum_cards cs =
    let fun activate (cs, acc) =
	    case cs of
		[] => acc
	      | x::xs' => activate(xs', card_value(x) + acc)
    in activate(cs, 0)
    end

(* Problem 2f. *)
fun score (cs, g) =
    let val sum = sum_cards(cs)
	fun pre_score (sum, g) =
	    if sum > g
	    then 3 * (sum - g)
	    else g - sum
    in
	if all_same_color(cs)
	then pre_score(sum,g) div 2
	else pre_score(sum,g)
    end

(* Problem 2g. *)
fun officiate (cards, moves, goal) =
    let	fun commence_game (cards, moves, goal, held_cards) =
	    if sum_cards(held_cards) > goal
	    then score(held_cards, goal)
	    else case moves of
		     [] => score(held_cards, goal)
		   | (Discard c)::ms' => commence_game(cards, ms', goal, remove_card(held_cards, c, IllegalMove))
		   | Draw::ms' => case cards of
				      [] => score(held_cards,goal)
				    | c::cs' => commence_game(cs', ms', goal, c::held_cards)
    in
	commence_game(cards, moves, goal, [])
    end
