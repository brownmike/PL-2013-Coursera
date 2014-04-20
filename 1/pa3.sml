(*
Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.)
*)
fun is_older (xs: (int*int*int), ys: (int*int*int)) = 
    let fun cmp (x:int, y:int)=
        if x>=y then false else true
    in
       if cmp((#1 xs),(#1 ys)) then true
       else if cmp((#2 xs),(#2 ys)) then true
       else if cmp((#3 xs),(#3 ys)) then true
       else false
    end


(*
Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month.
*)
fun number_in_month (l: (int*int*int) list, m: int)=
   let 
       fun cmp (x: int, y:int)= if x=y then 1 else 0
   in
       if null l then 0 else 
        cmp(m,(#2 (hd l))) + number_in_month (tl l, m)     
   end


(*
Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem.
*)
fun number_in_months (l: (int*int*int) list, m: int list)=
     if null l orelse null m then 0 else
     number_in_month(l, hd m) + number_in_months(l, tl m)


(*
Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given.
*)
fun dates_in_month (l: (int *int *int) list, m: int)=
    let
	fun cmp (x: int, y: int) = if x=y then true else false
    in
        if null l then [] else 
        if cmp(#2(hd l),m) then (hd l)::dates_in_month(tl l,m) else dates_in_month(tl l,m)
    end


(*
Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML's list-append operator (@).
*)
fun dates_in_months (l: (int*int*int) list, m: int list)=
    if null l orelse null m then [] else
    dates_in_month(l,hd m)@dates_in_months(l,tl m)



(*
Write a function get_nth that takes a list of strings and an int n and returns the nth element of the
list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay.
*)
fun get_nth (l: string list, n:int)=
    if null l then "n is biger than the list's size or smaller than 1 or the list is empty" else 
    if n=1 then (hd l) else get_nth(tl l, n-1)



(*
Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December.
*)
fun date_to_string(x: (int*int*int))=
    let
	val l=["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(l,(#2 x))^" "^Int.toString(#3 x)^", "^Int.toString(#1 x)
    end



(*
Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the rst n elements of the list add to less than sum, but the rst
n+1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case.
*)
fun number_before_reaching_sum (sum: int, l: int list)=
    let
	val x=(hd l)
        fun cmp (x: int, y:int) = if x<y then 1 else 0
    in
        if null l then 0 else
        if cmp(x,sum)=1 then (cmp(x,sum)+number_before_reaching_sum((sum-(hd l)),(tl l))) else 0
    end



(*
Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem.
*)    
fun what_month(x: int)=
    let
	val l=[31,28,31,30,31,30,31,31,30,31,30,31];
    in
        if x<1 then 0 else 1+number_before_reaching_sum(x,l) 
    end



(*
Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.
*)
fun month_range (x:int,y:int) =
    let
	fun cmp(x:int,y:int) = if x<=y then true else false
    in
        if x>y orelse x<1 orelse y<1 then [] else if cmp(x,y) then what_month(x)::month_range((x+1),y) else []
    end



(*
Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.
*)
fun oldest (l: (int*int*int) list) =
   if null l then NONE else
   let val temp=oldest(tl l)
   in
       if isSome temp andalso not (is_older((hd l),(valOf temp))) then
       temp else SOME(hd l)
   end



(*
(*
Challenge Problem: Write functions number_in_months_challenge and dates_in_months_challenge
that are like your solutions to problems 3 and 5 except having a month in the second argument multiple
times has no more effect than having it once. (Hint: Remove duplicates, then use previous work.)
*)
fun remove (x:int,L:int list) =
    if (L=[]) then []
    else (if (x=hd(L))
    then remove(x,tl(L))
    else hd(L)::remove(x,tl(L)));

fun removedupl (L: int list) =
    if (L=[]) then []
    else hd(L)::remove(hd(L),removedupl(tl(L)));

fun number_in_months_challenge (l: (int*int*int) list, m: int list) =
    let
        val new_m=removedupl(m)
    in
	if (null l orelse null m) then 0 else number_in_months(l,new_m)
    end

fun dates_in_months_challenge (l: (int*int*int) list, m: int list) =
    let
	val new_m = removedupl(m)
    in
	if(null l orelse null m) then [] else dates_in_months(l,new_m)
    end



(*
Challenge Problem: Write a function reasonable_date that takes a date and determines if it
describes a real date in the common era. A \real date" has a positive year (year 0 did not exist), a
month between 1 and 12, and a day appropriate for the month. Solutions should properly handle leap
years. Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100.
(Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.)
*)
fun reasonable_date (d: (int*int*int))=
    let
	fun isleap (x: int) = if (x mod 400 = 0) orelse ((x mod 4=0) andalso (x mod 100 <> 0)) then true else false
	fun leap (d: (int*int*int)) = if (#2 d < 13) then 
					  if ((#2 d = 1) orelse (#2 d = 3) orelse (#2 d = 5) orelse (#2 d = 7) 
					  orelse (#2 d = 8) orelse (#2 d = 10) orelse (#2 d = 12)) andalso (#3 d <32) then 
					      true else if ((#2 d = 4) orelse (#2 d = 6) orelse (#2 d = 9) orelse (#2 d = 11)) 
	                                      andalso (#3 d < 31) then true 
                                              else if (#2 d = 2) andalso (#3 d <30) then true else false else false 
        fun normal (d: (int*int*int))=  if (#2 d < 13) then 
					  if ((#2 d = 1) orelse (#2 d = 3) orelse (#2 d = 5) orelse (#2 d = 7) 
					  orelse (#2 d = 8) orelse (#2 d = 10) orelse (#2 d = 12)) andalso (#3 d <32) then 
					      true else if ((#2 d = 4) orelse (#2 d = 6) orelse (#2 d = 9) orelse (#2 d = 11)) 
	                                      andalso (#3 d < 31) then true 
                                              else if (#2 d = 2) andalso (#3 d <29) then true else false else false 
    in
	if (#1 d)>0 andalso (#2 d) > 0 andalso (#3 d)>0 then if isleap(#1 d) then leap(d) else normal(d)
        else false 
    end

*)
