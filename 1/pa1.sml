(*
Function: is_older
Description: Function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.)
*)
fun is_older(d1:int*int*int,d2:int*int*int) =
    if (#1 d1) > (#1 d2)
    then false
    else if (#1 d1) < (#1 d2)
    then true
    else if (#2 d1) > (#2 d2)
    then false
    else if (#2 d1) < (#2 d2)
    then true
    else if (#3 d1) > (#3 d2)
    then false
    else if (#3 d1) < (#3 d2)
    then true
    else false
(*
Function: number_in_month
Description:Function that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month.
*)
fun number_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else
        if (#2 (hd(dates)) = month)
        then 1 + number_in_month(tl(dates),month)
        else 0 + number_in_month(tl(dates),month)
(*
Function: number_in_months
Description: Function that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated.
*)		   
fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months 
	then 0
	else number_in_month(dates,hd(months)) + number_in_months(dates,tl(months))
(*
Function: dates_in_month
Description: Function that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given.
*)
fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else
       if (#2 (hd(dates)) = month)
       then hd(dates)::dates_in_month(tl(dates),month)
       else dates_in_month(tl(dates),month)
(*
Function: date_in_months
Description: Function that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated.
*)	  
fun dates_in_months(dates: (int*int*int) list, months: int list) =	  
    if null months
	then []
	else dates_in_month(dates,hd(months))@dates_in_months(dates,tl(months))
(*
Function: get_nth
Description: Function that takes a list of strings and an int n and returns the nth element of the
list where the head of the list is 1st . Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay.
*)
fun get_nth(lst: string list,n: int) = 
    if null lst 
	then ""
    else	   
       if n = 1
       then hd(lst)
       else get_nth(tl(lst),n-1) 	
(*
Function: date_to_string
Description: Function that takes a date and returns a string of the form January 20, 2013.
*)		  
fun date_to_string(date: int*int*int) =
    get_nth(["January","February","March","April","May","June","July","August","September","October","November","December"],#2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date) 
(*
Function: number_before_reaching_sum
Description: Function that takes an int called sum, which you can assume is positive, and an int list,
which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value.
*)
fun number_before_reaching_sum(sum: int, lst: int list) = 
let
    fun calc(x:int,idx:int,sum:int,lst:int list) = 
    let
      fun lsthd(lst:int list) = 
         if null lst
	 then 0
	 else hd(lst)
      val x = x + lsthd(lst)
      val y = idx
    in
      if null lst
      then 0
      else 
        if x < sum
	then calc(x,idx+1,sum,tl(lst))      
	else y
    end
in
   calc(0,0,sum,lst)
end
(*
Function: what_month
Description: Function that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). 
*)   
fun what_month(day:int) =
let
    val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    val month = number_before_reaching_sum(day,months)
in
    if month=0
    then 0
    else month+1
end   
(*
Function: month_range
Description: Function that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.
*)
fun month_range(day1:int,day2:int) =
    if day1 > day2
    then []
    else [what_month(day1)]@month_range(day1+1,day2)
(*
Function: oldest
Description: Function that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.
*)	
fun oldest(lst:(int*int*int) list) =
    if null lst
    then NONE
    else   
      let 
	     val tl_d = oldest(tl lst)
      in
	    if isSome tl_d andalso is_older(hd lst,valOf tl_d)
	    then tl_d
	    else SOME(hd lst)
      end	
(********** CHALLENGE PROBLEM ********)
(*
Function: is_member
Description: Function that proof is an integer is member of an integer list
*)
fun is_member(x:int, lst:int list) =
    if null lst
    then false
	else
	    if x = hd(lst)
        then true
        else is_member(x,tl(lst))   		 
(*
Function: remove_duplicates
Description: Function that remove duplicates integers from a list
*)   
fun remove_duplicates(lst2:int list) =
    if null lst2
    then []
    else 
        if is_member(hd(lst2),tl(lst2))
        then remove_duplicates(tl(lst2))
        else hd(lst2)::remove_duplicates(tl(lst2)) 	
(*
Function: number_in_months_challenge
Description: Function that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Having a month in the second argument multiple times has no more effect than having it once.
*)		
fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
     let
	val nmonths = remove_duplicates(months)	 
     in
        number_in_months(dates,nmonths)   
     end    
(*
Function: date_in_months_challenge
Description: Function that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated.
Having a month in the second argument multiple times has no more effect than having it once.
*)
fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =	  
    let
       val nmonths = remove_duplicates(months)	 
    in
       dates_in_months(dates,nmonths)   
    end    
(*
Function: reasonable_date
Description: Function that takes a date and determines if it describes a real date in the common era. 
A â€œreal dateâ€ has a positive year (year 0 did not exist), a month between 1 and 12, and a day 
appropriate for the month. 
*)
fun reasonable_date(date: int*int*int) =
let
   fun is_leap_year(year:int) =
       if (year mod 400 = 0) orelse ((year mod 4 = 0) andalso (year mod 100 <> 0))
       then true
       else false
        
   fun is_valid_month(month:int) =
       if (month >= 1) andalso (month <= 12)
       then true
       else false	 
	   
   fun is_valid_year(year:int) =   
       if year > 0 
       then true
       else false
	   
   fun is_valid_day(year:int,month:int,day:int) =
        if (day >= 1)
	    then
            if (month = 4) orelse (month = 6) orelse (month = 9) orelse (month = 11)
	    then 
	        if day > 30
                then false
                else true				
	    else		 
                if (month = 2)
		then
		    if is_leap_year(year) 
                    then 
			if day > 29
			then false
			else true
                    else		
                       if day > 28
                       then false
                       else true					   
                else 			
                    if day > 31
                    then false
                    else true					
		else false
in
   if is_valid_year(#1 date) andalso is_valid_month(#2 date) andalso is_valid_day(#1 date,#2 date,#3 date)
   then true
   else false
end    
