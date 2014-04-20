use "pa2.sml";

val dates = [(1984, 08, 12),(1996,09,20),(2011,12,02),(2004,10,01),(1999,12,31),(9999,9999,9999),(0,0,0),(2011,12,02),(9999,12,12),(12,12,12),(2398,089,3991301)];

val months = [1,2,3,4,5,6,7,8,9,10,11,12,9999,0];
  
val strings = ["1","2","3","4","5","6","7","8","9","10"];

val fifteen_ones = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1];

val int_list = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];

(* Question #1 *)
print "\nQuestion 1\n";
is_older((1984,08,12),(1984,08,13));
is_older((~1,~1,~1),(~1,~1,~2));
is_older((0101,01,01),(0100,01,01));
is_older((1234,99,99),(1234,99,99));

(* Question #2 *)
print "\nQuestion 2\n";
number_in_month(dates, 12);
number_in_month(dates, 3);
number_in_month(dates, 0);
number_in_month(dates, ~50);

(* Question #3 *)
print "\nQuestion 3\n";
number_in_months(dates, months);
number_in_months(dates, []);
number_in_months(dates, [8]);
number_in_months([], []);

(* Question #4 *)
print "\nQuestion 4\n";
dates_in_month(dates, 8);
dates_in_month(dates, 10);
dates_in_month(dates, 0);
dates_in_month(dates, 24);
dates_in_month([], ~1);

(* Question #5 *)
print "\nQuestion 5\n";
dates_in_months(dates, months);
dates_in_months(dates, [~1,2,0,9999]);
dates_in_months([],[]);
dates_in_months(dates,[]);
dates_in_months([],months);

(* Question #6 *)
print "\nQuestion 6\n";
get_nth(strings, 10);
get_nth(strings, 1);
get_nth(["1"], 1);

(* Question #7 *)
print "\nQuestion 7\n";
date_to_string((1984,08,12));
date_to_string((1984,8,12));
date_to_string((0001,1,1));

(* Question #8 *)
print "\nQuestion 8\n";
number_before_reaching_sum(12, fifteen_ones);
number_before_reaching_sum(1, fifteen_ones);
number_before_reaching_sum(~1, fifteen_ones);
number_before_reaching_sum(28, int_list);
number_before_reaching_sum(119, int_list);

(* Question #9 *)
print "\nQuestion 9\n";
what_month(1);
what_month(365);
what_month(0);
what_month(~1);

(* Question #10 *)
print "\nQuestion 10\n";
month_range(1,365);
month_range(1,1);
month_range(28,33);
month_range(0,2);
month_range(365,365);

(* Question #11 *)
print "\nQuestion 11\n";
oldest(dates);
oldest([(1984,08,12),(1984,8,12)]);
oldest([(0,0,0),(9999,9999,9999)]);
oldest([(~1,~1,~1)]);
