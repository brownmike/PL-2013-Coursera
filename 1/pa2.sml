fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if #1 date1 < #1 date2
    then true
    else if #1 date1 = #1 date2 andalso #2 date1 < #2 date2
    then true
    else if #1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2
    then true
    else false
             
fun number_in_month (dates : (int * int * int) list, month : int) =
    let
        fun in_month (date : int * int * int) =
            if #2 date = month
            then 1
            else 0
    in
        if null dates
        then 0
        else in_month(hd dates) + number_in_month(tl dates, month)
    end

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month (dates, hd months) + number_in_months(dates, tl months)
        
fun dates_in_month (dates : (int * int * int) list, month : int) =
    let
        fun in_month (date : int * int * int) =
            if #2 date = month
            then SOME date
            else NONE
    in
        if null dates
        then []
        else
            let val is_in_month = in_month(hd dates)
            in
                if isSome is_in_month
                then valOf is_in_month :: dates_in_month(tl dates, month)
                else dates_in_month(tl dates, month)
            end
    end

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth (tl strings, n - 1)

fun date_to_string (date : int * int * int) =
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, nums : int list) =
    if null nums
    then 0
    else
        let
            fun number_before_reaching_sum_index (n : int, sum : int, nums : int list) =
                let val new_sum = sum - hd nums
                in
                    if new_sum <= 0
                    then n
                    else number_before_reaching_sum_index (n + 1, new_sum, tl nums)
                end
        in
            number_before_reaching_sum_index (0, sum, nums)
        end

fun what_month (day_of_year : int) =
    let
        val days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day_of_year, days_per_month) + 1
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
        let fun compare_date(date : int * int * int, dates : (int * int * int) list) =
                if null dates
                then SOME date
                else
                    if is_older(date, hd dates)
                    then compare_date(date, tl dates)
                    else compare_date(hd dates, tl dates)
        in
            compare_date(hd dates, tl dates)
        end
(*
fun dedupe (xs : int list) =
    let
        fun check_for_dupe(n : int, xs : int list) =
            if null xs
            then SOME n
            else if n = hd xs
            then NONE
            else check_for_dupe(n, tl xs)
    in
        if null xs
        then []
        else
            let
                val exists = check_for_dupe(hd xs, tl xs)
            in
                if isSome exists
                then valOf exists :: dedupe(tl xs)
                else dedupe(tl xs)
            end
    end

fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
    let
        val deduped_months = dedupe(months)
    in
        if null deduped_months
        then 0
        else number_in_month (dates, hd deduped_months) + number_in_months(dates, tl deduped_months)
    end

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
    let
        val deduped_months = dedupe(months)
    in
        if null deduped_months
        then []
        else dates_in_month(dates, hd deduped_months) @ dates_in_months(dates, tl deduped_months)
    end

fun reasonable_date (date : int * int * int) =
    let
        val year = #1 date;
        val month = #2 date;
        val day = #3 date;
        val days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

        fun is_leap_year () =
            (year mod 400 = 0) orelse ((year mod 4 = 0) andalso not (year mod 100 = 0))

        fun get_nth (xs : 'a list, n : int) =
            if n = 1
            then hd xs
            else get_nth (tl xs, n - 1)
    in
        (* years less than or equal 0, or a month less than or equal 0 *)
        (* or a month greater than 12 are not reasonable *)
        if year <= 0 orelse month <= 0 orelse month > 12 orelse day <= 0
        then false
        else if is_leap_year() andalso (month = 2)
        then if day <= 29
             then true
             else false
        else if get_nth(days_per_month, month) < day
        then false
        else true
    end
*)
