fun is_older(d: int * int * int, d': int * int * int) =
    let val days = ((#1 d) * 365) + ((#2 d) * 31) + (#3 d)
        val days' = ((#1 d') * 365) + ((#2 d') * 31) + (#3 d')
    in
      if days < days'
      then true
      else false
    end

fun number_in_month(xs: (int * int * int) list, n: int) =
    if null xs
    then 0
    else if (#2 (hd xs) )=n
        then 1 + number_in_month(tl xs, n)
        else number_in_month(tl xs, n)

fun number_in_months(xs: (int * int * int) list, ns: int list) =
    if null ns
    then 0
    else number_in_month(xs, hd ns) + number_in_months(xs, tl ns)

fun dates_in_month(xs: (int * int * int) list, n: int) =
    if null xs
    then []
    else if (#2 (hd xs))=n
    then hd xs :: dates_in_month(tl xs, n)
    else dates_in_month(tl xs, n)

fun dates_in_months(xs: (int * int * int) list, ns: int list) =
    if null ns
    then []
    else dates_in_month(xs, hd ns) @ dates_in_months(xs, tl ns)

fun get_nth(xs: string list, n: int) =
    if n=1
    then hd xs
    else get_nth(tl xs, n - 1)

fun date_to_string(date: (int * int * int)) =
    let val months = ["January", "February", "March", "April", "May", "June",
                      "July", "August", "September", "October", "November", "December"]
    in
      get_nth(months, (#2 date)) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum: int, xs: int list) =
    let fun iter(n: int, total: int, xs': int list) =
            if total + (hd xs') >= sum
            then n
            else iter(n + 1, total + (hd xs'), tl xs')
    in
      iter(0, 0, xs)
    end

fun what_month(day: int) =
    let val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
      number_before_reaching_sum(day, months) + 1
    end

fun month_range(d: int, d': int) =
    if d > d'
    then []
    else what_month(d) :: month_range(d + 1, d')

fun oldest(xs: (int * int * int) list) =
    let fun iter(current: (int * int * int), xs: (int * int * int) list) =
            if null xs
            then current
            else
              if is_older(current, hd xs)
              then iter(current, tl xs)
              else iter(hd xs, tl xs)
    in
      if null xs
      then NONE
      else SOME (iter(hd xs, tl xs))
    end
