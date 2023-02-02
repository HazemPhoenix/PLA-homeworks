fun is_older(d1: int * int * int, d2: int * int * int) =
    if (365 * (#1 d1) + 31 * (#2 d1) + (#3 d1)) < (365 * (#1 d2) + 31 * (#2 d2) + (#3 d2))
	     then true
         else false

fun number_in_month(lod: (int*int*int) list, mon: int) =
    if null lod
    then 0
    else if #2 (hd lod) = mon
    then 1 + number_in_month(tl lod, mon)
    else number_in_month(tl lod, mon)

fun number_in_months(lod: (int*int*int) list, lom: int list) =
    if null lom
    then 0
    else number_in_month(lod, hd lom) + number_in_months(lod, tl lom)

fun dates_in_month(lod: (int*int*int) list, mon: int) =
    if null lod
    then []
    else if #2 (hd lod) = mon
    then hd lod :: dates_in_month(tl lod, mon)
    else dates_in_month(tl lod, mon)


fun dates_in_months(lod: (int*int*int) list, lom: int list) =
    if null lom
    then []
    else dates_in_month(lod, hd lom) @ dates_in_months(lod, tl lom)


fun get_nth(los: string list, n: int) = 
    if n = 1 
    then hd los
    else get_nth(tl los, n - 1)


fun date_to_string(d: int*int*int) =
    let 
        val m = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(m, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
    end

(*
fun number_before_reaching_sum(sum: int, loi: int list) =
    if sum <= hd loi
    then 0
    else 
        if sum - (hd loi) - hd (tl loi) <= 0 
        then 1 
        else 1 + number_before_reaching_sum(sum - hd loi, tl loi)
*)

fun number_before_reaching_sum(sum: int, loi: int list) =
    if null loi
    then 0
    else 
        if sum <= hd loi
        then 0
        else 1 + number_before_reaching_sum(sum - hd loi, tl loi)


fun what_month(d: int) =
    number_before_reaching_sum(d, [0, 31, 28, 31, 30, 31, 30, 31, 31, 31, 30, 31, 30])

fun month_range(d1: int, d2: int) =
    if d1 > d2
    then []
    else what_month(d1) :: month_range(d1 + 1, d2)

fun oldest(lod: (int*int*int) list) = 
    if null lod 
    then NONE
    else if null (tl lod)
    then SOME (hd lod)
    else 
        if is_older(hd lod, hd (tl lod)) = false
        then oldest(tl lod)
        else oldest(hd lod :: tl (tl lod))