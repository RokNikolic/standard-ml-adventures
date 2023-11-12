fun next (n : int) : int =
    n + 1

val next_result = next(3)

fun add (a : int, b : int) : int =
    a + b

val add_result = add(3,4)

fun majority (a : bool, b : bool, c : bool) : bool =
    if a then
        b orelse c
    else 
        b andalso c

val majority_result = majority(true, true, true)

fun median (a : real, b : real, c : real) : real =
    if Real.==(Real.max(a, b), a) then
        if Real.==(Real.max(b, c), c) then
            Real.min(a, c)
        else
            b
    else
        if Real.==(Real.max(a, c), c) then
            Real.min(b, c)
        else 
            a


val median_result = median(5.0, 1.0, 4.0)

fun triangle (a : int, b : int, c : int)  : bool =
    if (a + b > c) andalso (a + c > b) andalso (b + c > a) then
        true
    else
        false

val triangle_result = triangle(1, 2, 4)