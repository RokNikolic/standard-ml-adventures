fun zip(x, y) = 
    case (x, y) of
        ([], []) => []
        | (x::xs, y::ys) => (x, y) :: zip (xs, ys)
        | _ => []
	    
val zip_result = zip([1,2,3], ["a", "b", "c", "d"])

fun unzip(x) = 
    let
        fun unzip_helper (x, first, second) =
            case (x) of
            ([]) => (first, second)
            | _ => case (hd x) of (i, s) => unzip_helper(tl x, first @ [i], second @ [s])
    in
        unzip_helper(x, [], [])
    end
	    
val unzip_result = unzip([(1,"a"),(2,"b"),(3,"c")])

(* Natural numbers *)
datatype natural = Succ of natural | One;
exception NotNaturalNumber;

fun subtract (a, b) = 
    case (a, b) of
        (One, _) => raise NotNaturalNumber
        | (Succ x , One) => x
        | (Succ x, Succ y) => subtract(x, y)

val subtract_result = subtract(Succ (Succ (Succ (Succ One))), Succ (Succ One));

(* Elementary higher order functions *)
fun any (f, s) =
    case (f, s) of
        (_, []) => false
        | (f, x::xs) => if f (x) then 
							true
						else any (f, xs)

val any_result = any ((fn x => x = 5), [3, 2, 1, 4])

fun map (f, s) =
	case (f, s) of
		(_, []) => []
		| (f, x::xs) => f (x) :: map (f, xs)

val map_result = map ((fn x => x + 1), [1, 2, 3, 4]);

fun filter (f, s) =
	let
		fun filter_helper (f, s, accumulate) =
			case (f, s) of
				(_, []) => accumulate
				| (f, x::xs) => if f (x) then 
									filter_helper (f, xs, accumulate @ [x])
								else filter_helper (f, xs, accumulate)
	in
		filter_helper(f, s, [])
	end

val filter_result = filter ((fn x => x = 1), [1, 2, 1, 4])

(* Reduce *)
fun fold (f, acc, sez) = 
    case sez of
        [] => acc
        | glava::rep => fold(f, f(glava, acc), rep)

val fold_result = fold (fn (x, y) => x + y , 0, [1, 2, 3, 4])

(* Trees...again *)
datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;