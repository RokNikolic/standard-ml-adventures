fun foldl (f, z, s1::s) = foldl(f, f(z, s1), s)
  |foldl(_, z, []) = z

fun foldr(f, z, s) = foldr (f, z, foldr(fn (z, si) => si::z, [], s))

fun map(f, s) = foldr(fn(z,si) => f si::z, [], s)

fun filter(f, s) = foldr(fn(z, si) => if f si then si::z else z, [], s)

fun append(xs, ys) = foldr(fn(z, x) => x::z, ys, xs);

(*
foldl (fn (z,x) => x + 1 :: z, [], [1,2,3]);
foldr (fn (z,x) => x + 1 :: z, [], [1,2,3]);

filter(fn x=> x mod 2 = 1, [1,2,3,4,5]);
map(fn x=> x mod 2 = 1, [1,2,3,4,5]);
append([1,2,3],[4,5,6]);
*)

(* Tasks *)
datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

(* Zip 2 lists *)
fun zip(x, y) = 
    case (x, y) of
        ([], []) => []
        | (x::xs, y::ys) => (x, y) :: zip (xs, ys)
        | _ => []
	    
val zip_result = zip([1,2,3], ["a", "b", "c", "d"])

(* Unzip list *)
fun unzip(x) = 
    let
        fun unzip_helper (x, first, second) =
            case (x) of
            ([]) => (first, second)
            | _ => case (hd x) of (i, s) => unzip_helper(tl x, first@[i], second@[s])
    in
        unzip_helper(x, [], [])
    end
	    
val unzip_result = unzip([(1,"a"),(2,"b"),(3,"c")])

(* Natural numbers *)
datatype natural = Succ of natural | One;
exception NotNaturalNumber;

(* Subtract natural numbers a - b*)
fun subtract (a, b) = 
    case (a, b) of
        (One, _) => raise NotNaturalNumber
        | (Succ x , One) => x
        | (Succ x, Succ y) => subtract(x, y)

val subtract_result = subtract(Succ (Succ (Succ (Succ One))), Succ (Succ One));