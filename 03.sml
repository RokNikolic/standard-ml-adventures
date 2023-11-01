datatype ('prvi, 'drugi) seznamParov = Prazen | Element of 'prvi * 'drugi * ('prvi, 'drugi) seznamParov;  
type 'a multiMnozica = ('a, int) seznamParov;

fun seznamParov (x::xs, y::ys) = 
    Element (x, y, seznamParov(xs, ys)) 
    | seznamParov _ = Prazen;

val result_seznamParov = seznamParov([1,2,3], ["a", "b", "c", "d"]);

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

(* Za oddajo *)
datatype natural = Succ of natural | One;
exception NotNaturalNumber;

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

(*Zip 2 lists*)
fun zip(x, y) = 
    case (x, y) of
        ([], []) => []
        | (x::xs, y::ys) => (x, y) :: zip (xs, ys)
        | _ => []
	    
val result_zip = zip([1,2,3], ["a", "b", "c", "d"])

(*Unzip list*)
fun unzip(x) = 
    case x of
        [] => []
        | x::xs => [x[0]::unzip xs, x[1]:: unzip xs]
        | _ => []
	    
val result_unzip = unzip([(1,"a"),(2,"b"),(3,"c")])
        