(* datatype ('prvi, 'drugi) seznamParov = Prazen | Element of 'prvi * 'drugi * ('prvi, 'drugi) seznamParov;  
type 'a multiMnozica = ('a, int) seznamParov;

fun seznamParov (x::xs, y::ys) = 
    Element (x, y, seznamParov(xs, ys)) 
    | seznamParov _ = Prazen;

val result_seznamParov = seznamParov([1,2,3], ["a", "b", "c", "d"]);

fun foldl (f, z, s1::s) = foldl(f, f(z, s1), s)
  |foldl(_, z, []) = z

(* Not repna *)
fun foldr' (f, z, s1::s) = f(foldr'(f,z,s), s1)| foldr'(_, z, []) =z

(* Repna *)
fun foldr(f, z, s) = foldr (f, z, foldr(fn (z, si) => si::z, [], s))

fun map(f, s) = foldr(fn(z,si) => f si::z, [], s)

fun filter(f, s) = foldr(fn(z, si) => if f si then si::z else z, [], s)

fun append(xs, ys) = foldr(fn(z, x) => x::z, ys, xs);

foldl (fn (z,x) => x + 1 :: z, [], [1,2,3]);
foldr' (fn (z,x) => x + 1 :: z, [], [1,2,3]);
foldr (fn (z,x) => x + 1 :: z, [], [1,2,3]);

filter(fn x=> x mod 2 = 1, [1,2,3,4,5]);
map(fn x=> x mod 2 = 1, [1,2,3,4,5]);
append([1,2,3],[4,5,6]);


(* funkcija za izračun časa izvajanja funkcije f : unit -> 'a
fun timeIt f =
    let val timer = Timer.startCPUTimer ()
      val _ = f ()
      val dt = Time.toMilliseconds (#usr (Timer.checkCPUTimer (timer)))
    in dt end;

(* seznam s celimi števili do milijon *)
val longList = List.tabulate (100* 100, (fn i => i));

(* alternirajoča vsota stevil od 0 do milijon --> 0 - 1 + 2 - 3 + 4 - ... *)
fun f1 () = foldr' (fn (z, x) => x - z, 0, longList);
fun f2 () = foldr (fn (z, x) => x - z, 0, longList); (* repno-rekurzivna različica *)

(* rezultati izvajanja *)
val rez1 = f1 ();
val rez2 = f2 ();

(* časi izvajanja v milisekundah *)
val ms1 = timeIt f1;
val ms2 = timeIt f2; *) *)

(* Za oddajo *)
datatype natural = Succ of natural | One;
exception NotNaturalNumber;

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

fun zip(x : 'a list, y : 'b list) : ('a, 'b) list = 
    let
        fun zip_helper(x : 'a list, y : 'b list, accumulate : ('a, 'b) list) =
			accumulat (hd x, hd y)
			zip_helper(tl x, tl y)

    in
    zip_helper(x, y, [])
    end
        
val result_zip = zip([1,2,3], ["a", "b", "c", "d"])
        