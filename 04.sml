(* val _ = Control.polyEqWarn := false; *)

(* Currying examples *)
val f : int -> int -> int = 
    fn x => 
        fn y => 
            x * y

fun f x = 
    fn y => 
        x * y;

fun f x y = 
    x * y;

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c =
    fn f => 
        fn x => 
            fn y => f (x, y);

fun curry (f : 'a * 'b -> 'c) =
    fn x => 
        fn y => 
            f (x, y);

fun curry f x y = 
    f (x, y);

val uncurry = fn f => fn (x, y) => f x y;
fun uncurry f = fn (x, y) => f x y;
fun uncurry f (x, y) = f x y;

val swap = fn f => fn x => fn y => f y x;
fun swap f x y = f y x;

fun foldl _ acc [] = acc
|   foldl f acc (x :: xs) = foldl f (f (x, acc)) xs;

(* My fold/reduce but with currying*)
fun fold f acc sez = 
    case sez of
        [] => acc
        | glava::rep => fold f (f glava acc) rep
        
(* Podan seznam xs agregira z začetno vrednostjo z in funkcijo f v vrednost f (f (f z s_1) s_2) s_3) ... *)
(* Aggregates xs with an initial value z and function f and returns f (f (f z s_1) s_2) s_3) ... *)
val reduce = fn f z xs =>
    case xs of
        [] => z
        | glava::rep => reduce f (f z glava) rep
