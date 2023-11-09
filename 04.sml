val _ = Control.polyEqWarn := false;

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