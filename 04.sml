(* Currying examples *)
val f1 : int -> int -> int = 
    fn x => 
        fn y => 
            x * y;

fun f2 x = 
    fn y => 
        x * y;

fun f3 x y = 
    x * y;

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c =
    fn f => 
        fn x => 
            fn y => f (x, y);

fun curry2 (f : 'a * 'b -> 'c) =
    fn x => 
        fn y => 
            f (x, y);

fun curry3 f x y = 
    f (x, y);

(* Fold/reduce but with currying *)
fun fold f acc sez = 
    case sez of
        [] => acc
        | glava::rep => fold f (f glava acc) rep

val fold_result = fold (fn x => fn y => x + y) 0 [1, 2, 3, 4]

(* Fold/reduce with currying but different init*)
val reduce : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = 
    fn f => fn z => fn xs =>
        case xs of
            [] => z
            | glava::rep => reduce f (f z glava) rep

val reduce_result = reduce (fn x => fn y => x + y) 0 [1, 2, 3, 4]

(* Returns a list of squares of the numbers. Use List.map. *)
val squares : int list -> int list =
    fn list =>
        List.map (fn x => x * x) list

val squares_result = squares [1, 2, 3, 4]

(* Returns a list that contains only even numbers from xs. Use List.filter. *)
val onlyEven : int list -> int list =
    fn list =>
        List.filter (fn x => x mod 2 = 0) list

val onlyEven_result = onlyEven [1, 2, 3, 4]

val bestString : (string * string -> bool) -> string list -> string
