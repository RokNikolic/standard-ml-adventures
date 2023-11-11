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
val rec reduce : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = 
    fn f => fn z => fn xs =>
        case xs of
            [] => z
            | glava::rep => reduce f (f z glava) rep

val reduce_result = reduce (fn x => fn y => x + y) 0 [1, 2, 3, 4]

val squares : int list -> int list =
    fn list =>
        List.map (fn x => x * x) list

val squares_result = squares [1, 2, 3, 4]

val onlyEven : int list -> int list =
    fn list =>
        List.filter (fn x => x mod 2 = 0) list

val onlyEven_result = onlyEven [1, 2, 3, 4]

(* Returns the best string according to the function f *)
val bestString : (string * string -> bool) -> string list -> string =
    fn f => fn string_list => 
        List.foldl (fn (x, acc) => if f (x, acc) then x else acc) "" string_list

val bestString_result = bestString (fn (x, y) => x > y) ["t", "te", "test", "tes"]

(* Vrne leksikografsko največji niz. Uporabite bestString. *)
(* Returns the largest string according to alphabetical ordering. Use bestString. *)
val largestString : string list -> string =
    fn string_list =>
        bestString (fn (x, y) => x > y) string_list

val largestString_result = largestString ["t", "te", "test", "tes"]

val longestString : string list -> string =
    fn string_list =>
        bestString (fn (x, y) => x > y) string_list

val longestString_result = longestString ["t", "te", "test", "tes"]

val rec quicksort : ('a * 'a -> order) -> 'a list -> 'a list =
    fn f => fn array =>
        case array of
            [] => []
            | glava::rep => 
                let 
                    val lesser = List.filter (fn x => (f (x, glava) = LESS)) rep
                    val equal = List.filter (fn x => (f (x, glava) = EQUAL)) rep
                    val greater = List.filter (fn x => (f (x, glava) = GREATER)) rep
                in
                    quicksort f lesser @ equal @ [glava] @ quicksort f greater
                end

val quicksort_result = quicksort (Int.compare) [1, 6, 9, 3, 6, 2, 12, 15, 3, 5]

val rec better_quicksort : ('a * 'a -> order) -> 'a list -> 'a list =
    fn f => fn array =>
        case array of
            [] => []
            | array => 
                let 
                    val pivot = List.last(array)
                    val lesser = List.filter (fn x => (f (x, pivot) = LESS )) array
                    val equal = List.filter (fn x => (f (x, pivot) = EQUAL)) array
                    val greater = List.filter (fn x => (f (x, pivot) = GREATER)) array
                in
                    better_quicksort f lesser @ equal @ [pivot] @ better_quicksort f greater
                end

val better_quicksort_result = better_quicksort (Int.compare) [1, 6, 9, 3, 6, 2, 12, 15, 3, 5]

