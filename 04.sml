(* Currying examples *)
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

(* Returns the largest string according to alphabetical ordering. *)
val largestString : string list -> string =
    fn string_list =>
        bestString (fn (x, y) => x > y) string_list

val largestString_result = largestString ["t", "te", "test", "tes"]

val longestString : string list -> string =
    fn string_list =>
        bestString (fn (x, y) => String.size x > String.size y) string_list

val longestString_result = longestString ["t", "tessssss", "test", "tes"]

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

(* Dot product using List.foldl and ListPair.map. *)
val dot : int list -> int list -> int =
    fn list1 => fn list2 =>
        List.foldl (fn (x, acc) => x + acc) 0 (ListPair.map (fn (x, y) => x * y) (list1, list2))

val dot_result = dot [2, 3, 4, 5] [1, 6, 9, 3]

(* Transpose of matrix *)
val transpose : 'a list list -> 'a list list =
    fn list =>
        let
            fun transpose_counter list_in counter max =
                if counter = max then
                    []
                else
                    List.map (fn x => List.nth(x, counter)) list_in :: (transpose_counter list_in (counter + 1) max)
        in
        transpose_counter list 0 (List.length list)
        end

val transpose_result = transpose [[1,2,3],[4,5,6],[7,8,9]]

(* Multiplies two matrices. Uses dot and transpose. *)
val multiply : int list list -> int list list -> int list list =
    fn matrix1 => fn matrix2 =>
        let 
            val matrix2_transposed = transpose matrix2
        in
            List.map (fn rows => List.map (fn columns => dot rows columns) matrix2_transposed) matrix1
        end

val multiply_result = multiply [[1,2,3],[4,5,6],[7,8,9]] [[4,3,5],[6,3,2],[2,3,1]]
