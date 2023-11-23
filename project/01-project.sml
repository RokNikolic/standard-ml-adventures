val _ = Control.Print.printDepth := 10;
val _ = Control.Print.printLength := 10;
val _ = Control.Print.stringDepth := 2000;
val _ = Control.polyEqWarn := false;

val alphabet = "\n !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";

fun split blockSize list = 
    let 
        fun split_helper inner_list accumulated_list count =
            if count < blockSize then
                case inner_list of
                    [] => []
                    | head::tail => 
                        split_helper tail (accumulated_list @ [head]) (count + 1)
            else 
                accumulated_list :: split_helper inner_list [] 0
    in
        split_helper list [] 0
    end;

fun xGCD (a, b) = 
    let 
        fun extended_gcd (old_r, r, old_s, s, old_t, t) =
            if r <> 0 then
                let 
                    val quotient = old_r div r
                in
                    extended_gcd(r, old_r - quotient * r, s, old_s - quotient * s, t, old_t - quotient * t)
                end
            else 
                (old_r, old_s, old_t)
    in
        extended_gcd (a, b, 1, 0, 0, 1)
    end;

signature RING =
sig
    eqtype t
    val zero : t
    val one : t
    val neg : t -> t
    val xGCD : t * t -> t * t * t
    val inv : t -> t option
    val + : t * t -> t
    val * : t * t -> t
end;

functor Ring (val n : int) :> RING where type t = int =
struct
    type t = int
    val zero = 0
    val one = 1
    fun neg x = ~x mod n
    val xGCD = xGCD
    
    fun inv x =
        case xGCD (x mod n, n) of
        (1, s, _) => SOME (s mod n)
        | _ => NONE

    fun op + a =  Int.+ a mod n
    fun op * p =  Int.* p mod n
end;

signature MAT =
sig
  eqtype t
  structure Vec :
    sig
        val dot : t list -> t list -> t
        val add : t list -> t list -> t list
        val sub : t list -> t list -> t list
        val scale : t -> t list -> t list
    end
  val tr : t list list -> t list list
  val mul : t list list -> t list list -> t list list
  val id : int -> t list list
  val join : t list list -> t list list -> t list list
  val inv : t list list -> t list list option
end;

functor Mat (R : RING) :> MAT where type t = R.t =
struct
  type t = R.t
  structure Vec =
    struct
        fun dot list1 list2 = List.foldl (fn (x, acc) => R.+(x, acc)) R.zero (ListPair.map (fn (x, y) => R.*(x, y)) (list1, list2))
        fun add list1 list2 = ListPair.map R.+ (list1, list2)
        fun sub list1 list2 = ListPair.map (fn (x, y) => R.+(x, (R.neg y))) (list1, list2)
        fun scale scalar list1 = List.map (fn x => R.*(x, scalar)) list1
    end;

    fun tr matrix =
        case matrix of 
            [] => []
            | _ => case hd matrix of
                [] => []
                | _ => List.map (fn row => hd row) matrix :: tr (List.map (fn row => tl row) matrix)

    fun mul matrix1 matrix2 = List.map (fn rows => List.map (fn columns => Vec.dot rows columns) (tr matrix2)) matrix1;

    fun id size = 
        let
            fun row i = 
                List.tabulate (size, fn j => if i = j then R.one else R.zero);
        in
            List.tabulate (size, row)
        end;

    fun join matrix1 matrix2 = 
        case matrix1 of
            [] => matrix2
            | _ => case matrix2 of 
                [] => matrix1
                | _ => ListPair.map (fn (x, y) => x @ y) (matrix1, matrix2)

    fun reduce v m = List.map (fn (x :: u) => Vec.sub u (Vec.scale x v) | _ => raise Empty) m

    fun pivot ((v as x :: _) :: m) =
        (case R.inv x of
        SOME x1 => SOME (Vec.scale x1 v :: m)
        | NONE =>
            case m of
                ((u as y :: _) :: m2) =>
                    let val (g, s, t) = R.xGCD (x, y) in
                        case pivot (Vec.add (Vec.scale s v) (Vec.scale t u) :: m2) of
                            SOME (w :: m3) => SOME (w :: u :: v :: m3)
                        | _ => NONE
                    end    
                | _ => NONE)
        | pivot _ = NONE

    fun gauss (above, []) = SOME above
    |   gauss (above, below) =
            case pivot below of
                SOME ((_ :: v) :: m) => gauss (reduce v above @ [v], List.filter (fn z => List.exists (fn x => x <> R.zero) z) (reduce v m))
            |   _ => NONE

    fun inv matrix = gauss ([], (join matrix (id (List.length matrix))))

end;

fun get_elements number list =
    if number = 0 then
        []
    else 
        case list of 
            [] => []
            | head::tail => head :: get_elements (number - 1) tail

signature CIPHER =
sig
  type t
  val encrypt : t list list -> t list -> t list
  val decrypt : t list list -> t list -> t list option
  val knownPlaintextAttack : int -> t list -> t list -> t list list option
end;

functor HillCipherAnalyzer (M : MAT) :> CIPHER where type t = M.t =
struct
    type t = M.t
    
    fun encrypt key plaintext =
        let
            val split_text = split (List.length key) plaintext
        in
            List.concat (List.map (fn portion => hd (M.mul [portion] key)) split_text)
        end
                
    fun decrypt key ciphertext = 
        let
            val split_text = split (List.length key) ciphertext
            val inversed_key = M.inv key
        in
            case inversed_key of
                NONE => NONE
                | SOME i_key => SOME (List.concat (List.map (fn portion => hd (M.mul [portion] i_key)) split_text))
        end

    fun knownPlaintextAttack keyLenght plaintext ciphertext = 
        let
            val split_plain = split (keyLenght) plaintext
            val split_cipher = split (keyLenght) ciphertext

            fun knownPlaintextAttack_helper count =
                let
                    val x = get_elements (keyLenght + count) split_plain
                    val y = get_elements (keyLenght + count) split_cipher
                    val x2 = M.mul (M.tr x) x
                    val y2 = M.mul (M.tr x) y
                in  
                    if (keyLenght + count) > (List.length split_cipher) then
                        NONE
                    else
                        if count = 0 then
                            case M.inv x of
                                NONE => knownPlaintextAttack_helper (count + 1)
                                | SOME inv_of_x => SOME (M.mul inv_of_x y)
                        else
                            case M.inv x2 of
                                NONE => knownPlaintextAttack_helper (count + 1)
                                | SOME inv_of_x2 => SOME (M.mul inv_of_x2 y2)
                end
        in
            let
                val found_key = knownPlaintextAttack_helper 0
            in
                case found_key of
                    NONE => NONE
                    | SOME key => 
                        if (encrypt key plaintext) = ciphertext then
                            SOME key
                        else
                            NONE
            end
        end

end;

structure Trie :> 
sig
eqtype ''a dict
val empty : ''a dict
val insert : ''a list -> ''a dict -> ''a dict
val lookup : ''a list -> ''a dict -> bool
end
=
struct
  datatype ''a tree = N of ''a * bool * ''a tree list
  type ''a dict = ''a tree list

  val empty = [] : ''a dict

  fun insert w dict = raise NotImplemented
  fun lookup w dict = raise NotImplemented
end;