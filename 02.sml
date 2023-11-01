datatype stopnja = As | Kralj | Kraljica | Fant | Stevilka of int
datatype barva = Kriz | Pik | Srce | Karo

type karta = stopnja * barva

(* Kakšne barve je karta? *)
fun barvaKarte ((_, b): karta) : barva =
    b

(* Ali je karta veljavna? *)
fun veljavnaKarta ((s, _) : karta) : bool =
    case s of
        Stevilka i => i <= 10 andalso i >= 2
        | (As | Kralj | Kraljica | Fant) => true
    

(* Koliko je vredna karta? *)
fun vrednostKarte ((s, _) : karta) : int =
    case s of 
        Stevilka i => i
        | As => 11
        | (Kralj | Kraljica | Fant) => 10

(* Numbers *)
datatype number = Zero | Succ of number | Pred of number

(* Simplifies unary number *)
fun simp Zero = Zero
    | simp (Pred x) =
        (case simp x of Succ y => y
        | y => Pred y)
    | simp (Succ x) =
        (case simp x of Pred y => y
        | y => Succ y)

(* Negira število a. Pretvorba v int ni dovoljena! *)
fun neg (a : number) : number =
    case a of 
        Zero => Zero
        | Succ a => Pred (neg(a))
        | Pred a => Succ (neg(a))

val neg_result = neg(Pred (Succ (Succ (Pred (Pred Zero)))));

(* Vrne vsoto števil a in b. Pretvorba v int ni dovoljena! *)
fun add (a : number, b : number) : number =
    case a of
        Zero => b
        | Succ x => add(x, Succ(b))
        | Pred x => add(x, Pred(b))

(* Vrne rezultat primerjave števil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)
fun comp (a : number, b : number) : order =
    let 
        fun compare (values) : order =
            case values of
                (Zero, Zero) => EQUAL
                | (Succ x, Succ y) => compare(x, y)
                | (Pred x, Pred y) => compare(x, y)
                | (Pred _, _) => LESS
                | (Succ _, _) => GREATER
                | (_, Pred _) => GREATER
                | (_, Succ _) => LESS
    in
        compare(simp(a), simp(b))
    end

val comp_result = comp(Pred (Succ (Succ (Pred (Pred Zero)))),  Succ (Pred (Succ (Pred Zero))))

(* Trees *)
datatype tree = Node of int * tree * tree | Leaf of int;

fun min (Leaf x) = x
| min (Node (x, l, r)) = Int.min(x, Int.min(min l, min r))

(* Vrne true, če drevo vsebuje element x. *)
fun contains (tree : tree, x : int) : bool =
    case tree of 
        Leaf i => i = x
        | Node (i, l, r) =>
            i = x 
            orelse contains(l, x)
            orelse contains(r, x)

val contains_result = contains(Node(4, Node(2, Leaf 1, Leaf 3), Leaf 5), 6)

(* Vrne število listov v drevesu. *)
fun countLeaves (tree : tree) : int =
    let
        fun counter (tree: tree, count: int) : int =
            case tree of 
                Leaf _ => count + 1 
                | Node (_, l, r) =>
                    counter(l, count) + counter(r, count)
    in
        counter(tree, 0)
    end

val countLeaves_result = countLeaves(Node(4, Node(2, Leaf 1, Leaf 3), Node(3, Leaf 6, Leaf 8)))

(* Vrne število število vej v drevesu. *)
fun countBranches (tree : tree) : int =
    let
        fun counter (tree: tree, count: int) : int =
            case tree of 
                Leaf _ => count
                | Node (_, l, r) =>
                    2 + counter(l, count) + counter(r, count)
    in
        counter(tree, 0)
    end

val countBranches_result = countBranches(Node(4, Node(2, Leaf 1, Leaf 3), Leaf 5))

(* Vrne višino drevesa. Višina lista je 1. *)
fun height (tree : tree) : int =
    let
        fun counter (tree: tree, count: int) : int =
            case tree of 
                Leaf _ => count + 1 
                | Node (_, l, r) =>
                    Int.max(counter(l, count + 1), counter(r, count + 1))
    in
        counter(tree, 0)
    end

val height_result = height(Node(4, Node(2, Leaf 1, Node(7, Leaf 3, Leaf 2)), Node(3, Leaf 6, Leaf 8)))

(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)
fun toList (tree : tree) : int list =
    case tree of 
        Leaf value => [value] 
        | Node (value, l, r) => toList(l) @ [value] @ toList(r)

val toList_result = toList(Node(7, Node(2, Leaf 8, Leaf 3), Leaf 5))

(* Vrne true, če je drevo uravnoteženo:
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
fun isBalanced (tree : tree) : bool =
    case tree of
        (Leaf _) => true 
        | Node (_, l, r) =>
            abs(height(l) - height(r)) <= 1 
            andalso isBalanced(l) 
            andalso isBalanced(r)

val isBalanced_result = isBalanced(Node(7, Node(2, Leaf 8, Node(3, Leaf 2, Leaf 4)), Leaf 5))

(* Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)
fun isBST (tree : tree) : bool =
    let
        fun check_subtree_lesser(tree: tree, node_value) =
            case tree of
                (Leaf value) => value < node_value
                | Node (value, l, r) =>
                    value < node_value 
                    andalso check_subtree_lesser(l, node_value)
                    andalso check_subtree_lesser(r, node_value)
        
        fun check_subtree_greater(tree: tree, node_value) =
            case tree of
                (Leaf value) => value > node_value
                | Node (value, l, r) =>
                    value > node_value 
                    andalso check_subtree_lesser(l, node_value)
                    andalso check_subtree_lesser(r, node_value)
    in
    case tree of
        (Leaf _) => true
        | Node (value, l, r) =>
            check_subtree_lesser(l, value) 
            andalso check_subtree_greater(r, value)
            andalso isBST(l)
            andalso isBST(r)
    end

val isBST_result = isBST(Node(7, Node(2, Leaf 8, Node(3, Leaf 2, Leaf 4)), Leaf 5))