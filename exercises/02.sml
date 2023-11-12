(* Unary Numbers *)
datatype number = Zero | Succ of number | Pred of number

(* Simplifies unary number *)
fun simp Zero = Zero
    | simp (Pred x) =
        (case simp x of Succ y => y
        | y => Pred y)
    | simp (Succ x) =
        (case simp x of Pred y => y
        | y => Succ y)


fun neg (a : number) : number =
    case a of 
        Zero => Zero
        | Succ a => Pred (neg(a))
        | Pred a => Succ (neg(a))

val neg_result = neg(Pred (Succ (Succ (Pred (Pred Zero)))));

fun add (a : number, b : number) : number =
    case a of
        Zero => b
        | Succ x => add(x, Succ(b))
        | Pred x => add(x, Pred(b))

val add_result = add(Pred Zero,  Succ (Pred Zero))

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

fun contains (tree : tree, x : int) : bool =
    case tree of 
        Leaf i => i = x
        | Node (i, l, r) =>
            i = x 
            orelse contains(l, x)
            orelse contains(r, x)

val contains_result = contains(Node(4, Node(2, Leaf 1, Leaf 3), Leaf 5), 6)

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

fun toList (tree : tree) : int list =
    case tree of 
        Leaf value => [value] 
        | Node (value, l, r) => toList(l) @ [value] @ toList(r)

val toList_result = toList(Node(7, Node(2, Leaf 8, Leaf 3), Leaf 5))

fun isBalanced (tree : tree) : bool =
    case tree of
        (Leaf _) => true 
        | Node (_, l, r) =>
            abs(height(l) - height(r)) <= 1 
            andalso isBalanced(l) 
            andalso isBalanced(r)

val isBalanced_result = isBalanced(Node(7, Node(2, Leaf 8, Node(3, Leaf 2, Leaf 4)), Leaf 5))

(* Binary search tree *)
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