(* vektor je za 1 manjši kot matrika ker ne pišemo prve 1*)
fun reduce v m =
    List.map (fn x :: u => Vec.sub u (Vec.scale x v) | _ => raise Empty) m

fun pivot ((v as x :: _):: m) =
    (case R.inv x of
        SOME x' => SOME (Vec.scale x' v :: m)
        | NONE =>
            case m of 
                ((u as y :: _) :: m') =>
                    let val (g, s, t) = R.xGCD (x, y) in 
                    case pivot (Vec.add (Vec.scale s v) (Vec.scale t u) :: m') of
                        SOME (u :: m'') => SOME (w :: u :: v :: m'')
                        | _ => NONE
                    end
                | _ => NONE)
    | pivot _ = NONE

fun gauss (above, []) = SOME above
    | gauss (above, below) =
        case pivot below of
            SOME ((_ :: v) :: m) => gauss (reduce v above @ [v], 
                List.filter (List.exists (fn x => x <> R.zero)) (reduce v m))
            | _ => NONE

