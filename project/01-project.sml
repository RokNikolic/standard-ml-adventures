val _ = Control.Print.printDepth := 10;
val _ = Control.Print.printLength := 10;
val _ = Control.Print.stringDepth := 2000;
val _ = Control.polyEqWarn := false;

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
    end

