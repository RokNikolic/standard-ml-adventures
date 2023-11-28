signature RATIONAL =
sig
    eqtype rational

    exception BadRational

    val makeRational: int * int -> rational

    val neg: rational -> rational

    val inv: rational -> rational

    val add: rational * rational -> rational
    val mul: rational * rational -> rational

    val toString: rational -> string
end

structure Rational :> RATIONAL =
struct
    type rational = int * int

    exception BadRational

    fun gcd (a : int, b : int) : int =
    if b <> 0 then
        gcd(b, a mod b)
    else 
        a
    
    fun simplify ((a, b) : rational): rational =
        let 
            val divider = gcd (a, b)
        in
            if b = 0 then
                raise BadRational
            else
                (a div divider, b div divider)
        end


    fun makeRational (a, b)= 
        simplify (a, b)

    fun neg ((a, b) : rational) = simplify (~a, b)

    fun inv ((a, b) : rational) = 
        if a = 0 then
            raise BadRational
        else
            simplify (b, a)
    
    fun add ((a1, b1) : rational, (a2, b2) : rational) = 
        simplify ((a1 * b2) + (a2 * b1), b1 * b2)

    fun mul ((a1, b1) : rational, (a2, b2) : rational) = 
        simplify (a1 * a2, b1 * b2)

    fun toString ((a, b) : rational) = 
        if (a mod b) = 0 then
            Int.toString (a div b)
        else
            let 
                val (x, y) = simplify (a, b)
            in
                Int.toString x ^ "/" ^ Int.toString y
            end

end

signature EQ =
sig
    type t
    val eq : t -> t -> bool
end

signature SET =
sig
    type item

    type set

    val empty : set

    val singleton : item -> set

    val union : set -> set -> set

    val difference : set -> set -> set

    val subset : set -> set -> bool
end

functor SetFn (Eq: EQ): SET =
struct
    type item = Eq.t

    type set = item list

    val empty = []

    fun singleton (a : item) = [a]

    fun union set1 set2 = 
        let
            fun add_new list accumulator =
                case list of
                    [] => accumulator
                    | head::tail => 
                        if List.exists (fn x => Eq.eq head x) accumulator then 
                            add_new tail accumulator
                        else
                            add_new tail (accumulator @ [head])
        in
            add_new set2 (add_new set1 [])
        end

    fun difference set1 set2 =
        let
            fun add_not_in_second list1 list2 =
                case list1 of
                    [] => []
                    | head::tail => 
                        if List.exists (fn x => Eq.eq head x) list2 then 
                            add_not_in_second tail list2
                        else
                            head :: add_not_in_second tail list2
        in
            add_not_in_second set1 set2
        end

    fun subset set1 set2 =
        List.all (fn first => List.exists (fn second => Eq.eq first second) set2) set1

end
