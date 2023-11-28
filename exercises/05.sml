structure Rational =
struct
    type rational = int * int

    exception BadRational

    fun gcd (a : int, b : int) : int =
    if b <> 0 then
        gcd(b, a mod b)
    else 
        a
    
    fun simplify (a, b) =
        let 
            val divider = gcd (a, b)
        in
            if b = 0 then
                raise BadRational
            else
                if b < 0 then
                    (~a div divider, ~b div divider)
                else
                    (a div divider, b div divider)
        end


    fun makeRational (a, b) = 
        simplify (a, b)

    fun neg ((a, b) : rational) = simplify (~a, b)

    fun inv ((a, b) : rational) = 
        if a = 0 then
            raise BadRational
        else
            simplify (b, a)
    
    fun add ((a1, b1) : rational, (a2, b2) : rational) = 
        simplify (a1 * b2 + b1 * a2, b1 * b2)

    fun mul ((a1, b1) : rational, (a2, b2) : rational) = 
        simplify (a1 * a2, b1 * b2)

    fun toString ((a, b) : rational) = 
        let 
            val (a, b) = simplify (a, b)
        in
            if (a mod b) = 0 then
                Int.toString (a div b)
            else
                Int.toString a ^ " / " ^ Int.toString b
        end
end;
