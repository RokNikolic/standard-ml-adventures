signature RATIONAL =
sig
    (* Definirajte podatkovni tip rational, ki podpira preverjanje enakosti. *)
    eqtype rational

    (* Definirajte izjemo, ki se kliče pri delu z neveljavnimi ulomki - deljenju z nič. *)
    exception BadRational

    (* Vrne racionalno število, ki je rezultat deljenja dveh podanih celih števil. *)
    val makeRational: int * int -> rational

    (* Vrne nasprotno vrednost podanega števila. *)
    val neg: rational -> rational

    (* Vrne obratno vrednost podanega števila. *)
    val inv: rational -> rational

    (* Funkcije za seštevanje in množenje. Rezultat vsake operacije naj sledi postavljenim pravilom. *)
    val add: rational * rational -> rational
    val mul: rational * rational -> rational

    (* Vrne niz, ki ustreza podanemu številu.
        Če je število celo, naj vrne niz oblike "x" oz. "~x".
        Če je število ulomek, naj vrne niz oblike "x/y" oz. "~x/y". *)
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

    fun makeRational (a, b) = 
        let 
            val divider = gcd (a, b)
        in
            if b = 0 orelse a = b then
                raise BadRational
            else
                (a div divider, b div divider)
        end

    fun neg ((a, b) : rational) = (~a, b)

    fun inv ((a, b) : rational) = 
        if a = 0 then
            raise BadRational
        else
            (b, a)
    
    fun add ((a1, b1) : rational, (a2, b2) : rational) = (a1 * b2 + b1 * a2, b1 * b2)

    fun mul ((a1, b1) : rational, (a2, b2) : rational) = (a1 * a2, b1 * b2)

    fun toString ((a, b) : rational) = 
        if (a mod b) = 0 then
            Int.toString (a div b)
        else
            Int.toString a ^ " / " ^ Int.toString b
end;
