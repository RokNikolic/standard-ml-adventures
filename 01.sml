(*  Vrne fakulteto števila n, n >= 0. *)
fun factorial (n : int) : int =
    let 
        fun accumulate (n : int, counter : int) : int =
            if n = 0 then 
                counter
            else
                accumulate (n - 1, counter * n)
    in
        accumulate(n, 1)
    end

val factorial_result = factorial(5)

(*  Vrne n-to potenco števila x, n >= 0. *)
fun power (x : int, n : int) : int =
    if n = 1 then 
        x
    else if n = 0 then
        1
    else
        x * power(x, n-1)

val power_result = power(3, 5)

(*  Vrne največjega skupnega delitelja pozitivnih števil a in b, a >= b. *)
fun gcd (a : int, b : int) : int =
    if b <> 0 then
        gcd(b, a mod b)
    else 
        a

val gcd_result = gcd(60, 50)

(*  Vrne dolžino seznama. *)
fun len (xs: int list) : int =
    let 
        fun len_inner (list_in : int list, i : int) : int = 
                    if null list_in then
                        i 
                    else
                        len_inner (tl list_in, i + 1)
    in
        len_inner (xs, 0)
    end

val len_result = len([1,1,1,1])

(*  Vrne SOME zadnji element seznama. Če je seznam prazen vrne NONE. *)
fun last (xs : int list) : int option =
    if null xs then
        NONE
    else 
        let
            fun obrni (list_in : int list, list_temp : int list): int list =
                if null list_in then
                    list_temp
                else
                    obrni(tl list_in, hd list_in :: list_temp)
        in
            SOME (hd (obrni (xs, [])))
        end

val last_result = last([1,2,3,4])

(*  Vrne SOME n-ti element seznama. Prvi element ima indeks 0. Če indeks ni veljaven, vrne NONE. *)
fun nth (xs : int list, n : int) : int option =
    let 
        fun len (list_in : int list, i : int) : int = 
            if null list_in then
                i 
            else
                len (tl list_in, i + 1)
        fun remove_elements (list_in : int list, i : int) : int list = 
            if i = 0 then
                list_in
            else
                remove_elements (tl list_in, i - 1)
    in
        if n >= (len(xs,0)) orelse n < 0 then
            NONE
        else
            SOME (hd ( remove_elements(xs, n)))
    end

val nth_result = nth([1,2,3,4,5], 5)

(*  Vrne nov seznam, ki je tak kot vhodni, le da je na n-to mesto vrinjen element x. Prvo mesto v seznamu ima indeks 0. Indeks n je veljaven (0 <= n <= length xs). *)
fun insert (xs : int list, n : int, x : int) : int list =
    if n = 0 then 
        x :: xs
    else 
        hd xs :: insert(tl xs, n - 1, x)

val insert_result = insert([1,2,3,4], 4, 99)

(*  Vrne nov seznam, ki je tak kot vhodni, le da so vse pojavitve elementa x odstranjene. *)
fun delete (xs : int list, x : int) : int list =
    let
        fun obrni (list_in : int list, list_temp : int list): int list =
                if null list_in then
                    list_temp
                else
                    obrni(tl list_in, hd list_in :: list_temp)
        fun delete_and_reverse (list_in : int list, list_temp : int list): int list =
            if null list_in then
                list_temp
            else if hd list_in <> x then
                delete_and_reverse(tl list_in, hd list_in :: list_temp)
            else
                delete_and_reverse(tl list_in, list_temp)
    in
        obrni(delete_and_reverse(xs, []), [])
    end

val delete_result = delete([1,2,3,4,5], 3)

(*  Vrne obrnjen seznam. V pomoč si lahko spišete še funkcijo append, ki doda na konec seznama. *)
fun reverse (xs : int list) : int list =
    let
        fun obrni (list_in : int list, list_temp : int list): int list =
            if null list_in then
                list_temp
            else
                obrni(tl list_in, hd list_in :: list_temp)
    in
        obrni(xs, [])
    end

val reverse_result = reverse([1,2,3])

(*  Vrne true, če je podani seznam palindrom. Tudi prazen seznam je palindrom. *)
fun palindrome (xs : int list) : bool =
    let
        fun obrni (list_in : int list, list_temp : int list): int list =
            if null list_in then
                list_temp
            else
                obrni(tl list_in, hd list_in :: list_temp)
    in
        xs = (obrni(xs, []))
    end

val palindrom_result = palindrome([1,2,1])
