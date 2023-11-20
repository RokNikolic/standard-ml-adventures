(* usefull for making unit tests *)
fun readFile filename =
  let val is = TextIO.openIn filename
  in 
    String.map (fn c => if Char.isGraph c orelse c = #" " orelse c = #"\n" then c else #" ")
      (TextIO.inputAll is)
    before TextIO.closeIn is
  end

val y = "Y,}*a@F^d'=1$e:g!n&x:L}U9(y8on+x+a:N2ym0 mQn+o#a)aaO4l+<Y,}*a@F^P#q]/lJj.\"oE./].Ff!HS=af~|sakH#jRWVJ835\\UY5sQtW+\n,2s{]MNFbpN\nf\\qZ5<YMfBKA9g\\$q.4lx*K(i{R=?LCo`+[:LBKv91W>Wu[0CT}.{%c[Wk-1L<\"R3M<IU5\ngIoK|]b4)0?<7^:zBA@&TM0m$^iV1TmhC2]0 bohxX.LS?4MTZ#m.V4XQ0jLE|8Zxg\\*?eow2O\n*3p}T(_3^|Jo6WsL<~`5Uh/G?\"ESG,KZ)s~)}XySr7L!q5i_{yX?-d#BfD rG8SyLoH2d]u'O=\";\"Sj0 $!IuT9Zv4g{B%x%N5C=)3#(yE,X?HUj(i{>1y'%m<LO{TF?kqiVqX(2ai?\\mluZ|%zSo(<`9AD>ABDX/p!lRnXBnZK{TCl~v%r+s]}Wu:dUdPRh$t=1\"cYW5ig-r#Vj--51re8}LT&\\$6;qbax9E%=#$HR.>7\\ IJ_K%HW +8SzW)'w;ax9E%=#$.WB$^^JxVcVsC3YB7\nduuKCQSJs<L*qaZSpVMRsv%HW3IU;N]ek[c#9zj,lpLa-G<Yv<hxY])>3!DQ,p~da\"]@[*rClxy19`B,gUfZZ?=8<,&vw)ke,l>dDCyEt\nLwLBQ0LY)f}30)x3L<\n#=Iv,GgZ^n\\11&rg?X~#D]5QtMW*E/';E@Q[F%^;[Nj+Yu6~rrwl^ /vmgdVp2Tsge~8@#'dbax9E%=#$.f6}3zXFf:~i)wX?x:uUm1v\n*#>UM<otbzi:4\\V'ax9E%=#$ClG-Bg*j5O^\\jpqjnmA]no=y^!K]gXk?i'Ble\\,:tZJ6+39bZ4Wh1V}rO *n#aG_wKv!V#,mv^qf?2F)ax9E%=#$v;jbTd4se$1k+rog ~JC\nTvuw27OO$t/n\\11&rg?X~#D]5Qts{D%z5l'N B'<c->=\";\"Sj0 nQ<4,k4Aax9E%=#$yf$<[+wW|g'NmHUr.f6}3zXFS1a||v!.N/<e*qBPkcN(\\t\n/@sQ\n{^wIkS3{l\n_P<<2>?X'.ax9E%=#$dL=r:K@DnKfEmP+,7`f*+1w3_2hH\n^k)<<2>?X'.fwI/Y.m.%HW3IU;NgFJ3ZA$$s'^. LB=nFCS.a)j)>3!DQ,p]kGLt[:Pqx6pJ|!ffiP>FWeY!MT>0yi+>\"H;I;$7{U&C+JCTT<6Hadp^~X>G8C&awBM+6#1t7L!q5i_{yX?-d#BfH?tXKK0C.L2RvFm+u#sl=h'U'o#C\\^~UJorpHsZ_n{X;cApU_{b9#]1z2DYGa9.J~(L%RAJ+i,5b0PWj2krZnQ/s++ +r~+Q_?H}5dQjq\"u?2)\nL6#-2BA8e}RuxuTp|5=|p*:J;0fnOh=}-Af4T7bVw5H*Gv~8HC$%sHeP2{tHYoa^]+J8,s7Kx-Z/T6z0P&b)f5X1KR{N8}^h.yx-9=mS`o`^HMgz)3e/(TFtKsI>$G\\]8X&geCd/.-X+g!U+Z-/.l%Wt.Qp&(K;MgIqhh[A-x%+o`CeA8OHO1thHCBQiP/SpPd1-z{V~^[{'SMVJmy^P|'{gWXg/Fw_rD}<E?5[D6aqN}\"`3w]hcBgtUy.Kfe-pklHHbe8h<Z7hR`=qP.k|_sWU!;Ca\\`G[L\\Q+}^Aa|w+rZ@p)eyk[P,!Tk@?6C;z;~,uAV+@(!L_J8VVI$!\"3N5-4w$HHbe8h<ZNw7A67{)Z1^(?@%uyEB7\"YvkXI`A e!1%+o`CeA8trfw}!Za,?2g!'5CS=NT7uCRvVf24,[RcXxIr ^6"

val alphabet = "\n !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"



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
      fun dot _ _ = raise NotImplemented
      fun add _ _ = raise NotImplemented
      fun sub _ _ = raise NotImplemented
      fun scale _ _ = raise NotImplemented
    end

  fun tr _ = raise NotImplemented
  fun mul _ _ = raise NotImplemented
  fun id _ = raise NotImplemented
  fun join _ _ = raise NotImplemented
  fun inv _ = raise NotImplemented
end;

signature CIPHER =
sig
  type t
  val encrypt : t list list -> t list -> t list
  val decrypt : t list list -> t list -> t list option
  val knownPlaintextAttack : int -> t list -> t list -> t list list option
end;

functor HillCipherAnalyzer (M : MAT) :> CIPHER
  where type t = M.t
=
struct
  type t = M.t
  
  fun encrypt key plaintext = raise NotImplemented
  fun decrypt key ciphertext = raise NotImplemented
  fun knownPlaintextAttack keyLenght plaintext ciphertext = raise NotImplemented
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

signature HILLCIPHER =
sig
  structure Ring : RING where type t = int
  structure Matrix : MAT where type t = Ring.t
  structure Cipher : CIPHER where type t = Matrix.t
  val alphabetSize : int
  val alphabet : char list
  val encode : string -> Cipher.t list
  val decode : Cipher.t list -> string
  val encrypt : Cipher.t list list -> string -> string
  val decrypt : Cipher.t list list -> string -> string option
  val knownPlaintextAttack :
      int -> string -> string -> Cipher.t list list option
  val ciphertextOnlyAttack : int -> string -> Cipher.t list list option
end

functor HillCipher (val alphabet : string) :> HILLCIPHER =
struct

(*printable characters*)
val alphabetSize = String.size alphabet
val alphabet = String.explode alphabet

structure Ring = Ring (val n = alphabetSize)
structure Matrix = Mat (Ring)
structure Cipher = HillCipherAnalyzer (Matrix)

fun encode txt = raise NotImplemented
fun decode code = raise NotImplemented

local
  fun parseWords filename =
    let val is = TextIO.openIn filename
      fun read_lines is =
        case TextIO.inputLine is of
          SOME line =>
            if String.size line > 1
            then String.tokens (not o Char.isAlpha) line @ read_lines is
            else read_lines is
          | NONE => []
    in List.map (String.map Char.toLower) (read_lines is) before TextIO.closeIn is end

  val dictionary = List.foldl (fn (w, d) => Trie.insert w d) Trie.empty (List.map String.explode (parseWords "hamlet.txt")) handle NotImplemented => Trie.empty
in
  fun encrypt key plaintext = raise NotImplemented
  fun decrypt key ciphertext = raise NotImplemented
  fun knownPlaintextAttack keyLenght plaintext ciphertext = raise NotImplemented
  fun ciphertextOnlyAttack keyLenght ciphertext = raise NotImplemented
  end
end;
