(*------------------------------------------------------------------------------

   MiniLight OCaml : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2006-2009, 2013.

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*)




(**
 * Yes, it is the 3D vector!.
 *
 * ...mostly the usual sort of stuff.
 *
 * (Unused methods are commented out. They do work fine though.)
 *
 * (array type implementation seemed same speed as record type implementation,
 * but array might be slightly smaller)
 *
 * @invariants
 * - array length = 3
 *)




(* type --------------------------------------------------------------------- *)

type t  = float array
type vt = t




(* constants ---------------------------------------------------------------- *)

let vZERO      = Array.make 3 0.0
(*let vHALF      = Array.make 3 0.5*)
let vONE       = Array.make 3 1.0
(*let vEPSILON   = Array.make 3 epsilon_float*)
(*let vALMOSTONE = Array.make 3 (1.0 -. epsilon_float)*)
(*let vMINIMUM   = Array.make 3 ~-.max_float*)
(*let vMAXIMUM   = Array.make 3 max_float*)
(*let vSMALL     = Array.make 3 fp_small*)
(*let vLARGE     = Array.make 3 fp_large*)
let vONEX      = [| 1.0 ; 0.0 ; 0.0 |]
let vONEY      = [| 0.0 ; 1.0 ; 0.0 |]
let vONEZ      = [| 0.0 ; 0.0 ; 1.0 |]




(* construction / conversion ------------------------------------------------ *)

let vCreate x y z = [| x ; y ; z |]
let vInit f       = [| f 0 ; f 1 ; f 2 |]

(*let vOfArray a    = [| a.(0) ; a.(1) ; a.(2) |]*)
(*let vToArray v    = v*)




(* queries ------------------------------------------------------------------ *)

(* access *)

(*let vX v   = v.(0)*)
(*let vY v   = v.(1)*)
(*let vZ v   = v.(2)*)
(*let vI v i = v.(i)*)


(* basic abstract operations *)

let vFold f v     = f (f v.(0) v.(1)) v.(2)
let vZip  f v0 v1 = [| f v0.(0) v1.(0) ; f v0.(1) v1.(1) ; f v0.(2) v1.(2) |]


(* arithmetic *)

(*let vSum      v =  v.(0) +. v.(1) +. v.(2)*)
(*let vAverage  v = (v.(0) +. v.(1) +. v.(2)) *. (1.0 /. 3.0)*)
(*let vSmallest v = min v.(0) (min v.(1) v.(2))*)
(*let vLargest  v = max v.(0) (max v.(1) v.(2))*)

let ( +| ) v0 v1 = [| v0.(0) +. v1.(0) ; v0.(1) +. v1.(1) ; v0.(2) +. v1.(2) |]
let ( -| ) v0 v1 = [| v0.(0) -. v1.(0) ; v0.(1) -. v1.(1) ; v0.(2) -. v1.(2) |]
let ( *| ) v0 v1 = [| v0.(0) *. v1.(0) ; v0.(1) *. v1.(1) ; v0.(2) *. v1.(2) |]
let ( /| ) v0 v1 = [| v0.(0) /. v1.(0) ; v0.(1) /. v1.(1) ; v0.(2) /. v1.(2) |]
let ( *|.) v0 f1 = [| v0.(0) *. f1 ;     v0.(1) *. f1 ;     v0.(2) *. f1     |]
let ( /|.) v0 f1 = let f1 = (1.0 /. f1) in
   [| v0.(0) *. f1 ; v0.(1) *. f1 ; v0.(2) *. f1 |]

let vDot v0 v1 = (v0.(0) *. v1.(0)) +. (v0.(1) *. v1.(1)) +. (v0.(2) *. v1.(2))
let vLength v  = sqrt (vDot v v)
(*let vDistance v0 v1 = vLength (v0 -| v1)*)

let (~-|) v = [| ~-.(v.(0)) ; ~-.(v.(1)) ; ~-.(v.(2)) |]
(*let vAbs  v = [| abs_float(v.(0)) ; abs_float(v.(1)) ; abs_float(v.(2)) |]*)

(* Zero vectors, and vectors of near zero magnitude, produce zero length,
   and (since 1 / 0 is conditioned to 0) ultimately a zero vector result.
   Vectors of extremely large magnitude produce +infinity length, and (since
   1 / inf is 0) ultimately a zero vector result.
   (Perhaps zero vectors should produce infinite results, but pragmatically,
   zeros are probably easier to handle than infinities.) *)
let vUnitize v     = if vLength v <> 0.0 then v /|. vLength v else vZERO
let vCross   v0 v1 =
   [| (v0.(1) *. v1.(2)) -. (v0.(2) *. v1.(1)) ;
      (v0.(2) *. v1.(0)) -. (v0.(0) *. v1.(2)) ;
      (v0.(0) *. v1.(1)) -. (v0.(1) *. v1.(0)) |]


(* logical *)

let vClamp    lower upper v = vZip min upper (vZip max lower v)
(*let vClampMin lower v       = vZip max lower v*)
(*let vClampMax upper v       = vZip min upper v*)
(*let vClamp01  v             = vClamp vZERO vALMOSTONE v*)

(*let vIsZero v = (v.(0) = 0.0) && (v.(1) = 0.0) && (v.(1) = 0.0)*)
(*let vCompare v0 v1 = vZip (fun a b -> float (compare a b)) v0 v1*)
(*let vSign    v     = vCompare v vZERO*)

(*let vTest f v0 v1 = vZip (fun a b -> bToF (f a b)) v0 v1*)
(*let ( =|)   v0 v1 = vTest ( =) v0 v1*)
(*let (<>|)   v0 v1 = vTest (<>) v0 v1*)
(*let ( <|)   v0 v1 = vTest ( <) v0 v1*)
(*let (<=|)   v0 v1 = vTest (<=) v0 v1*)
(*let ( >|)   v0 v1 = vTest ( >) v0 v1*)
(*let (>=|)   v0 v1 = vTest (>=) v0 v1*)




(* IO ----------------------------------------------------------------------- *)

let vRead inBuf =
   Scanf.bscanf inBuf " ( %f %f %f )" vCreate
(*let vWrite outBuf v =
   Printf.bprintf outBuf "(%g %g %g)" v.(0) v.(1) v.(2)*)
