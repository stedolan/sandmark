(*------------------------------------------------------------------------------

   MiniLight OCaml : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2006-2009, 2013.

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*)




open Int32

(* uses:
 * - Unix
 *)

(* renaming is significantly slower: *)
(*let (xor, and', shl, shr, toFl) =
   Int32.( logxor, logand, shift_left, shift_right_logical, to_float )*)




(**
 * Simple, fast, good random number generator.
 *
 * Internally mutating.
 *
 * @implementation
 * 'Maximally Equidistributed Combined Tausworthe Generators'; L'Ecuyer; 1996.
 * http://www.iro.umontreal.ca/~lecuyer/myftp/papers/tausme2.ps
 * http://www.iro.umontreal.ca/~simardr/rng/lfsr113.c
 *
 * 'Conversion of High-Period Random Numbers to Floating Point'; Doornik; 2006.
 * http://www.doornik.com/research/randomdouble.pdf
 *
 * @invariants
 * - t.length = 4
 *)




(* type --------------------------------------------------------------------- *)

type t = int32 array




(* constants ---------------------------------------------------------------- *)

(* default seed and seed minimums *)
let _SEED      = 987654321l
(*let _SEED_MINS = [| 2l ; 8l ; 16l ; 128l |]*)




(* construction ------------------------------------------------------------- *)

(**
 * Construct a Random2 instance.
 *
 * @return (string, Random2.t) pair of id/seed and random
 *)
(*let create () =

   (* Unix time -- signed 32-bit, seconds since 1970 *)
   let time =
      let a = mod_float ((floor (Unix.time ())) +. 2147483648.0) 4294967296.0 in
      of_float (a +. (if a >= 0.0 then ~-.1.0 else 1.0) *. 2147483648.0) in
   (* rotate to make frequently changing bits more significant *)
   let seed = logor (shift_left time 8) (shift_right_logical time 24) in

   (* *** VERY IMPORTANT ***
      The initial seeds z1, z2, z3, z4  MUST be larger
      than 1, 7, 15, and 127 respectively. *)
   let state = Array.init 4 (fun i -> if (seed < 0l) || (seed >= _SEED_MINS.(i))
      then seed else _SEED) in
   (* make seed/id as 8 digit hex number string *)
   let id = Printf.sprintf "%08lX" state.(3) in

   ( id, state )*)

(**
 * Construct a Random2 instance.
 *
 * @return (Random2.t)
 *)
(* *** VERY IMPORTANT ***
   The initial seeds z1, z2, z3, z4  MUST be larger
   than 1, 7, 15, and 127 respectively. *)
let create () = Array.make 4 _SEED




(* queries ------------------------------------------------------------------ *)

(**
 * Random integer, 32-bit signed, >= -2^31 and <= 2^31-1.
 *
 * @param ra (Random2.t)
 *
 * @return (int32)
 *)
let int32 ra =

   ra.(0) <- logxor (shift_left (logand ra.(0) 0xFFFFFFFEl) 18)
             (shift_right_logical (logxor (shift_left ra.(0)  6) ra.(0)) 13) ;
   ra.(1) <- logxor (shift_left (logand ra.(1) 0xFFFFFFF8l)  2)
             (shift_right_logical (logxor (shift_left ra.(1)  2) ra.(1)) 27) ;
   ra.(2) <- logxor (shift_left (logand ra.(2) 0xFFFFFFF0l)  7)
             (shift_right_logical (logxor (shift_left ra.(2) 13) ra.(2)) 21) ;
   ra.(3) <- logxor (shift_left (logand ra.(3) 0xFFFFFF80l) 13)
             (shift_right_logical (logxor (shift_left ra.(3)  3) ra.(3)) 12) ;

   (logxor ra.(0) (logxor ra.(1) (logxor ra.(2) ra.(3))))


(**
 * Random real, double precision, [0,1) interval (never returns 1).
 *
 * @param ra (Random2.t)
 *
 * @return (float)
 *)
let real64 ra =

   (to_float (int32 ra)) *. (1.0 /. 4294967296.0) +. 0.5 +.
   (to_float (logand (int32 ra) 0x001FFFFFl)) *. (1.0 /. 9007199254740992.0)


(**
 * Random real, double precision, (0,1) interval (never returns 0 or 1).
 *
 * @param ra (Random2.t)
 *
 * @return (float)
 *)
(*let real64_ ra =

   (to_float (int32 ra)) *. (1.0 /. 4294967296.0) +.
   (0.5 +. (1.0  /. 4503599627370496.0) *. 0.5) +.
   (to_float (logand (int32 ra) 0x000FFFFFl)) *. (1.0 /. 4503599627370496.0)*)




(*
 * LFSR113-LEcuyer, seed: 987654321, first few int32u rands.
 * 
 * EB975594  3952563604
 * 471B9434  1192989748
 * 9078435E  2423800670
 * 49540227  1230242343
 * 2EF9F25D   788132445
 * 23C908D6   600377558
 * AE5E533A  2925417274
 * 69054221  1761952289
 * 
 * LFSR113-LEcuyer, seed: 987654321, first few real64 rands.
 * (with left-to-right evaluation of int32u calls)
 * 
 * 4.202779282028417107142104214290156960487365722656250000000000e-01
 * 6.433507023036078020794548137928359210491180419921875000000000e-02
 * 6.835013845195827553169465318205766379833221435546875000000000e-01
 * 1.811267868998279739756185335863847285509109497070312500000000e-01
 * 8.499654106161308453337710488995071500539779663085937500000000e-01
 * 8.761154864510395379184615194390062242746353149414062500000000e-01
 * 2.663372339453352610760816787660587579011917114257812500000000e-01
 * 1.962157346722648298964486457407474517822265625000000000000000e-01
 *)
