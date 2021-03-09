(*------------------------------------------------------------------------------

   MiniLight OCaml : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2006-2009, 2013.

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*)




open Vector3f

(* uses:
 * - Random2
 * - Vector3f
 * - Triangle
 * - SpatialIndex
 *)




(**
 * Collection of objects in the environment.
 *
 * Makes a sub-collection of emitting objects.
 *
 * @invariants
 * - triangles length <= _MAX_TRIANGLES
 * - emitters length  <= _MAX_TRIANGLES
 * - skyEmission      >= 0
 * - groundReflection >= 0 and <= 1
 *)




(* type --------------------------------------------------------------------- *)

type t = {
   (* objects *)
   triangles : Triangle.t array ;
   emitters  : Triangle.t array ;
   index     : SpatialIndex.t ;

   (* background *)
   skyEmission      : Vector3f.t ;
   groundReflection : Vector3f.t
}




(* constants ---------------------------------------------------------------- *)

(**
 * Maximum number of objects in Scene.
 * (2^24 ~= 16 million)
 *)
let _MAX_TRIANGLES = 0x1000000




(* construction ------------------------------------------------------------- *)

(**
 * Construct a Scene instance.
 *
 * @param inBuffer    (Scanf.Scanning.scanbuf) to read from
 * @param eyePosition (Vector3f.t) eye position
 *
 * @return (Scene.t)
 *)
let create (inBuffer:Scanf.Scanning.scanbuf) eyePosition =

   (* read default scene background in order *)
   let skyEmission      = vRead inBuffer in
   let groundReflection = vRead inBuffer in

   (* read triangles in order *)
   let triangles =
      let rec readTriangles ts i =
         try if i = 0 then ts
            else readTriangles ((Triangle.create inBuffer) :: ts) (i - 1)
         (* EOF is not really exceptional here, but the code is simpler *)
         with | End_of_file -> ts in
      (* until maximum or EOF *)
      readTriangles [] _MAX_TRIANGLES in

   (* find emitting triangles *)
   let emitters =
      (* non-zero emission and area *)
      let isEmitter t = ((Triangle.emitivity t) <> vZERO) &&
         ((Triangle.area t) > 0.0) in
      List.filter isEmitter triangles in

   {  triangles = Array.of_list triangles ;
      emitters  = Array.of_list emitters ;
      index     = SpatialIndex.create eyePosition triangles ;

      skyEmission      = vClamp vZERO skyEmission skyEmission ;
      groundReflection = vClamp vZERO vONE        groundReflection  }




(* queries ------------------------------------------------------------------ *)

(**
 * Nearest intersection of ray with object.
 *
 * @param sc           (Scene.t)
 * @param rayOrigin    (Vector3f.t) ray origin
 * @param rayDirection (Vector3f.t) ray direction
 * @param lastHit      (Triangle.t option) previous intersected object
 *
 * @return ((Triangle.t, Vector3f.t) option) hit object, and hit position
 *)
let intersection sc rayOrigin rayDirection lastHit =

   SpatialIndex.intersection sc.index rayOrigin rayDirection lastHit


(**
 * Number of emitters in scene.
 *
 * @param sc (Scene.t)
 *
 * @return (int) number of emitters
 *)
let emittersCount sc = Array.length sc.emitters


(**
 * Monte-carlo sample point on monte-carlo selected emitting object.
 *
 * @param sc     (Scene.t)
 * @param random (Random2.t) random number generator
 *
 * @return ((Triangle.t, Vector3f.t) option) object, and point on object | none
 *)
let emitter sc random =

   if (emittersCount sc) <= 0 then None else
      (* select emitter *)
      let emitter = sc.emitters.( min ((emittersCount sc) - 1) (truncate
         ((Random2.real64 random) *. (float_of_int (emittersCount sc)))) ) in
      (* get position on triangle *)
      Some (emitter, Triangle.samplePoint emitter random)


(**
 * Default/'background' light of scene universe.
 *
 * @param sc            (Scene.t)
 * @param backDirection (Vector3f.t) direction to eye
 *
 * @return (Vector3f.t) emitted radiance
 *)
let defaultEmission sc backDirection =

   (* sky for downward ray, ground for upward ray *)
   if (vDot backDirection vONEY) < 0.0
      then sc.skyEmission else sc.skyEmission *| sc.groundReflection
