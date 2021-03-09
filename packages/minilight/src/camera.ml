(*------------------------------------------------------------------------------

   MiniLight OCaml : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2006-2009, 2013.

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*)




open Vector3f

(* uses:
 * - Random2
 * - Vector3f
 * - RayTracer
 * - Image
 *)




(**
 * View definition and rasterizer.
 *
 * @invariants
 * - viewAngle is >= _VIEW_ANGLE_MIN and <= _VIEW_ANGLE_MAX degrees, in radians
 * - viewDirection is unitized
 * - right is unitized
 * - up is unitized
 * - viewDirection, right, and up form a coordinate frame
 *)




(* type --------------------------------------------------------------------- *)

type t = {
   (* eye definition *)
   viewPosition  : Vector3f.t ;
   viewAngle     : float ;

   (* view frame *)
   viewDirection : Vector3f.t ;
   right         : Vector3f.t ;
   up            : Vector3f.t
}




(* constants ---------------------------------------------------------------- *)

(**
 * View angle range, in degrees.
 *)
let _VIEW_ANGLE_MIN =  10.0
and _VIEW_ANGLE_MAX = 160.0




(* construction ------------------------------------------------------------- *)

(**
 * Construct a Camera instance.
 *
 * @param inBuffer (Scanf.Scanning.scanbuf) to read from
 *
 * @return (Camera.t)
 *)
let create (inBuffer:Scanf.Scanning.scanbuf) =

   (* read view definition in order, with conditioning *)
   let viewPosition  = vRead inBuffer in
   let viewDirection = let vd = vUnitize (vRead inBuffer) in
      (* if degenerate, default to Z *)
      if vd <> vZERO then vd else vONEZ in
   let viewAngle     = Scanf.bscanf inBuffer " %f" (fun v ->
      (max 10.0 (min v 160.0)) *. (3.141592653589793238512809 /. 180.0)) in

   (* make other directions of view coord frame *)
   let right, up =
      (* make trial 'right', using viewDirection and assuming 'up' is Y *)
      let right = vUnitize (vCross vONEY viewDirection) in
      (* check 'right' is valid
         -- i.e. viewDirection was not co-linear with 'up' *)
      if right <> vZERO then
         (* use 'right', and make 'up' properly orthogonal *)
         (right, vUnitize (vCross viewDirection right))
      else
         (* make 'up' Z if viewDirection is down, otherwise -Z *)
         let up = if viewDirection.(1) < 0.0 then vONEZ else ~-|vONEZ in
         (* remake 'right' *)
         (vUnitize (vCross up viewDirection), up) in

   {  viewPosition  = viewPosition ;
      viewAngle     = viewAngle ;

      viewDirection = viewDirection ;
      right         = right ;
      up            = up  }




(* queries ------------------------------------------------------------------ *)

(**
 * Position of the eye.
 *)
let eyePoint ca = ca.viewPosition


(**
 * Accumulate a frame of samples to the image.
 *
 * @param ca     (Camera.t)
 * @param scene  (Scene.t)   scene to render
 * @param random (Random2.t) random number generator
 * @param image  (Image.t)   image to add to
 *
 * @return ()
 *)
let frame ca scene random image =

   let rayTracer = RayTracer.create scene

   and fwidth  = float (Image.width  image)
   and fheight = float (Image.height image) in

   (* step through image pixels, sampling them *)
   for y = (Image.height image) - 1 downto 0 do
      for x = (Image.width image) - 1 downto 0 do

         (* make sample ray direction, stratified by pixels *)
         let sampleDirection =
            (* make image plane XY displacement vector [-1,+1) coefficients,
               with sub-pixel jitter *)
            let cx, cy = let module R = Random2 in
               ((((float x) +. (R.real64 random)) *. 2.0 /. fwidth ) -. 1.0,
                (((float y) +. (R.real64 random)) *. 2.0 /. fheight) -. 1.0) in
            (* make image plane offset vector *)
            let offset = (ca.right *|. cx) +|
               (ca.up *|. (cy *. (fheight /. fwidth))) in
            (* add image offset vector to view direction *)
            vUnitize (ca.viewDirection +|
               (offset *|. (tan (ca.viewAngle *. 0.5)))) in

         (* trace ray to get radiance *)
         let radiance = RayTracer.radiance rayTracer ca.viewPosition
            sampleDirection random in

         (* add radiance to pixel *)
         Image.addToPixel' image x y radiance

      done
   done
