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
 *)




(**
 * Surface point at a ray-object intersection.
 *
 * All direction parameters are away from surface.
 *)




(* type --------------------------------------------------------------------- *)

type t = {
   triangle : Triangle.t ;
   position : Vector3f.t
}




(* constants ---------------------------------------------------------------- *)

let _PI = 3.141592653589793238512809




(* construction ------------------------------------------------------------- *)

(**
 * Construct a SurfacePoint instance.
 *
 * @param triangle (Triangle.t) surface's object
 * @param position (Vector3f.t) position of point on surface
 *
 * @return (SurfacePoint.t)
 *)
let create triangle position = { triangle = triangle ; position = position }




(* queries ------------------------------------------------------------------ *)

(**
 * Emission from surface element to point.
 *
 * @param sp           (SurfacePoint.t)
 * @param toPosition   (Vector3f.t) point being illuminated
 * @param outDirection (Vector3f.t) direction from emitting point
 * @param isSolidAngle (bool) use solid angle
 *
 * @return (Vector3f.t) emitted radiance
 *)
let emission sp toPosition outDirection isSolidAngle =

   let cosOut = vDot outDirection (Triangle.normal sp.triangle) in

   (* emit from front face of surface only *)
   if cosOut <= 0.0 then vZERO else

      (* estimate solid angle *)
      let solidAngle = if not isSolidAngle then 1.0 else
         let distance2 = let ray = toPosition -| sp.position in vDot ray ray in
         (* with infinity clamped-out *)
         (cosOut *. (Triangle.area sp.triangle)) /. (max distance2 1e-6) in

      (Triangle.emitivity sp.triangle) *|. solidAngle


(**
 * Light reflection from ray to ray by surface.
 *
 * @param sp           (SurfacePoint.t)
 * @param inDirection  (Vector3f.t) negative of inward ray direction
 * @param inRadiance   (Vector3f.t) inward radiance
 * @param outDirection (Vector3f.t) outward (eyeward) ray direction
 *
 * @return (Vector3f.t) reflected radiance
 *)
let reflection sp inDirection inRadiance outDirection =

   let inDot  = vDot inDirection  (Triangle.normal sp.triangle)
   and outDot = vDot outDirection (Triangle.normal sp.triangle) in

   (* directions must be on same side of surface (no transmission) *)
   if (inDot < 0.0) <> (outDot < 0.0) then vZERO else

      (* ideal diffuse BRDF:
         radiance scaled by reflectivity, cosine, and 1/pi *)
      (inRadiance *| (Triangle.reflectivity sp.triangle)) *|.
         (abs_float inDot /. _PI)


(**
 * Monte-carlo direction of reflection from surface.
 *
 * @param sp          (SurfacePoint.t)
 * @param inDirection (Vector3f.t) eyeward ray direction
 * @param random      (Random2.t)  random number generator
 *
 * @return ((Vector3f.t, Vector3f.t) option) sceneward ray direction, and light
 *                                           scaling of interaction point | none
 *)
let nextDirection sp inDirection random =

   let reflectivity     = Triangle.reflectivity sp.triangle in
   let reflectivityMean = (vDot reflectivity vONE) /. 3.0 in

   (* do russian-roulette for reflectance magnitude *)
   if (Random2.real64 random) >= reflectivityMean then None
   else
      (* cosine-weighted importance sample hemisphere *)

      (* make coord frame coefficients (z in normal direction) *)
      let x, y, z = let rand () = Random2.real64 random in
         let p2r1, sr2 = (_PI *. 2.0 *. (rand ())), (sqrt (rand ())) in
         ((cos p2r1) *. sr2), ((sin p2r1) *. sr2), (sqrt (1.0 -. (sr2 *. sr2)))

      (* make coord frame (except third) *)
      and tangent = Triangle.tangent sp.triangle
      and normal  = let n = Triangle.normal sp.triangle in
         (* put normal on inward ray side of surface
            (preventing transmission) *)
         if (vDot n inDirection) >= 0.0 then n else ~-|n in

      (* make vector from frame scaled by coefficients *)
      let outDirection =
         (tangent *|. x) +| ((vCross normal tangent) *|. y) +| (normal *|. z)
      (* make color by dividing-out mean from reflectivity *)
      and color = reflectivity /|. reflectivityMean in

      if outDirection = vZERO then None else Some (outDirection, color)


(**
 * Triangle accessor (Triangle.t).
 *)
let hitObject sp = sp.triangle

(**
 * Position accessor (Vector3f.t).
 *)
let position sp = sp.position
