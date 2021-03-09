(*------------------------------------------------------------------------------

   MiniLight OCaml : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2006-2009, 2013.

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*)




open Vector3f

module Sp = SurfacePoint

(* uses:
 * - Vector3f
 * - Scene
 * - SurfacePoint
 *)




(**
 * Ray tracer for general light transport.
 *
 * Traces a path with emitter sampling: A single chain of ray-steps advances
 * from the eye into the scene with one sampling of emitters at each node.
 *)




(* type --------------------------------------------------------------------- *)

type t = { scene : Scene.t }




(* construction ------------------------------------------------------------- *)

(**
 * Construct a RayTracer instance.
 *
 * @param scene (Scene.t) collection of objects
 *
 * @return (RayTracer.t)
 *)
let create scene = { scene = scene }




(* implementation ----------------------------------------------------------- *)

(**
 * Radiance from an emitter sample.
 *
 * @param rt           (RayTracer.t)
 * @param rayDirection (Vector3f.t)     previous ray direction
 * @param surfacePoint (SurfacePoint.t) surface point receiving emission
 * @param random       (Random2.t)      random number generator
 *
 * @return (Vector3f.t) emitted radiance
 *)
let emitterSample rt rayDirection surfacePoint random =

   (* single emitter sample, ideal diffuse BRDF:
         reflected = (emitivity * solidangle) * (emitterscount) *
            (cos(emitdirection) / pi * reflectivity)
      -- SurfacePoint does the first and last parts (in separate functions) *)

   (* check an emitter was found *)
   match Scene.emitter rt.scene random with

   | Some (emitter, emitPosition) ->

      (* make direction to emit point *)
      let emitDirection =
         vUnitize (emitPosition -| (Sp.position surfacePoint)) in

      (* send shadow ray to get light *)
      let emissionIn =
         match Scene.intersection rt.scene (Sp.position surfacePoint)
            emitDirection (Some (Sp.hitObject surfacePoint)) with
         (* shadowed *)
         | Some (hitObject, _) when hitObject != emitter -> vZERO
         (* unshadowed: get inward emission value *)
         | _ -> Sp.emission (Sp.create emitter emitPosition)
            (Sp.position surfacePoint) ~-|emitDirection true in

      (* get amount reflected by surface *)
      Sp.reflection surfacePoint emitDirection
         (emissionIn *|. (float (Scene.emittersCount rt.scene))) ~-|rayDirection

   | None -> vZERO




(* queries ------------------------------------------------------------------ *)

(**
 * Radiance returned from a trace.
 *
 * @param rt           (RayTracer.t)
 * @param rayOrigin    (Vector3f.t)        ray start point
 * @param rayDirection (Vector3f.t)        ray direction
 * @param lastHit      (Triangle.t option) previous intersected object | none
 * @param random       (Random2.t)         random number generator
 *
 * @return (Vector3f.t) eye-ward radiance
 *)
let rec radiance rt rayOrigin rayDirection ?lastHit random =

   (* intersect ray with scene *)
   match Scene.intersection rt.scene rayOrigin rayDirection lastHit with

   (* a hit *)
   | Some (triangle, hitPosition) ->

      let surfacePoint = Sp.create triangle hitPosition in

      (* local emission (only for first-hit) *)
      let localEmission = if lastHit <> None then vZERO
         else Sp.emission surfacePoint rayOrigin ~-|rayDirection false

      (* emitter sample *)
      and illumination = emitterSample rt rayDirection surfacePoint random

      (* recursive reflection:
         single hemisphere sample, ideal diffuse BRDF:
            reflected = (inradiance * pi) * (cos(in) / pi * color) *
               reflectance
         -- reflectance magnitude is 'scaled' by the russian roulette,
         cos is importance sampled (both done by SurfacePoint),
         and the pi and 1/pi cancel out -- leaving just:
            inradiance * reflectance color *)
      and reflection =
         (* check surface reflects ray *)
         match Sp.nextDirection surfacePoint ~-|rayDirection random with
         | Some (nextDirection, color) ->
            (* recurse *)
            color *| (radiance rt (Sp.position surfacePoint) nextDirection
            ~lastHit:(Sp.hitObject surfacePoint) random)
         (* end path *)
         | None -> vZERO in

      (* total radiance returned *)
      reflection +| illumination +| localEmission

   | None ->

      (* no hit: default/background scene emission *)
      (Scene.defaultEmission rt.scene) ~-|rayDirection
