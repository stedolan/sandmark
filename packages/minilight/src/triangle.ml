(*------------------------------------------------------------------------------

   MiniLight OCaml : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2006-2009, 2013.

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*)




open Vector3f

(* uses:
 * - Random2
 * - Vector3f
 *)




(**
 * Simple, explicit/non-vertex-shared triangle.
 *
 * Includes geometry and quality.
 *
 * @implementation
 * Adapts ray intersection code from:
 * 'Fast, Minimum Storage Ray-Triangle Intersection';
 * Moller, Trumbore;
 * 1997.
 * (Journal of Graphics Tools, v2 n1 p21)
 * http://www.acm.org/jgt/papers/MollerTrumbore97/
 *
 * @invariants
 * - vertexs length = 3
 * - reflectivity >= 0 and <= 1
 * - emitivity    >= 0
 *)




(* type --------------------------------------------------------------------- *)

type t = {
   (* geometry *)
   vertexs      : Vector3f.t array ;

   (* quality *)
   reflectivity : Vector3f.t ;
   emitivity    : Vector3f.t
}




(* constants ---------------------------------------------------------------- *)

(**
 * General tolerance of 1 mm seems reasonable.
 *)
let _TOLERANCE = 1.0 /. 1024.0

(* Epsilon suitable for at least single precision. *)
and _EPSILON   = 1.0 /. 1048576.0




(* construction ------------------------------------------------------------- *)

(**
 * Construct a Triangle instance.
 *
 * @param inBuffer (Scanf.Scanning.scanbuf) to read from
 *
 * @return (Triangle.t)
 *)
let create (inBuffer:Scanf.Scanning.scanbuf) =

   (* read vectors in order:
      three vertexs, then reflectivity, then emitivity *)
   let vectors =
      let rec readVectors vs i =
         if i = 0 then vs else readVectors ((vRead inBuffer) :: vs) (i - 1) in
      readVectors [] 5 in

   {  vertexs      = Array.sub (Array.of_list (List.rev vectors)) 0 3 ;
      reflectivity = vClamp vZERO vONE (List.nth vectors 1) ;
      emitivity    = vClamp vZERO (List.nth vectors 0) (List.nth vectors 0)  }




(* derived fields ----------------------------------------------------------- *)

let edge0 tr = tr.vertexs.(1) -| tr.vertexs.(0)

let edge1 tr = tr.vertexs.(2) -| tr.vertexs.(1)

let edge3 tr = tr.vertexs.(2) -| tr.vertexs.(0)




(* queries ------------------------------------------------------------------ *)

(**
 * Axis-aligned bounding box of triangle.
 *
 * @param tr (Triangle.t)
 *
 * @return (2 Vector3f array) lower corner and upper corner
 *)
let bound tr =

   (* include tolerance *)
   let expand clamp nudge = vZip (fun a b -> nudge b a) (vONE *|. _TOLERANCE)
      (* min or max across all vertexs *)
      (vZip clamp tr.vertexs.(0) (vZip clamp tr.vertexs.(1) tr.vertexs.(2))) in
   [| expand min (-.) ; expand max (+.) |]


(**
 * Intersection point of ray with triangle.
 *
 * @param tr           (Triangle.t)
 * @param rayOrigin    (Vector3f.t) ray origin
 * @param rayDirection (Vector3f.t) ray direction
 *
 * @return (float option) distance along ray if intersected
 *)
let intersection tr rayOrigin rayDirection =

   (* begin calculating determinant -- also used to calculate U parameter *)
   let pvec = vCross rayDirection (edge3 tr) in
   let det  = vDot (edge0 tr) pvec in

   (* if determinant is near zero, ray lies in plane of triangle *)
   if (det > -._EPSILON) && (det < _EPSILON) then None
   else

      let invDet = 1.0 /. det in

      (* calculate distance from vertex 0 to ray origin *)
      let tvec = rayOrigin -| tr.vertexs.(0) in

      (* calculate U parameter and test bounds *)
      let u = (vDot tvec pvec) *. invDet in
      if (u < 0.0) || (u > 1.0) then None
      else

         (* prepare to test V parameter *)
         let qvec = vCross tvec (edge0 tr) in

         (* calculate V parameter and test bounds *)
         let v = (vDot rayDirection qvec) *. invDet in
         if (v < 0.0) || (u +. v > 1.0) then None
         else

            (* calculate t -- where ray intersects triangle *)
            let hitDistance = (vDot (edge3 tr) qvec) *. invDet in

            (* only allow intersections in the forward ray direction *)
            if hitDistance < 0.0 then None else Some hitDistance


(**
 * Monte-carlo sample point on triangle.
 *
 * @param tr     (Triangle.t)
 * @param random (Random2.t) random number generator
 *
 * @return (Vector3f.t) position
 *)
let samplePoint tr random =

   (* make barycentric coords *)
   let c0, c1 = let module R = Random2 in
      let srr1, r2 = sqrt (Random2.real64 random), (Random2.real64 random) in
      (1.0 -. srr1), ((1.0 -. r2) *. srr1)

   (* make barycentric axes *)
   and a0, a1 = (edge0 tr), (edge3 tr) in

   (* sum scaled components, and offset from corner *)
   ((a0 *|. c0) +| (a1 *|. c1)) +| (tr.vertexs.(0))


(**
 * Normal (Vector3f.t).
 *)
let normal tr = vUnitize (vCross (edge0 tr) (edge1 tr))

(**
 * Tangent (Vector3f.t).
 *)
let tangent tr = vUnitize (edge0 tr)

(**
 * Area (float).
 * (half area of parallelogram)
 *)
let area tr = 0.5 *. vLength (vCross (edge0 tr) (edge1 tr))


(**
 * Emitivity accessor (Vector3f.t).
 *)
let emitivity tr = tr.emitivity

(**
 * Reflectivity accessor (Vector3f.t).
 *)
let reflectivity tr = tr.reflectivity
