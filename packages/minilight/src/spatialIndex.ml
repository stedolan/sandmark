(*------------------------------------------------------------------------------

   MiniLight OCaml : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2006-2009, 2013.

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*)




open Vector3f

(* uses:
 * - Vector3f
 * - Triangle
 *)




(**
 * A minimal spatial index for ray tracing.
 *
 * Suitable for a scale of 1 numerical unit == 1 metre, and with a resolution
 * of 1 millimetre. (Implementation uses fixed tolerances)
 *
 * @implementation
 * Octree: axis-aligned, cubical. Subcells are numbered thusly:
 * <pre>      110---111
 *            /|    /|
 *         010---011 |
 *    y z   | 100-|-101
 *    |/    |/    | /
 *    .-x  000---001      </pre>
 *
 * Each cell stores its bound: fatter data, but simpler code.
 *
 * Calculations for building and tracing are absolute rather than incremental --
 * so quite numerically solid. Uses tolerances in: bounding triangles (in
 * Triangle.bound), and checking intersection is inside cell (both effective
 * for axis-aligned items). Also, depth is constrained to an absolute subcell
 * size (easy way to handle overlapping items).
 *
 * @invariants
 * - node.bound has length 2
 * - node.bound.(0) <= node.bound.(1)
 * - SubCells has length 8
 *)




(* types -------------------------------------------------------------------- *)

type nodeArray = SubCells of t array | Items of Triangle.t array
and  node      = { bound: Vector3f.t array ; subparts: nodeArray }
and  t         = Node of node | Empty




(* constants ---------------------------------------------------------------- *)

(* Maximum levels of nesting.
   Accommodates scene including sun and earth, down to cm cells
   (use 47 for mm) *)
let _MAXLEVELS = 44

(* Maximum items per leaf (when below maximum nesting level).
   8 seemed reasonably optimal in casual testing *)
and _MAXITEMS  =  8




(* construction ------------------------------------------------------------- *)

(**
 * Main recursive constructor.
 *
 * @param bound (2 Vector3f.t array) lower corner and upper corner
 * @param items (Triangle.t list)
 * @param level (int)
 *
 * @return (SpatialIndex.node)
 *)
let rec construct bound items level =

   (* if items overflow leaf and tree not too deep --
      make branch: make subcells, and recurse construction *)
   if (List.length items > _MAXITEMS) && (level < (_MAXLEVELS - 1)) then

      let q1 = ref 0 in
      (* define subcell maker *)
      let makeSubcell subcellIndex =

         (* make subcell bound *)
         let subBound =
            let center       = (bound.(0) +| bound.(1)) *|. 0.5
            and choose v0 v1 = vInit (fun i -> if ((subcellIndex lsr i) land 1)
               <> 0 then v1.(i) else v0.(i)) in
            (* choose axes of: lower or middle, and middle or upper;
               according to subcell index bits *)
            [| choose bound.(0) center ; choose center bound.(1) |] in

         (* collect items that overlap subcell *)
         let subItems =
            let isOverlap item =
               let itemBound = Triangle.bound item
               (* overlap test for half bound *)
               and isHalfOv b0 b1 = (vFold min (b0.(1) -| b1.(0))) >= 0.0 in
               (isHalfOv itemBound subBound) && (isHalfOv subBound itemBound) in
            List.filter isOverlap items in

         (* curtail degenerate subdivision by adjusting next level
            (degenerate if two or more subcells copy entire contents of parent,
            or if subdivision reaches below mm size)
            (having a model including the sun requires one subcell copying
            entire contents of parent to be allowed) *)
         q1 := !q1 + if List.length subItems = List.length items then 1 else 0 ;
         let q2  = (vFold min (subBound.(1) -| subBound.(0))) <
            (Triangle._TOLERANCE *. 4.0) in

         (* recurse to make subcell, if any overlapping subitems *)
         if List.length subItems > 0 then Node (construct subBound subItems
            (if (!q1 > 1) || q2 then _MAXLEVELS else level + 1)) else Empty in

      (* make subcells *)
      { bound = bound ; subparts = SubCells (Array.init 8 makeSubcell) }

   (* make leaf: store items, and end recursion *)
   else
      (* (make sure to trim any reserve capacity) *)
      { bound = bound ; subparts = Items (Array.of_list items) }


(**
 * Constructor a SpatialIndex instance.
 *
 * @param eyePosition (Vector3f.t)
 * @param items       (Triangle.t list)
 *
 * @return (SpatialIndex.t)
 *)
let create eyePosition items =

   (* make overall bound *)
   let bound =
      (* accommodate all items, and eye position
         (makes tracing algorithm simpler) *)
      let rectBound =
         let encompass rb item =
            let ib = Triangle.bound item in
            [| vZip min rb.(0) ib.(0) ; vZip max rb.(1) ib.(1) |] in
         List.fold_left encompass [| eyePosition ; eyePosition |] items in

      (* make cubical *)
      let maxSize = vFold max (rectBound.(1) -| rectBound.(0)) in
      [| rectBound.(0) ;
         vZip max rectBound.(1) (rectBound.(0) +| (vONE *|. maxSize)) |] in

   (* make subcell tree *)
   Node (construct bound items 0)




(* queries ------------------------------------------------------------------ *)

(** Find nearest intersection of ray with item.
 *
 * @param si           (SpatialIndex.t)
 * @param rayOrigin    (Vector3f.t)
 * @param rayDirection (Vector3f.t)
 * @param position     (Vector3f.t) walk position (default: rayOrigin)
 * @param lastHit      (Triangle.t option) previous intersected item
 *
 * @return ((Triangle.t, Vector3f.t) option) hit item, and hit position
 *)
let rec intersection si rayOrigin rayDirection ?(position = rayOrigin) lastHit =

   match si with

   | Node { bound = bound ; subparts = SubCells subCells } ->
      intersectBranch bound subCells rayOrigin rayDirection position lastHit

   | Node { bound = bound ; subparts = Items items } ->
      intersectLeaf bound items rayOrigin rayDirection lastHit

   | Empty -> None


(** Find nearest intersection of ray with branch.
 *
 * @param bound        (2 Vector3f.t array) lower corner and upper corner
 * @param subCells     (SpatialIndex.t array)
 * @param rayOrigin    (Vector3f.t)
 * @param rayDirection (Vector3f.t)
 * @param position     (Vector3f.t) walk position
 * @param lastHit      (Triangle.t option) previous intersected item
 *
 * @return ((Triangle.t, Vector3f.t) option) hit item, and hit position
 *)
and intersectBranch bound subCells rayOrigin rayDirection position lastHit =

   (* step through subcells and recurse *)

   let center = (bound.(0) +| bound.(1)) *|. 0.5 in

   (* find which subcell holds walk point (walk point is inside cell) *)
   let subCell =
      (* compare dimension with center *)
      let bit i = if position.(i) >= center.(i) then 1 lsl i else 0 in
      (bit 0) lor (bit 1) lor (bit 2) in

   (* define intersected subcell walker *)
   let rec walk subCell cellPosition =

      (* intersect subcell for possible hit *)
      match intersection subCells.(subCell) rayOrigin rayDirection
         ~position:(cellPosition) lastHit with

      (* no hit, so continue walking across subcells *)
      | None ->

         (* find next subcell ray moves to
            (by finding which face of the corner ahead is crossed first) *)
         let step, axis, _ =
            let findNext (step, axis, i) =
               (* find which face (inter-/outer-) the ray is heading for (in
                  this dimension) *)
               let dir  = rayDirection.(i)
               and high = (subCell lsr i) land 1 in
               let face = if ((if (dir < 0.0) then 1 else 0) lxor high) <> 0
                  then bound.(high).(i) else center.(i) in
               (* calculate distance to face
                  (div by zero produces infinity, which is later discarded) *)
               let distance = (face -. rayOrigin.(i)) /. dir in
               if distance <= step
                  then (distance, i, i + 1) else (step, axis, i + 1) in
            findNext (findNext (findNext (max_float, 0, 0))) in

         (* leaving branch if: direction is negative and subcell is low,
            or direction is positive and subcell is high *)
         if ((if (rayDirection.(axis) < 0.0) then 1 else 0) lxor
            ((subCell lsr axis) land 1)) = 1
         then None
         (* move to (outer face of) next subcell *)
         else walk (subCell lxor (1 lsl axis)) (rayOrigin +| (rayDirection *|.
            step))

      (* hit, so exit *)
      | Some _ as hit -> hit in

   (* step through intersected subcells *)
   walk subCell position


(** Find nearest intersection of ray with leaf.
 *
 * @param bound        (2 Vector3f.t array) lower corner and upper corner
 * @param items        (Triangle.t array)
 * @param rayOrigin    (Vector3f.t)
 * @param rayDirection (Vector3f.t)
 * @param lastHit      (Triangle.t option) previous intersected item
 *
 * @return ((Triangle.t, Vector3f.t) option) hit item, and hit position
 *)
and intersectLeaf bound items rayOrigin rayDirection lastHit =

   (* exhaustively intersect contained items *)

   let boundLow, boundUpp = bound.(0), bound.(1) in

   (* define nearest-finder *)
   let findNearest nearest item = match lastHit with
      (* avoid spurious intersection with surface just come from *)
      | Some it when it == item -> nearest
      | _ -> let _, _, nearestDistance = nearest in

         (* intersect item and inspect if nearest so far *)
         match Triangle.intersection item rayOrigin rayDirection with
         | Some distance when distance < nearestDistance ->
            let hit = rayOrigin +| (rayDirection *|. distance) in

            (* check intersection is inside cell bound (with tolerance) *)
            let t = Triangle._TOLERANCE in
            if (boundLow.(0) -. hit.(0) > t) || (hit.(0) -. boundUpp.(0) > t) ||
               (boundLow.(1) -. hit.(1) > t) || (hit.(1) -. boundUpp.(1) > t) ||
               (boundLow.(2) -. hit.(2) > t) || (hit.(2) -. boundUpp.(2) > t)
            then nearest else (Some item, hit, distance)
         | _ ->
            nearest in

   (* apply nearest-finder to items list *)
   (match Array.fold_left findNearest (None, vZERO, infinity) items with
   | Some hitObject, hitPosition, _ -> Some (hitObject, hitPosition)
   | None, _, _ -> None)
