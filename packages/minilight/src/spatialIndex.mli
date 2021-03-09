(*------------------------------------------------------------------------------

   MiniLight OCaml : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2006-2009, 2013.

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*)




type t

val create : Vector3f.t -> Triangle.t list -> t

val intersection : t -> Vector3f.t -> Vector3f.t -> ?position:Vector3f.t ->
   Triangle.t option -> (Triangle.t * Vector3f.t) option
