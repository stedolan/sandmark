#!/bin/bash

# --- using: OCaml 4.00 ---

ocamlopt.opt -verbose -unsafe -nodynlink -inline 100 -I src -o minilight-ocaml unix.cmxa src/random2.ml src/vector3f.ml src/triangle.ml src/spatialIndex.mli src/spatialIndex.ml src/scene.ml src/surfacePoint.ml src/rayTracer.ml src/image.ml src/camera.ml src/minilight.ml

rm src/*.cm?
rm src/*.o

exit
