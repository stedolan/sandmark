(*------------------------------------------------------------------------------

   MiniLight OCaml : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2006-2009, 2013.

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*)




(* uses:
 * - Random2
 * - Scene
 * - Image
 * - Camera
 *)




(**
 * Control module and entry point.
 *
 * Handles command-line UI, and runs the main progressive-refinement render
 * loop.
 *
 * Usage: Supply a model file pathname as the command-line argument. Or -? for
 * help.
 *)




(* user messages ------------------------------------------------------------ *)

let _TITLE  = "MiniLight 1.6 OCaml"
and _AUTHOR = "Harrison Ainsworth / HXA7241 : 2006-2009, 2013."
and _URL    = "http://www.hxa.name/minilight"
and _DATE   = "2013-05-04" ;;

let _BANNER_MESSAGE = "\n  " ^ _TITLE ^ " - " ^ _URL ^ "\n\n"

and _HELP_MESSAGE   = "\n\
   ----------------------------------------------------------------------\n  \
     " ^ _TITLE ^ "\n\n  " ^ _AUTHOR ^ "\n  " ^ _URL ^ "\n\n  " ^ _DATE ^ "\n\
   ----------------------------------------------------------------------\n\
   \n\
   MiniLight is a minimal global illumination renderer.\n\
   \n\
   usage:\n  \
     minilight modelFilePathName\n\
   \n\
   The model text file format is: \n  \
     #MiniLight\n\
   \n  \
     iterations\n\
   \n  \
     imagewidth imageheight\n  \
     viewposition viewdirection viewangle\n\
   \n  \
     skyemission groundreflection\n\
   \n  \
     vertex0 vertex1 vertex2 reflectivity emitivity\n  \
     vertex0 vertex1 vertex2 reflectivity emitivity\n  \
     ...\n\
   \n\
   - where iterations and image values are integers, viewangle is a real,\n\
   and all other values are three parenthised reals. The file must end\n\
   with a newline. E.g.:\n  \
     #MiniLight\n\
   \n  \
     100\n\
   \n  \
     200 150\n  \
     (0 0.75 -2) (0 0 1) 45\n\
   \n  \
     (3626 5572 5802) (0.1 0.09 0.07)\n\
   \n  \
     (0 0 0) (0 1 0) (1 1 0)  (0.7 0.7 0.7) (0 0 0)\n\
  \n" ;;




(* setup ctrl-c SIGINT interrupt handling ----------------------------------- *)

let () = Sys.catch_break true ;;




(* constants ---------------------------------------------------------------- *)

let _MODEL_FORMAT_ID = "#MiniLight" ;;




(* entry point -------------------------------------------------------------- *)

(* catch all exceptions *)
try

   (* check for help request *)
   if ((Array.length Sys.argv) <= 1) ||
      (Sys.argv.(1) = "-?") || (Sys.argv.(1) = "--help") then

      print_string _HELP_MESSAGE

   (* execute *)
   else
      let () = print_string _BANNER_MESSAGE in

      (* prepare to render *)

      (* make random generator *)
      let random = Random2.create () in

      (* get file names *)
      let modelFilePathname = Sys.argv.(1) in
      let imageFilePathname = modelFilePathname ^ ".ppm" in

      (* open model file and check format identifier at start of first line *)
      let modelFile = Scanf.Scanning.from_file modelFilePathname in
      if not (Scanf.bscanf modelFile "%s" ((=) _MODEL_FORMAT_ID)) then
         failwith "unrecognised input format" ;

      (* create top-level rendering objects with model file, in this order
         (image is mutable) *)
      let iterations = Scanf.bscanf modelFile " %u" (max 1) in
      let image      = Image.create  modelFile in
      let camera     = Camera.create modelFile in
      let scene      = Scene.create  modelFile (Camera.eyePoint camera) in

      (* render *)

      try
         (* render by progressive refinement *)
         for frameNo = 1 to iterations do
            (* display current iteration number *)
            Printf.printf "\riteration: %i%!" frameNo ;

            (* render a frame *)
            Camera.frame camera scene random image ;

            (* save image at twice error-halving rate, and at start and end *)
            if ((frameNo land (frameNo - 1)) = 0) || (iterations == frameNo)
               then begin
                  (* open file, write, close *)
                  let imageFile = open_out_bin imageFilePathname in
                  Image.formatted image frameNo imageFile ;
                  close_out imageFile end
         done ;

         print_string "\nfinished\n"

      (* handle ctrl-c interrupt *)
      with Sys.Break ->
         print_string "\ninterrupted\n"

with e ->
   let () = print_string "*** execution failed:  " in
   raise e ;;
