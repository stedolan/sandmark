(*------------------------------------------------------------------------------

   MiniLight OCaml : minimal global illumination renderer
   Harrison Ainsworth / HXA7241 : 2006-2009, 2013.

   http://www.hxa.name/minilight

------------------------------------------------------------------------------*)




open Vector3f

(* uses:
 * - Vector3f
 *)




(**
 * Pixel sheet, with simple tone-mapping and file formatting.
 *
 * Mutable.
 *
 * @implementation
 * Uses PPM image format:
 * http://netpbm.sourceforge.net/doc/ppm.html
 *
 * Uses Ward simple tonemapper:
 * 'A Contrast Based Scalefactor For Luminance Display'; Ward; 1994.
 * (Graphics Gems 4, AP)
 *
 * @invariants
 * - width  >= 1 and <= _DIMENSION_MAX
 * - height >= 1 and <= _DIMENSION_MAX
 * - pixels length = (width * height)
 *)




(* type --------------------------------------------------------------------- *)

type t = {
   width  : int ;
   height : int ;
   pixels : Vector3f.t array
}




(* constants ---------------------------------------------------------------- *)

(**
 * Image dimension maximum: for both width and height.
 *)
let _DIMENSION_MAX = 4000

(* Formatting items. *)
let _PPM_ID        = "P6"
and _MINILIGHT_URI = "http://www.hxa.name/minilight"

(* Guess of average screen maximum brightness,
   ITU-R BT.709 standard RGB luminance weighting,
   ITU-R BT.709 standard gamma. *)
let _DISPLAY_LUMINANCE_MAX = 200.0
and _RGB_LUMINANCE         = vCreate 0.2126 0.7152 0.0722
and _GAMMA_ENCODE          = 0.45




(* construction ------------------------------------------------------------- *)

(**
 * Construct a Image instance.
 *
 * @param inBuffer (Scanf.Scanning.scanbuf) to read from
 *
 * @return (Image.t)
 *)
let create (inBuffer:Scanf.Scanning.scanbuf) =

   (* read width and height in order, with conditioning *)
   let width, height = Scanf.bscanf inBuffer " %u %u" (fun w h ->
      (max 1 (min w _DIMENSION_MAX), max 1 (min h _DIMENSION_MAX))) in

   {  width  = width ;
      height = height ;
      pixels = Array.init (width * height) (fun _ -> vZERO)  }




(* class functions ---------------------------------------------------------- *)

(**
 * Calculate tone-mapping scaling factor.
 *
 * @param pixels  (vector3f.t array) pixels
 * @param divider (float)            pixel scaling factor
 *
 * @return (float) scaling factor
 *)
let toneMapping pixels divider =

   (* calculate estimate of world-adaptation luminance
      as log mean luminance of scene *)
   let adaptLuminance = if (Array.length pixels) = 0 then 1e-4 else
      let sumOfLogs =
         let logSummer sum pixel =
            (* luminance *)
            let y = (vDot pixel _RGB_LUMINANCE) *. divider in
            (* clamp luminance to perceptual minimum *)
            sum +. (log10 (max 1e-4 y)) in
         Array.fold_left logSummer 0.0 pixels in
      10.0 ** (sumOfLogs /. (float (Array.length pixels))) in

   (* make scale-factor from:
      ratio of minimum visible differences in luminance, in display-adapted
      and world-adapted perception (discluding the constant that cancelled),
      divided by display max to yield a [0,1] range *)
   let a = 1.219 +. ((_DISPLAY_LUMINANCE_MAX *. 0.25) ** 0.4)
   and b = 1.219 +. (adaptLuminance                   ** 0.4) in

   ((a /. b) ** 2.5) /. _DISPLAY_LUMINANCE_MAX




(* commands ----------------------------------------------------------------- *)

(**
 * Accumulate (add, not just assign) a value to the image.
 *
 * @param im       (Image.t)
 * @param x        (int)        x coord
 * @param y        (int)        y coord
 * @param radiance (Vector3f.t) radiance
 *
 * @return ()
 *)
let addToPixel' im x y radiance =

   (* only inside image bounds *)
   if (x >= 0) && (x < im.width) && (y >= 0) && (y < im.height) then

      let index = x + ((im.height - 1 - y) * im.width) in
      (* mutate *)
      im.pixels.(index) <- (im.pixels.(index) +| radiance)

   else ()




(* queries ------------------------------------------------------------------ *)

(**
 * Width (int).
 *)
let width im  = im.width

(**
 * Height (int).
 *)
let height im = im.height


(**
 * Write the image to a serialised format.
 *
 * @param im        (Image.t)
 * @param iteration (int)         number of accumulations made to the image
 * @param out       (out_channel) to receive the image
 *
 * @return ()
 *)
let formatted im iteration (out:out_channel) =

   let divider        = 1.0 /. (float (max 1 iteration)) in
   let tonemapScaling = toneMapping im.pixels divider in

   (* write PPM P6 format *)

   (* write ID and comment *)
   let () = Printf.fprintf out "%s\n# %s\n\n" _PPM_ID _MINILIGHT_URI in
   (* write width, height, maxval *)
   let () = Printf.fprintf out "%i %i\n%i\n" im.width im.height 255 in

   (* write pixels *)
   let pixelWriter pixel =
      let channelWriter c =

         (* tonemap, gamma encode, quantize *)
         let mapped    = c *. divider *. tonemapScaling in
         let gammaed   = (max 0.0 mapped) ** _GAMMA_ENCODE in
         let quantized = truncate ((gammaed *. 255.0) +. 0.5) in
         (* output as byte *)
         output_byte out (max 0 (min quantized 255)) in

      Array.iter channelWriter pixel in
   Array.iter pixelWriter im.pixels
