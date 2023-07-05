open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius =
  let width = Image.width image in
  let height = Image.height image in
  let result = Image.copy image in
  Image.mapi result ~f:(fun ~x ~y _ ->
    Image.mean_pixel
      (Image.slice
         image
         ~x_start:(Int.max 0 (x - radius))
         ~x_end:(Int.min width (x + radius))
         ~y_start:(Int.max 0 (y - radius))
         ~y_end:(Int.min height (y + radius))))
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
