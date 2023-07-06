open Core

(* assumes all images are greyscale *)
let add_error ~image ~error ~width ~height ~x ~y =
  if x < width - 1 (* east *)
  then
    Image.set
      image
      ~x:(x + 1)
      ~y
      (Pixel.of_int
         (Pixel.red (Image.get image ~x:(x + 1) ~y)
          + Int.of_float (7. /. 16. *. error)));
  if x < width - 1 && y < height - 1 (* southeast *)
  then
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (Pixel.of_int
         (Pixel.red (Image.get image ~x:(x + 1) ~y:(y + 1))
          + Int.of_float (1. /. 16. *. error)));
  if y < height - 1 (* south *)
  then
    Image.set
      image
      ~x
      ~y:(y + 1)
      (Pixel.of_int
         (Pixel.red (Image.get image ~x ~y:(y + 1))
          + Int.of_float (5. /. 16. *. error)));
  if x > 0 && y < height - 1 (* southwest *)
  then
    Image.set
      image
      ~x:(x - 1)
      ~y:(y + 1)
      (Pixel.of_int
         (Pixel.red (Image.get image ~x:(x - 1) ~y:(y + 1))
          + Int.of_float (3. /. 16. *. error)))
;;

(* This should look familiar by now! *)
let transform image =
  let grey = Grayscale.transform image in
  let max = Image.max_val grey in
  let width = Image.width grey in
  let height = Image.height grey in
  Image.mapi grey ~f:(fun ~x ~y (old_value, _, _) ->
    let new_value = if old_value > max / 2 then max else 0 in
    let error = old_value - new_value in
    add_error ~image:grey ~error:(Float.of_int error) ~width ~height ~x ~y;
    Pixel.of_int new_value)
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
