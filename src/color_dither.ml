open Core

let add_error ~image ~errors:(r, g, b) ~width ~height ~x ~y =
  if x < width - 1 (* east *)
  then
    Image.set
      image
      ~x:(x + 1)
      ~y
      ( Pixel.red (Image.get image ~x:(x + 1) ~y)
        + Int.of_float (7. /. 16. *. r)
      , Pixel.green (Image.get image ~x:(x + 1) ~y)
        + Int.of_float (7. /. 16. *. g)
      , Pixel.blue (Image.get image ~x:(x + 1) ~y)
        + Int.of_float (7. /. 16. *. b) );
  if x < width - 1 && y < height - 1 (* southeast *)
  then
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      ( Pixel.red (Image.get image ~x:(x + 1) ~y:(y + 1))
        + Int.of_float (1. /. 16. *. r)
      , Pixel.green (Image.get image ~x:(x + 1) ~y:(y + 1))
        + Int.of_float (1. /. 16. *. g)
      , Pixel.blue (Image.get image ~x:(x + 1) ~y:(y + 1))
        + Int.of_float (1. /. 16. *. b) );
  if y < height - 1 (* south *)
  then
    Image.set
      image
      ~x
      ~y:(y + 1)
      ( Pixel.red (Image.get image ~x ~y:(y + 1))
        + Int.of_float (5. /. 16. *. r)
      , Pixel.green (Image.get image ~x ~y:(y + 1))
        + Int.of_float (5. /. 16. *. g)
      , Pixel.blue (Image.get image ~x ~y:(y + 1))
        + Int.of_float (5. /. 16. *. b) );
  if x > 0 && y < height - 1 (* southwest *)
  then
    Image.set
      image
      ~x:(x - 1)
      ~y:(y + 1)
      ( Pixel.red (Image.get image ~x:(x - 1) ~y:(y + 1))
        + Int.of_float (3. /. 16. *. r)
      , Pixel.green (Image.get image ~x:(x - 1) ~y:(y + 1))
        + Int.of_float (3. /. 16. *. g)
      , Pixel.blue (Image.get image ~x:(x - 1) ~y:(y + 1))
        + Int.of_float (3. /. 16. *. b) )
;;

let update_value old_value channels max =
  let old_float = Float.of_int old_value /. Float.of_int max in
  let frac_of_max =
    Float.round_down (old_float *. Float.of_int (channels - 1))
    /. Float.of_int (channels - 1)
  in
  let new_value = Int.of_float (frac_of_max *. Float.of_int max) in
  let error = old_value - new_value in
  new_value, error
;;

(* This should look familiar by now! *)
let transform image ~channels =
  let max = Image.max_val image in
  let width = Image.width image in
  let height = Image.height image in
  Image.mapi image ~f:(fun ~x ~y (old_red, old_green, old_blue) ->
    let new_red, red_error = update_value old_red channels max in
    let new_green, green_error = update_value old_green channels max in
    let new_blue, blue_error = update_value old_blue channels max in
    add_error
      ~image
      ~errors:
        ( Float.of_int red_error
        , Float.of_int green_error
        , Float.of_int blue_error )
      ~width
      ~height
      ~x
      ~y;
    new_red, new_green, new_blue)
;;

let command =
  Command.basic
    ~summary:"Dither a color image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and channels =
        flag
          "channels"
          (required Command.Param.int)
          ~doc:"N the number of channels per color"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~channels in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_dither_color.ppm")]
;;
