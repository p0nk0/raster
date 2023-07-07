open Core

let in_bounds width height x y = 0 <= x && x < width && 0 <= y && y < height

let matrix =
  Array.of_list [ 0; 0; 0; 7; 5; 3; 5; 7; 5; 3; 1; 3; 5; 3; 1 ]
  |> Array.map ~f:(fun n -> Float.of_int n /. 48.)
;;

(* assumes all images are greyscale *)
let add_error ~image ~error ~width ~height ~x ~y =
  let in_bounds = in_bounds width height in
  let positions = Array.init 15 ~f:(fun i -> (i % 5) + x - 2, (i / 5) + y) in
  let get_value x y = Pixel.red (Image.get image ~x ~y) in
  let matrix = Array.map matrix ~f:(fun n -> Int.of_float (error *. n)) in
  Array.iteri positions ~f:(fun i (x, y) ->
    if in_bounds x y
    then Image.set image ~x ~y (Pixel.of_int (get_value x y + matrix.(i))))
;;

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
    ~summary:
      "Dither an image with a better dithering algorithm \
       (Jarvis-Judice-Ninke)"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither2.ppm")]
;;
