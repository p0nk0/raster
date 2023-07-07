open Core

let in_bounds width height x y = 0 <= x && x < width && 0 <= y && y < height
let horrizontal_gradient = [ -1; 0; 1; -2; 0; 2; -1; 0; 1 ]
let vertical_gradient = [ -1; -2; -1; 0; 0; 0; 1; 2; 1 ]

let convolve image x y width height ~horrizontal =
  let in_bounds = in_bounds width height in
  let positions =
    Array.init 9 ~f:(fun i -> (i % 3) + x - 1, (i / 3) + y - 1)
  in
  let gradient =
    Array.of_list
      (if horrizontal then horrizontal_gradient else vertical_gradient)
  in
  let get_value x y =
    if in_bounds x y then Pixel.red (Image.get image ~x ~y) else 0
  in
  Array.mapi positions ~f:(fun i (x, y) -> get_value x y * gradient.(i))
  |> Array.fold ~init:0 ~f:( + )
;;

let transform image =
  let blur = Blur.transform image ~radius:2 in
  let grey = Grayscale.transform blur in
  let max = Image.max_val grey in
  let threshold = 0.4 *. Float.of_int max in
  let white = Pixel.of_int max in
  let width = Image.width image in
  let height = Image.height image in
  Image.mapi grey ~f:(fun ~x ~y _ ->
    let grad_x =
      Float.square
        (Float.of_int (convolve grey x y width height ~horrizontal:true))
    in
    let grad_y =
      Float.square
        (Float.of_int (convolve grey x y width height ~horrizontal:false))
    in
    let grad = Float.sqrt (grad_x +. grad_y) in
    if Float.compare grad threshold > 0 then white else Pixel.zero)
;;

let command =
  Command.basic
    ~summary:"Detects edges on an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge.ppm")]
;;
