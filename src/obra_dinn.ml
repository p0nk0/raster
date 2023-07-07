open Core

let transform image =
  let max = Float.of_int (Image.max_val image) in
  let dark =
    ( Int.of_float (0.2 *. max)
    , Int.of_float (0.2 *. max)
    , Int.of_float (0.1 *. max) )
  in
  let light =
    Int.of_float (0.9 *. max), Int.of_float max, Int.of_float max
  in
  Bayer_dither.transform ~small:true image
  |> Image.map ~f:(fun (old_value, _, _) ->
       if old_value = 0 then dark else light)
;;

let command =
  Command.basic
    ~summary:"Convert an image to the Obra Dinn color palette"
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
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_obra_dinn.ppm")]
;;
