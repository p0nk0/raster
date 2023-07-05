let find_hidden value = (value land 3) lsl 6

let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    find_hidden r, find_hidden g, find_hidden b)
;;

let command =
  Command.basic
    ~summary:"Steganography an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm image ~filename:"images/Mystery.ppm"]
;;
