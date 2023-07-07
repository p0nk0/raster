open Core

(* premade 2x2 bayer array from wikipedia (used in obra dinn) *)
let bayer1 =
  Array.of_list [ 0; 2; 3; 1 ]
  |> Array.map ~f:(fun n -> Float.of_int (n + 1) /. 4.)
;;

(* 4x4 array from wikipedia (unused) *)
(* [ 0; 8; 2; 10; 12; 4; 14; 6; 3; 11; 1; 9; 15; 7; 13; 5 ] *)

(* premade 8x8 bayer array from wikipedia *)
(* I wish the array didn't look like that lmao *)
let bayer2 =
  Array.of_list
    [ 0
    ; 32
    ; 8
    ; 40
    ; 2
    ; 34
    ; 10
    ; 42
    ; 48
    ; 16
    ; 56
    ; 24
    ; 50
    ; 18
    ; 58
    ; 26
    ; 12
    ; 44
    ; 4
    ; 36
    ; 14
    ; 46
    ; 6
    ; 38
    ; 60
    ; 28
    ; 52
    ; 20
    ; 62
    ; 30
    ; 54
    ; 22
    ; 3
    ; 35
    ; 11
    ; 43
    ; 1
    ; 33
    ; 9
    ; 41
    ; 51
    ; 19
    ; 59
    ; 27
    ; 49
    ; 17
    ; 57
    ; 25
    ; 15
    ; 47
    ; 7
    ; 39
    ; 13
    ; 45
    ; 5
    ; 37
    ; 63
    ; 31
    ; 55
    ; 23
    ; 61
    ; 29
    ; 53
    ; 21
    ]
  |> Array.map ~f:(fun n -> 1. -. (Float.of_int (n - 1) /. 64.))
;;

let get_threshold bayer size x y = bayer.((x % size) + (y % size * size))

let transform ?(small = false) image =
  let grey = Grayscale.transform image in
  let max = Image.max_val grey in
  let bayer, size = if small then bayer1, 2 else bayer2, 8 in
  Image.mapi grey ~f:(fun ~x ~y (old_value, _, _) ->
    let threshold = get_threshold bayer size x y in
    let new_value =
      if Float.compare (Float.of_int old_value /. Float.of_int max) threshold
         > 0
      then max
      else 0
    in
    Pixel.of_int new_value)
;;

let command =
  Command.basic
    ~summary:"Dither an image with a better dithering algorithm (Bayer)"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither3.ppm")]
;;
