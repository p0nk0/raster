open! Core

let command =
  Command.group
    ~summary:"A tool to perform various image manipulations"
    [ "grayscale", Grayscale.command
    ; "bluescreen", Blue_screen.command
    ; "blur", Blur.command
    ; "dither", Dither.command
    ; "dither2", Big_dither.command
    ; "dither3", Bayer_dither.command
    ; "detect-edges", Edge_detection.command
    ; "color-dither", Color_dither.command
    ; "steganography", Steganography.command
    ; "obra-dinn", Obra_dinn.command
    ]
;;
