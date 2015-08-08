;;; Routines for parsing TMX files

(define (Tmx_apply_to_bg tmx_hash main-or-sub)
  "This is the main function to set a screen's BG layers
from a TMX file.

If the TMX has a tilesheet, it is copied into a screen's tilesheet
storage area.

It is an error to have more than one tilesheet.

Up to 4 layers are copied to BG layers.  TMX Image layers will
become BG BMP layers.  TMX Tile layers will be come BG MAP layers.

It is an error to have more than 4 layers, or to have a MAP layer
without a tilesheet.

This will re-map indices from TMX tilesheet coordinates (where
a tilesheet's dimensions may have any integer value) to Burro
tilesheet coordinates (where a tilesheet's dimension are one
of a list of fixed sizes like MATRIX_16x16).

Each layer needs to have a custom 'priority' property, so that
it can be staged properly.

The VRAM allocator will be used to assign the layers and tilesheet
into appropriate matrix sizes and VRAM buffer.  All previous VRAM
assignments for this main-or-sub will be overwritten.

An error will be thrown on VRAM allocation failure.
"
  #t)

(define (Tmx_tilesheet_validate tmx_hash)
  "Checks that the tilesheet referenced in the given TMX hashtable has
a valid size and that its data file exists in the data directory."
  #t)

(define (Tmx_get_tilesheet_size tmx_hash)
  "Returns the dimensions of the tilesheet referenced in a given TMX hashtable.

Will throw an error on failure"
  (1 . 1))

(define (Tmx_load_tilesheet tmx_hash main_or_sub)
  "Given a TMX hash, this loads up the tilesheet reference in the TMX
hash into the tilesheet VRAM buffer of the main or sub screens.

Will throw an error on failure."
  #f)

(define (Tmx_load_map_layer tmx_hash ...)
  "Given a TMX hash, this loads up a TMX Tile layer into one of the
background layers of the main or sub screens.

This will re-map indices from TMX tilesheet coordinates (where
a tilesheet's dimensions may have any integer value) to Burro
tilesheet coordinates (where a tilesheet's dimension are one
of a list of fixed sizes like MATRIX_16x16).

A TMX Tile layer needs to have a custom property 'priority' so
that ")

