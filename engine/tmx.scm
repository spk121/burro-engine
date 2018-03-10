;;; tmx.scm -- parse the JSON files generated from TMX files as created by
;;;             the TileD editor
;;;
;;; Copyright 2015 Michael L. Gran <spk121@yahoo.com>
;;; GPL3+

(define TMX_MAIN_BG_PRAM #f)

#|
(define (tmx_LoadFromDataFile filename)
  "Unpack the JSON file named FILENAME which is in the
path pointed to by the BURRO_DATA_DIR environment variable"
  (let ([fullFilename (burro_FullPathToDataFile filename)]
        [filePort #f])
    (cond
     ((not fullFilename)
      #f)
     (else
      (set! filePort (open-file fullFilename "r"))
      (false-if-exception (json->scm filePort))))))

;; Tilesheet information

(define (_tmx_ValidateSheetInfo T)
  (let ([sheet-list (hash-ref T "tilesets")])
    (cond
     ((not sheet-list)
      (error "No tilesheet information found in TMX"))
     ((> (length sheet-list) 1)
      (error "Multiple tilesheets found in TMX")))))

(define (_tmx_GetSheetInfo T)
  (car (hash-ref T "tilesets")))

(define (_tmxSheetInfo_GetFilename TS)
  (hash-ref TS "image"))

(define (_tmxSheetInfo_GetMatrixSize TS)
  (apply matrix-find-best-fit
         (data-image-size
          (_tmxSheetInfo_GetFilename TS))))

(define (_tmx_UpdateMainBgSpritesheet T)
  (let* ([sheetInfo (_tmx_GetSheetInfo T)]
         [size (_tmxSheetInfo_GetMatrixSize sheetInfo)]
         [pram (pram_Alloc size)])
    (when TMX_MAIN_BG_PRAM
          (pram_Free TMX_MAIN_BG_PRAM))
    (set! TMX_MAIN_BG_PRAM pram)
    (sheet_AssignMemory SHEET_BG_MAIN size pram)
    (sheet_SetBmpFromFile SHEET_BG_MAIN filename)))

|#
#|
(define (tmx-main-do T)
  "Copy tmx layers to the main screen"
  
  (_tmx_ValidateSheetInfo T)

  
  ;; First, assign memory
  (let ([sheetInfo (_tmx_GetSheetInfo T)])
    
    (when (tmx-has-sheet T)
          (set! sheet-vram (tmx-sheet-assign-memory T))
          (unless sheet-vram
                  (error "Can't find a free PRAM buffer for a TMX tilesheet"))
          (unless (sheet-set-bmp-from-file (tmx-sheet-filename T))
                  (pram-free sheet-pram)
                  (error "Can't load ~a as a tilesheet"
                         (tmx-sheet-filename T))))

    (when (tmx-has-bmp-or-map-layers T)
          (let ([layers (hash-ref tmx "layers")])

            (let layer-loop ([layer-vram-list '()]
                             [layer-cur (car layers)]
                             [layer-rest (cdr layers)])
              
              (let ([T (hash-ref layer-cur "type")])
                (cond
                 ((string=? "imagelayer" T)
                  (let ([matrix-size (apply matrix-find-best-fit (data-image-size (hash-ref layer-cur "image")))])
                    (if matrix-size
                        (let ([layer-vram (tmx-layer-assign-memory layer-index matrix-size)])
                          (unless layer-vram
                                  (error "Out of PRAM memory"))
                          (unless (bg-set-bmp-from-file (tmx-bmp-layer-filename T))
                                  (error "Can't load ~a as a bmp"
                                         (tmx-bmp-layer-filename T))))
                        ;; else
                        (error "Image is too large for available PRAM"))))

                 ((string=? "tilelayer" T)
                  (let ([matrix-size (matrix-find-best-fit (hash-ref x "width") (hash-ref x "height"))])
                    (if matrix-size
                        (let ([layer-vram (tmx-layer-assign-memory matrix-size)])
                          (unless layer-vram
                                  (error "Out of PRAM memory"))
                          (let ([layer-bv (bg->bytevector XXX)])
                            
                       (else
                        #f))))
                    )))))))))
|#
#|
(define (tmx-layer-types tmx)
  "Return, as a list of strings, the types of layers in a TMX
hashtable, listed in order of their appearance."
  (append
    (map-in-order (lambda (x)
                    (let ([T (hash-ref x "type")])
                      (if (string? T)
                          T
                          ;; else
                          "unknown")))
                  (hash-ref tmx "layers"))))

(define (tmx-best-fit-sheet-size tmx)
  (let ([sheet (first (hash-ref tmx "tilesets"))])
    (let ([tilewidth (hash-ref sheet "tilewidth")]
          [tileheight (hash-ref sheet "tileheight")]
          [imageheight (hash-ref sheet "imageheight")]
          [imagewidth (hash-ref sheet "imagewidth")]
          [filename (hash-ref sheet "image")])
      (let* ([bmpsize (data-image-size filename)]
             [matrixsize (apply matrix-find-best-fit bmpsize)])
        (console-write-string
         (format #f "~a tmx_size ~ax~a bmp_size ~a"
                 filename
                 imagewidth
                 imageheight
                 bmpsize))
        matrixsize))))

(define (tmx-best-fit-layer-sizes tmx)
  "Return, as a list of matrix-size-indices, the minimum size
of layers required to hold the layer information in a TMX
hashtable, listed in order of their appearance."

  ;; for 'imagelayer', this is the image size according to GDK
  ;; for 'tilelayer', the size is in the TMX JSON
  ;; for 'objectlayer', the size is irrelevant
  (append
    (map-in-order (lambda (x)
                    (let ([T (hash-ref x "type")])
                      (cond
                       ((string=? "imagelayer" T)
                        (apply matrix-find-best-fit (data-image-size (hash-ref x "image"))))
                       ((string=? "tilelayer" T)
                        (matrix-find-best-fit (hash-ref x "width") (hash-ref x "height")))
                       (else
                        #f))))
                  (hash-ref tmx "layers"))))
!#
#|


(define fp (open-file "brickroom1.json" "r"))
(define x (json->scm fp))

;; This is the overall size of the map, in tiles
(define map-height (hash-ref x "height"))
(define map-width (hash-ref x "width"))

;; This is the size of a tile.
;; These have to match Burro's TILE_HEIGHT and TILE_WIDTH
(hash-ref x "tileheight")
(hash-ref x "tilewidth")

;; This needs to be the string "right-down"
(hash-ref x "renderorder")

;; Our maps can only have 1 tilesheet, so this has to be 0
;; if the backgrounds are all BMP or exactly 1
;; if there are any maps
(length (hash-ref x "tilesets"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define TS (car (hash-ref x "tilesets")))

;; These need to match Burro's TILE_HEIGHT and TILE_WIDTH
(hash-ref TS "tilewidth")
(hash-ref TS "tileheight")

;; These need to be 1 and zero respectively
(hash-ref TS "firstgid")
(hash-ref TS "spacing")

;; This is the sizeof the tilesheet, in pixels
(hash-ref TS "imageheight")
(hash-ref TS "imagewidth")

;; This is the stride, in tiles, of the tilesheet.
;; We need this to convert tile indices in maps to
;; tile locations on the tilesheet
(/ (hash-ref TS "imagewidth") (hash-ref TS "tilewidth"))

;; This is the filename of the tilesheet
(hash-ref TS "image")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a list of all the layers
(hash-ref x "layers")

;; This extracts a list of the layers that are map layers
;; You can assume that the first such layer is BG_MAIN_0
;; with priority 0, and the second is BG_MAIN_1, with priority 1
;; and so on.
(append
 (car
  (map-in-order (lambda (x)
                  (if (or (string=? "tilelayer" (hash-ref x "type"))
                          (string=? "bitmap" (hash-ref x "type")))
                      (list x)
                      '()))
                (hash-ref x "layers"))))



;; Say "L" is a particular map layer

;; This is the size of a layer in tiles
(hash-ref L "height")
(hash-ref L "width")

;; This sets if a layer is initially "shown" or "hidden"
(hash-ref L "visible")

;; This is the global opacity of a layer.  This is probably
;; going to be ignored.
(hash-ref L "opacity")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BMP LAYERS

;; This is the type of a BMP layer
(string=? "imagelayer" (hash-ref L "type"))

;; This is the filename of the image
(hash-ref L "image")

;; Note that the actual size of the image has nothing to
;; do with the size of the layer in tiles.  If the image
;; is too big, it can be truncated.  If it is too small,
;; it can be padded with transparency.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAP LAYERS

;; This is the actual map, a 1D list of indices
;; These indices need to be converted in Burro indices for
;; a tilesheet, so
;; burro_index = (tmx_index / tmx_stride) * burro_stride + (tmx_index % tmx_stride)
;; Take care to strip off and then re-apply the tile flip flags
(hash-ref L "data")

;; To move the TMX map indices to Burro map indices
(bg-set-to-map BG_MAIN_0)
(define bv (bg-get-bytevector BG_MAIN_0))
(bytevector-u32-set! bv (* 4 map-location-index) tilesheet-index-and-flags)
;; the '4' is for sizeof(uint32_t)

;; This is where it gets a bit tricky.
;; For TMX, the map has a given height and width in tiles,
;; and the tilesheet has a given height and width in tiles.

;; For Burro, the map and the tilesheet has to be one of the
;; pre-approved MATRIX_ sizes

;; burroMapRow = dataIndex / tmxMapWidth
;; burroMapCol = dataIndex % tmxMapWidth
;; burroMapIndex = burroMapRow * burroMapWidth + burroMapCol

;; The indices inside of the map itself also need to be adjusted
;; burroTilesheetRow = dataVal / tmxTilesheetWidth
;; burroTilesheetCol = dataVal / tmxTilesheetWidth
;; burroTilesheetIndex = burroTilesheetRow * burroTilesheetWidth
;;                       + burroTilesheetCol

;; Keep in mind that a layer's MATRIX_ size may be larger or
;; smaller than the TMX file's, so the map may need to be
;; truncated or zero-padded.

;; The tilesheet-index-and-flags is the 32-bit location of a tile in the
;; associated tilesheet, along with the associated vert, horiz, and diag
;; flip flags
;; The update the whole rendering of the bg layer with an explicit call
;; 'bg-update'
(bg-update BG_MAIN_0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OBJECT LAYERS

;; Burro only allows one "objectgroup" layer per TMX map

;; Say OG is an object group

;; This is the list of objects in an object group
(hash-ref O "objects")

;; Say OGL is the list of objects

|#
