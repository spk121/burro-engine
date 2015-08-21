(define-module (tmx)
  #:use-module (json)
  #:use-module (burro)
  #:export (tmx-load-from-data-file
            tmx-layer-types
            tmx-best-fit-layer-sizes))

(define (tmx-load-from-data-file filename)
  "Unpack the JSON file named FILENAME which is in the
path pointed to by the BURRO_DATA_DIR environment variable"
  (let ([full-filename (data-path filename)])
    (if full-filename
        (let ([fp (open-file full-filename "r")])
          (false-if-exception (json->scm fp)))
        ;; else
        #f)))

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
