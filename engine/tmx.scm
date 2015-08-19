(use-modules (json))

(define fp (open-file "brickroom1.json" "r"))
(define js (json->scm fp))

;; These have to be less than or equal to the map size
;; for each layer used
(define map-height (hash-ref js "height"))
(define map-width (hash-ref js "width"))

;; These have to match Burro's TILE_HEIGHT and TILE_WIDTH
(hash-ref x "tileheight")
(hash-ref x "tilewidth")

;; Our maps can only have 1 tilesheet, so this has to be 0
;; if the backgrounds are all BMP or exactly 1
;; if there are any maps
(length (hash-ref x "tilesets"))
(define TS (car (hash-ref x "tilesets")))

;; These need to match Burro's TILE_HEIGHT and TILE_WIDTH
(hash-ref TS "tilewidth")
(hash-ref TS "tileheight")

;; These need to be 1 and zero respectively
(hash-ref TS "firstgid")
(hash-ref TS "spacing")

;; This needs to be the string "right-down"
(hash-ref x "renderorder")

;; This is the stride, in tiles, of the tilesheet.
;; We need this to convert tile indices in maps to
;; tile locations on the tilesheet
(/ (hash-ref TS "imagewidth") (hash-ref TS "tilewidth"))

;; This is the filename of the tilesheet
(hash-ref TS "image")

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

;; These need to fit within the matrix
(hash-ref L "height")
(hash-ref L "width")

;; This sets if a layer is initially "shown" or "hidden"
(hash-ref L "visible")

;; This is the actual map, a 1D list of indices
;; These indices need to be converted in Burro indices for
;; a tilesheet, so
;; burro_index = (tmx_index / tmx_stride) * burro_stride + (tmx_index % tmx_stride)
;; Take care to strip off and then re-apply the tile flip flags
(hash-ref L "data")


;; Burro only allows one "objectgroup" layer per TMX map

;; Say OG is an object group

;; This is the list of objects in an object group
(hash-ref O "objects")

;; Say OGL is the list of objects
