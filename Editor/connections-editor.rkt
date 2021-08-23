#lang racket/gui

;; a panel for adding and subtracting connections between places in the editor map 

(define connector (-> (or/c '+ '-) hash? any/c))

(provide
 From
 To  
 Color
 Seg# 

 (contract-out 
  (manage-connections
   ;; fr is the frame where the selection is added, a default frame is provided;
   ;; the hash-table function receives a hash with four attributes: from, to, color, seg#
   ;;   and is assumed to update some global table
   ;; the 2 strings specify one more panel with which a user can add a new connection 
   (->* (connector)
        (#:connections0 [listof [list/c string? string? color? natural?]]
         #:frame (or/c #false (is-a?/c area-container<%>)) #:x natural? #:y natural?)
        (-> [listof [list/c string? string?]] any)))))

;; ---------------------------------------------------------------------------------------------------
(require Trains/Common/basic-constants)
(require pict)

(module+ picts
  (require (submod "..")))
  
;; ---------------------------------------------------------------------------------------------------

#;{type Connection* = [Listof Connection]}
#;{type Connection  = [List String String] || [List String String Symbol Natural]}
 
(define From  'from)
(define To    'to)
(define Color 'color)
(define Seg#  'seg#)

(define FRAME (new frame% [label "edit connections"] [width 300] [height 300]))

(define (manage-connections edit #:connections0 (c0 '[]) #:frame [fr FRAME] #:x [x #f] #:y [y 0])
  (define the-frame (or fr FRAME))
  (when x (send the-frame move x y))
  (define cb (cb* the-frame (callback the-frame fr edit)))
  (cb c0)
  cb)

#; {[Instance Frame] {Connector -> [String String [String Natural] -> Void]} -> Connections -> Void}
(define ((cb* fr cb) connections0)
  (send fr begin-container-sequence)
  (for ([c connections0]) (apply cb c))
  (send fr end-container-sequence))

(define C "color: ")
(define S "segments: ")

#; {Frame Connector -> [Symbol Symbol [Symbol Natural] -> Void]}
(define ((callback fr do-show? edit-connection) a b (color #false) (seg# #false))
  (define attr
    (hash From  a
          To    b
          Color (or (and color (~a color)) (first COLORS))
          Seg#  (or seg# (first SEG#))))
  #;{ (U '+ '-) String String [String -> Any] -> Choice Event -> Void}
  (define ((set action from to convert) co evt)
    (define val (send co get-string (send co get-selection)))
    (set! attr (hash-set attr action (convert val))))

  (define pa (new horizontal-pane% [parent fr][alignment '(left top)] [spacing 1]))
  (define iv (~a "between " a " and " b))
  (new text-field% [parent pa] [label ""] [min-width 100] [init-value iv] [enabled #false])
  (new choice% [parent pa][label C][choices (map ~a COLORS)] [callback (set Color a b values)])
  (new choice% [parent pa][label S][choices (map ~a SEG#)]   [callback (set Seg# a b string->number)])

  (define next (if color '- '+))
  #; {Button Event -> Void}
  (define (swap x y)
    (edit-connection next attr)
    (set! next (if (eq? next '+) '- '+))
    (send x set-label (~a next)))
  (new button% [parent pa] [label (~a next)] [callback swap])
  
  (send fr show do-show?))

;; ---------------------------------------------------------------------------------------------------
(module+ picts
  (send FRAME begin-container-sequence)
  ((manage-connections #:connections0 '[["x" "y" "red" 3]] #:x 100 #:y 0
                       (compose displayln list))
   (map list '["AB" "AC" "BC"] '["AB" "AC" "BC"]))
  (send FRAME end-container-sequence))