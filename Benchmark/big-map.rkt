#lang racket

(provide big-map)

(define big-map
  
  '#hasheq((cities
            .
            (("19" (62 565))
             ("18" (69 479))
             ("17" (29 460))
             ("16" (149 423))
             ("15" (122 412))
             ("14" (9 610))
             ("13" (130 655))
             ("12" (69 467))
             ("11" (5 469))
             ("10" (107 636))
             ("9" (199 704))
             ("8" (38 689))
             ("7" (189 727))
             ("6" (188 221))
             ("5" (42 93))
             ("4" (144 538))
             ("3" (111 229))
             ("2" (51 486))
             ("1" (168 607))
             ("0" (61 202))))
           (connections
            .
            #hasheq((|0|
                     .
                     #hasheq((|13| . #hasheq((green . 3)))
                             (|19| . #hasheq((white . 3)))
                             (|3| . #hasheq((white . 3)))
                             (|7| . #hasheq((red . 3)))))
                    (|1|
                     .
                     #hasheq((|10| . #hasheq((blue . 3)))
                             (|11| . #hasheq((white . 3)))
                             (|12| . #hasheq((green . 3)))
                             (|15| . #hasheq((green . 3)))
                             (|3| . #hasheq((blue . 3)))))
                    (|10| . #hasheq((|13| . #hasheq((green . 3)))
                                    (|2| . #hasheq((blue . 3)))
                                    (|7| . #hasheq((green . 3)))))
                    (|11| . #hasheq((|13| . #hasheq((green . 3)))
                                    (|8| . #hasheq((red . 3)))))
                    (|12|
                     .
                     #hasheq((|16| . #hasheq((green . 3)))
                             (|18| . #hasheq((green . 3)))
                             (|19| . #hasheq((red . 3)))
                             (|2| . #hasheq((white . 3)))
                             (|6| . #hasheq((green . 3) (white . 3)))))
                    (|13| . #hasheq((|4| . #hasheq((green . 3)))))
                    (|14| . #hasheq())
                    (|15| . #hasheq((|7| . #hasheq((blue . 3))) (|9| . #hasheq((white . 3)))))
                    (|16| . #hasheq((|17| . #hasheq((green . 3))) (|2| . #hasheq((green . 3)))))
                    (|17|
                     .
                     #hasheq((|3| . #hasheq((blue . 3)))
                             (|5| . #hasheq((green . 3) (red . 3)))
                             (|7| . #hasheq((blue . 3) (red . 3)))
                             (|9| . #hasheq((blue . 3)))))
                    (|18| . #hasheq((|6| . #hasheq((white . 3))) (|9| . #hasheq((white . 3)))))
                    (|19| . #hasheq((|6| . #hasheq((blue . 3))) (|8| . #hasheq((red . 3)))))
                    (|2| . #hasheq((|5| . #hasheq((green . 3)))))
                    (|3| . #hasheq((|9| . #hasheq((blue . 3)))))
                    (|4| . #hasheq())
                    (|5| . #hasheq((|6| . #hasheq((white . 3)))
                                   (|7| . #hasheq((green . 3)))
                                   (|8| . #hasheq((blue . 3)))))
                    (|6| . #hasheq())
                    (|7| . #hasheq())
                    (|8| . #hasheq())
                    (|9| . #hasheq())))
           (height . 800)
           (width . 200)))