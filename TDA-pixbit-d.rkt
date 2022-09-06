#lang racket

(provide pixbit-d)

(define pixbit-d ;;contructor del bit de la imagen 
  (lambda (x y bit depth)
    (list x y bit depth)))
