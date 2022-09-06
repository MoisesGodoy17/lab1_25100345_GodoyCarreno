#lang racket

(provide pixrgb-d)

(define pixrgb-d ;;contructor del bit de la imagen 
  (lambda (x y r g b depth)
    (list x y r g b depth)))