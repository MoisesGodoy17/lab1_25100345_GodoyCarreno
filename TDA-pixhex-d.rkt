#lang racket


(define pixhex-d ;;contructor del bit de la imagen 
  (lambda (x y hex depth)
    (list x y hex depth)))