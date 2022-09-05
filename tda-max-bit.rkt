#lang racket

;;contructor de pixmap-d

(define constructor-imagen ;; version 3.-> contructor de imagen
  (lambda (largo ancho  . imagen)
    (list largo ancho imagen )))

(define pixrgb-d ;;contructor del bit de la imagen 
  (lambda (x y r g b depth)
    (list x y r g b depth)))
;(image 2 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40))

(define get-fil-pixrgb-d ;;seclectores de pixbit-d
  (lambda (constructor-imagen)
    (car constructor-imagen)))

(define get-col-pixrgb-d ;;selectores de pixbit-d
  (lambda (constructor-imagen)
    (car (cdr constructor-imagen))))

(define get-lista-datos-pixel
  (lambda (constructor-imagen)
    (get-rgb (car(cddr constructor-imagen)) '())))

(define get-rgb
  (lambda (constructor-imagen lista)
    (cond
      [(null?constructor-imagen) lista]
      (else get-rgb))

;(list (car (cddr (car constructor-imagen))) (car (cdddr (car constructor-imagen))) (car (cddddr (car constructor-imagen))) (car (last constructor-imagen)))))
    

;;(get-lista-datos-pixel (constructor-imagen 2 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40)))