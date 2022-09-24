#lang racket

(provide pixhex-d)
(provide cant-elementos-pixhex)
(provide es-pixhex?)

;Descripcion: funcion que contruye un pixhex-d
;Dominio; Num X Num X String X Num
;Recorrido; pixhex-d

(define pixhex-d ;;contructor del bit de la imagen 
  (lambda (x y hex depth)
    (list x y hex depth)))

;Descripcion: funcion que suma la cantidad de elementos que hay en un pixel
;Dominio; Lista
;Recorrido; Num
;Tipo de recursion: Natural

(define cant-elementos-pixhex
  (lambda (pixel)
    (cond
      [(null? pixel) 0]
      (else (+ 1 (cant-elementos-pixhex (cdr pixel)))))))

;;(eq? (string-ref "Apple" 0) #\A)
;;(string-ref "Apple" 0)
;(eq? (string-ref (car '("#12" "#2")) 1) #\1)


(define es-pixhex?
  (lambda (pixel posicion)
    (cond
      [(eq? posicion 7) #t]
      [(eq? (string-ref (car pixel) posicion) #\#) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\0) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\1) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\2) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\3) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\4) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\5) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\6) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\7) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\8) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\9) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\A) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\B) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\C) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\D) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\E) (es-pixhex? pixel (+ 1 posicion))]
      [(eq? (string-ref (car pixel) posicion) #\F) (es-pixhex? pixel (+ 1 posicion))]
      (else #f))))

;(es-pixhex? (cdr pixel (+ 1 posicion))


