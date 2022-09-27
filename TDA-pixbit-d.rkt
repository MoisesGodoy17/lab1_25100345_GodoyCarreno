#lang racket

(provide pixbit-d)
(provide es-pixbit?)

;Descripcion: funcion que construye un pixbit-d.
;Dominio: Num X Num X Num X Num.
;Recorrido: Pibit-d.

(define pixbit-d ;;contructor del bit de la imagen 
  (lambda (x y bit depth)
    (list x y bit depth)))

;Funcion: Funcion que determina si es un pixbit, toma un pixel y verifica si este es 1 o 0
;de ser asi retorna #t. 
;Dominio: Pixbit
;Recorrido: Boolean
;Tipo de recursion: No aplica.

(define es-pixbit? ;pasarlo al tda pixbit
  (lambda (pixel)
    (cond
      [(eq? (cant-elementos-pixbit pixel) 2)
       (cond
         [(eq? (car pixel) 1) #t]; si es 1 retorno #t
         [(eq? (car pixel) 0) #t]
         (else #f))]
      (else #f))))

;Descripcion: funcion que suma la cantidad de elementos que hay en un pixel
;Dominio; Lista
;Recorrido; Num
;Tipo de recursion: Natural

(define cant-elementos-pixbit
  (lambda (pixel)
    (cond
      [(null? pixel) 0]
      (else (+ 1 (cant-elementos-pixbit (cdr pixel)))))))

;;(es-pixbit? '(1 2))