#lang racket

;(define constructor-imagen ;;contructor imagen
  ;(lambda (largo ancho  . imagen)
    ;(list largo ancho  imagen)))

(define constructor-imagen ;; version 3.
  (lambda (largo ancho  . imagen)
    (envo-make-matrix (get-fil-pixbit-d (list largo ancho imagen)) (get-col-pixbit-d (list largo ancho imagen)) (envo-datos-imagen (list largo ancho imagen)))));;envo-datos-imagen (list largo ancho  imagen

;(define constructor-imagen ;; version 2 (se queda). ;;version 2
  ;(lambda (largo ancho  . imagen)
    ;(envo-datos-imagen (list largo ancho  imagen))))

(define pixbit-d ;;contructor del bit de la imagen 
  (lambda (x y bit depth)
    (list x y bit depth)))

;;(constructor-imagen 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4)) 
;;'(2 2 ((0 0 1 10) (0 1 0 20) (1 0 0 30) (1 1 1 4)))
;; de esta matrix de dato pixbit me interesa solo el bit en este caso y la profundidad

(define get-fil-pixbit-d ;;seclectores de pixbit-d
  (lambda (constructor-imagen)
    (car constructor-imagen)))

(define get-col-pixbit-d ;;selectores de pixbit-d
  (lambda (constructor-imagen)
    (car (cdr constructor-imagen))))

;;(car (cddr imagen))
;;>'((0 0 1 10) (0 1 0 20) (1 0 0 30) (1 1 1 4))

;;(car (cddr (car imagen-c)))
;;>1

(define envo-datos-imagen
  (lambda (imagen)
    (datos-imagen (car(cddr imagen)) '())))

(define datos-imagen
  (lambda (imagen lista)
    (cond
      [(null? imagen) lista];;lista
      (else (datos-imagen (cdr imagen) (cons (car (cddr (car imagen))) (cons (car (cdddr (car imagen))) lista)))))));;invertir para que no me quede la profundidad primero y despues el pixel.

;; al pasarle "lista" al creador de matriz quedara filete, pues al pasarlo en pares el principio sera el fin y viceversa.

;;(constructor-imagen 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4))
;;-> '(1 4 0 30 0 20 1 10)

;;(cons 1 (cons 2 (cons 3 '())))

;;(define hola '((1 2 3)))
;;(car (cdr (car hola))) -> 2

;;----crea imagen (construrtor)----

(define envo-make-matrix
  (lambda (fil col elementos)
    (make-matrix 0 0 fil col elementos '() '())))

(define make-matrix 
  (lambda (cant-f cant-c fil col element lista matris)
    (cond
      ;[(eq? col cant-c) matris] ;;(invierte-image matris '())
      [(null? element) matris]
      [(eq? 1 cant-f) (make-matrix 0 0 fil col (cdr element) '() (cons lista matris))] ; (cons lista matris)
      [(eq? 0 cant-f) (make-matrix (+ 1 cant-f) cant-c fil col (cdr element) (cons (car element) (cons (car (cdr element)) lista)) matris)]))) ; tengo una list ej: '(1)
      ;[(null? element) matris])))
      ;(else (make-matrix (+ 1 cant-f) cant-c fil col (cddr element) (cons (car element) (cons (car (cdr element)) lista)) (cons lista matris))))))


;(define invierte-image ; funcion que invierte una matris 
  ;(lambda (matris matris-inv)
    ;(cond
      ;[(null? matris) matris-inv]
      ;(else (invierte-image (cdr matris) (cons (car matris) matris-inv))))))

;-> (cons (car defi) (cons (car (cdr defi)) '()))

;> (constructor-imagen 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4))
;'((1 10) (0 20) (0 30) (1 4))

