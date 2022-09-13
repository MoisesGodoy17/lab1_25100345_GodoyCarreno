#lang racket

(require "TDA-pixbit-d.rkt")
(require "TDA-pixrgb-d.rkt")

(define constructor-imagen ;; version 3.-> contructor de imagen
  (lambda (largo ancho  . imagen)
    (envo-constru-imagen (list largo ancho imagen) (get-fil (list largo ancho imagen)) (get-col (list largo ancho imagen)))));;list largo ancho  imagen

;;(constructor-imagen 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4))
;;(constructor-imagen 2 2 (pixbit-d  0 1 0 20) (pixbit-d  0 0 1 10) (pixbit-d 1 1 1 4) (pixbit-d 1 0 0 30))

;;(constructor-imagen 3 3 (pixbit-d 1 2 1 99) (pixbit-d  0 1 0 20) (pixbit-d  0 0 1 10) (pixbit-d 1 1 1 4) (pixbit-d 1 0 0 30) (pixbit-d 0 2 0 39))

(define get-fil ;;seclectores 
  (lambda (constructor-imagen)
    (car constructor-imagen)))

(define get-col ;;selectores 
  (lambda (constructor-imagen)
    (car (cdr constructor-imagen))))

(define envo-constru-imagen
  (lambda (pixeles fil col)
    (constru-imagen (car (cddr pixeles)) '() '() fil col 0 0)))

;;constructor final.

(define constru-imagen
  (lambda (pixeles imagen lista-aux fil col cant-f cant-c)
    (cond
      [(eq? cant-f fil) (constru-imagen pixeles (cons (reverse lista-aux) imagen) '() fil col 0 (+ 1 cant-c))]
      [(eq? cant-c col) (reverse imagen)]
      (else (constru-imagen (cdr pixeles) imagen (cons (get-bit-depth(car pixeles)) lista-aux) fil col (+ 1 cant-f) cant-c)))))

(define get-bit-depth;;selector
  (lambda (pixeles)
    (cddr pixeles)))


(define flipV;;funcion flipV-> recive la imagen e invierte las filas
  (lambda (imagen)
    (flipV-interna imagen '())))

(define flipV-interna
  (lambda (imagen flip-list)
    (cond
      [(null? imagen) flip-list]
      (else (flipV-interna (cdr imagen) (cons (car imagen) flip-list))))))

;;(define flipH
  ;;(lambda (imagen)
    ;;(flipH-invierte-filas imagen '() '())))

;;(define flipH-invierte-filas
  ;;(lmabda (imagen flip-list imagen-flip)
          ;;(cond
            ;;[(null? imagen)])))



    
;;(flipV (constructor-imagen 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4)))

;;ordena pixeles y construye
;;(define constru-imagen
  ;;(lambda (pixeles lista-temp imagen fil col cant-c cant-f)
    ;;(cond
      ;;[(eq? cant-c col) (reverse imagen)]
      ;;[(eq? cant-f fil) (constru-imagen pixeles '() (cons (ordena-pixeles lista-temp '() col fil 0 0) imagen) fil col (+ 1 cant-c) 0)]
      ;;[(eq? cant-c (car (car pixeles))) (constru-imagen (cdr pixeles) (cons (car pixeles) lista-temp) imagen fil col cant-c (+ 1 cant-f))]
      ;;[(null? pixeles)(constru-imagen pixeles lista-temp imagen fil col cant-c cant-f)])))


;;(define ordena-pixeles
  ;;(lambda(pixeles lista-aux fil col cant-c cant-f)
    ;;(cond
      ;;[(eq? cant-f fil) (reverse lista-aux)]
      ;;[(eq? cant-f  (car (cdr (car pixeles)))) (ordena-pixeles pixeles (cons (filtra-pos (car pixeles)) lista-aux) fil col cant-c (+ 1 cant-f))]
      ;;[(null? pixeles) (ordena-pixeles pixeles lista-aux fil col cant-c cant-f)]
      ;;(else (ordena-pixeles (cdr pixeles) lista-aux fil col cant-c cant-f)))))

;;(define filtra-pos
  ;;(lambda (pixeles)
    ;;(cddr pixeles)))
;;-hasta aca lo nuevo

;;->((0 0 1 10) (0 1 0 20) (1 0 0 30) (1 1 1 4))

;(define constru-imagen -> constructor original
         ;(lambda(pixeles imagen fil col cant-f cant-c)
           ;(cond
             ;[(null? pixeles) (reverse imagen)];;(reverse imagen
             ;(else (constru-imagen (cdr pixeles) (cons (crea-filas (car pixeles) '()) imagen) fil col 0 0)))))
             

;;Nuevo contructor -> no terminado
;(define constru-imagen
  ;(lambda (pixeles imagen temp fil col cant-f cant-c);;nueva version
    ;(cond
      ;[(eq? cant-f fil) (constru-imagen (cdr pixeles) (cons (reverse temp) imagen) '() fil col 0 (+ 1 cant-c))]
      ;[(eq? cant-c col) imagen]
      ;[(null? pixeles) imagen]
      ;(else (constru-imagen (cdr pixeles) imagen (cons (crea-filas (car pixeles) '()) imagen) fil col (+ 1 cant-f) cant-c)))))
;;

;(define crea-filas
  ;(lambda (pixeles lista)
    ;(cond 
     ;[(null? (cddr pixeles)) (reverse lista)];;lista
     ;(else (crea-filas (cdr pixeles) (cons (car (cddr pixeles)) lista))))))

;;(car(reverse prueba))
;;->10

;;(cdr lista)
;;-> '((0 1 0 20) (1 0 0 30) (1 1 1 4))

(define lista '((0 0 1 10) (0 1 0 20) (1 0 0 30) (1 1 1 4)))
(define prueba '(1 2 3 4 10))

;;(constructor-imagen 2 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40))

;;filtro que compruba si esta en orden la los pixeles, en referencia a las posicines x e y

;> (car (cdr (car '((1 2 3 45) (7 87264 4) (02 3)))))      
;2

;(define ordena
  ;(lambda (lista largo ancho cant-a cant-l '())
    ;(cond
      ;[(eq? (car (car lista)) acum) (odena(car (cdr lista)) largo ancho cant-a cant-l (cond (car lista) temp))])))
      ;[(null? lista) llamo a la otra funcion con la lista de elementos a ordenar]

;;"\n" salto de linea

(define flipH
  (lambda (imagen)
    (flipH-interna (get-fil imagen) (get-col imagen) imagen 0 0 '() '())))

(define flipH-interna
  (lambda (constructor-imagen fil col cant-fil cant-col lista imagen)
          (cond
            [(null? constructor-imagen)]
            [(eq? cant-fil fil) (flipH-interna (cdr constructor-imagen) fil col 0 (+ 1 cant-col) '() (cons (reverse lista) imagen))]
            [(eq? cant-col col) imagen]
            (else (flipH-interna (cdr constructor-imagen) fil col (+ 1 cant-fil) cant-col (cons (car constructor-imagen) lista) imagen)))))

;;(flipH (constructor-imagen 2 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40)))
 