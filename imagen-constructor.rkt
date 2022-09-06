#lang racket

(require "TDA-pixbit-d.rkt")
(require "TDA-pixrgb-d.rkt")

(define constructor-imagen ;; version 3.-> contructor de imagen
  (lambda (largo ancho  . imagen)
    (envo-constru-imagen (list largo ancho imagen) (get-fil(list largo ancho imagen)) (get-col(list largo ancho imagen)))));;list largo ancho  imagen

;;(constructor-imagen 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4)) 

(define get-fil ;;seclectores 
  (lambda (constructor-imagen)
    (car constructor-imagen)))

(define get-col ;;selectores 
  (lambda (constructor-imagen)
    (car (cdr constructor-imagen))))

(define envo-constru-imagen
  (lambda (pixeles fil col)
    (constru-imagen (car (cddr pixeles)) '() fil col 0 0)))

;;->((0 0 1 10) (0 1 0 20) (1 0 0 30) (1 1 1 4))

(define constru-imagen
         (lambda(pixeles imagen fil col cant-f cant-c)
           (cond
             [(null? pixeles) (reverse imagen)]
             (else(constru-imagen (cdr pixeles) (cons (crea-filas (car pixeles) '()) imagen) fil col 0 0)))))
             

(define crea-filas
  (lambda (pixeles lista)
    (cond 
     [(null? (cddr pixeles)) lista]
     (else (crea-filas (cdr pixeles) (cons (car (cddr pixeles)) lista))))))

;;(car(reverse prueba))
;;->10

;;(cdr lista)
;;-> '((0 1 0 20) (1 0 0 30) (1 1 1 4))

(define lista '((0 0 1 10) (0 1 0 20) (1 0 0 30) (1 1 1 4)))
(define prueba '(1 2 3 4 10))

;;(constructor-imagen 2 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40))