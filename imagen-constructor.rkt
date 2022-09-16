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

(define flipH
  (lambda (imagen)
    (get-filas imagen '())))

(define get-filas;;get
  (lambda (imagen flip-list)
          (cond
            [(null? imagen) (flipV-interna flip-list '())];;llamo a la funcion flipV, esta me invierte el arreglo
            (else (get-filas (cdr imagen) (cons (flipH-invierte-filas (car imagen) '()) flip-list))))))

(define flipH-invierte-filas
  (lambda (filas list-aux)
    (cond
      [(null? filas) list-aux]
      (else (flipH-invierte-filas (cdr filas) (cons (car filas) list-aux))))))

(define get-cant-col;;get cololumnas
  (lambda (imagen)
    (cond
      [(null? imagen) 0]
      (else (+ 1 (get-cant-col (cdr imagen)))))))

(define get-cant-fil;;get filas
  (lambda (imagen)
    (get-cant-col (car imagen))))

    
;;(flipV (constructor-imagen 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4)))
;;(flipH (constructor-imagen 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4)))
;;(flipH (constructor-imagen 3 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 0 1 1 49) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4) (pixbit-d 1 2 1 50)))


;;funcion que recorta el cuadrante de una imagen y retorna una imagen recortada
;;x es y e y es x, esta al revez
(define crop
  (lambda (imagen x1 y1 x2 y2)
    (toma-pixeles imagen x1 y1 x2 y2 '() (get-cant-col imagen) (get-cant-fil imagen) 0 0)))

(define toma-pixeles
  (lambda (imagen x1 y1 x2 y2 recorte fil col cant-col cant-fil)
    (cond
      [(< y2 cant-col) recorte]
      [(null? imagen) recorte]
      [(<= y1 cant-col) (toma-pixeles (cdr imagen) x1 y1 x2 y2 (cons (get-puntos (car imagen) '() x1 x2 fil 0) recorte) fil col (+ 1 cant-col) 0)]
      (else (toma-pixeles (cdr imagen) x1 y1 x2 y2 recorte fil col (+ 1 cant-col) cant-fil)))))

(define get-puntos
  (lambda (fila recorte x1 x2 fil cant-fil)
    (cond
      [(< x2 cant-fil) recorte]
      [(<= x1 cant-fil)(get-puntos (cdr fila) (cons (car fila) recorte) x1 x2 fil (+ 1 cant-fil))]
      (else (get-puntos (cdr fila) recorte x1 x2 fil (+ 1 cant-fil))))))

;;(crop (constructor-imagen 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4)) 0 1 1 1)


(define img1 '(((1 10) (0 20)) ((0 30) (1 4))))
;;(crop img1 0 1 1 1)

;;funcion que toma un pixel y lo tranforma a un hex

;(provide get-R)
;(provide get-G)
;(provide get-B)

(define imgRGB->imgHex
  (lambda (imagen)
    (get-columnas imagen '())))

(define get-columnas
  (lambda (imagen lista-temp)
    (cond
      [(null? imagen) (reverse lista-temp)]
      (else (get-columnas (cdr imagen) (cons (recorre-pixeles (car imagen) '()) lista-temp))))))

(define recorre-pixeles
  (lambda (columna lista)
    (cond
      [(null? columna) (reverse lista)]
      (else (recorre-pixeles (cdr columna) (cons (cons (string-append "#"
      (string-append (car (resto-num (get-R (car columna)) '())) (string-append (car (resto-num (get-G (car columna)) '())) (car (resto-num (get-B (car columna)) '()))))) (list (get-D (car columna)))) lista))))))


;;(imgRGB->imgHex (constructor-imagen 2 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40)))
