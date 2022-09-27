#lang racket

(provide pixrgb-d)
(provide get-R)
(provide get-G)
(provide get-B)
(provide get-D)
(provide resto-num)
(provide resta-resto)
(provide conversion->hex)
(provide hex->string)
(provide cant-elementos-pixrgb)
(provide es-pixrgb?)

;Descripcion: funcion que construye un pixrgb-d.
;Dominio: Num X Num X Num X Num X Num X Num.
;Recorrido: Pixrgb-d.

(define pixrgb-d ;;contructor del bit de la imagen 
  (lambda (x y r g b depth)
    (list x y r g b depth)))

;Descripcion: funcion obtiene la componene R de un pixel RGB
;Dominio: Pixrgb-d.
;Recorrido: Num.

(define get-R
  (lambda (pixrgb);;recibe una lista de pixeles rgb (250 0 250 10)
    (first pixrgb)))

;Descripcion: funcion obtiene la componene G de un pixel RGB
;Dominio: Pixrgb-d.
;Recorrido: Num.

(define get-G
  (lambda (pixrgb)
    (second pixrgb)));;retorna el G del RGB

;Descripcion: funcion obtiene la componene B de un pixel RGB
;Dominio: Pixrgb-d.
;Recorrido: Num.

(define get-B
  (lambda (pixrgb)
    (third pixrgb)));; retorna el B del RGB

;Descripcion: funcion obtiene la componene D de un pixel RGB
;Dominio: Pixrgb-d.
;Recorrido: Num.

(define get-D
  (lambda (pixrgb)
    (last pixrgb)))

;Descripcion: funcion que suma la cantidad de elementos que hay en un pixel
;Dominio: Pixrgb-d.
;Recorrido: Num.
;Tipo de recursion: Natural.

(define cant-elementos-pixrgb
  (lambda (pixel)
    (cond
      [(null? pixel) 0]
      (else (+ 1 (cant-elementos-pixrgb (cdr pixel)))))))


;Descripcion: funcion que determina si el pixel de entrada es un pixrgb.
;Dominio: Pixrgb-d.
;Recorrido: Num.

(define es-pixrgb?
  (lambda (pixel acum cant-elemen)
    (cond
      [(eq? 3 acum) #t]
      [(eq? cant-elemen 4)
       (cond
         [(<= 0 (car pixel))
          (cond
            [(<= (car pixel) 255) (es-pixrgb? (cdr pixel) (+ 1 acum) cant-elemen)]
            (else #f))]
         (else #f))]
      (else #f))))

;Descripcion: obtiene el resto de una componente de un pixrgb-d.
;Dominio: Num X List.
;Recorrido: List

(define resto-num
  (lambda (numero lista)
    (resta-resto (cons (quotient numero 16) lista) numero)));;(cons (quotient numero 16) lista

;Descripcion: obtiene el resto del resto anterior.
;Dominio: Num X List.
;Recorrido: List

(define resta-resto
  (lambda (lista numero)
    (conversion->hex (cons (- numero (* 16 (car lista))) lista) '())))

;Descripcion: funcion que tranforma una lista con restos de numeros a una representacion hex.
;Dominio: List X List.
;Recorrido: String

(define conversion->hex
  (lambda (imagen temp)
    (cond
      [(null? imagen) (hex->string temp '())]
      [(eq? 10 (car imagen)) (conversion->hex (cdr imagen) (cons "A" temp))]
      [(eq? 11 (car imagen)) (conversion->hex (cdr imagen) (cons "B" temp))]
      [(eq? 12 (car imagen)) (conversion->hex (cdr imagen) (cons "C" temp))]
      [(eq? 13 (car imagen)) (conversion->hex (cdr imagen) (cons "D" temp))]
      [(eq? 14 (car imagen)) (conversion->hex (cdr imagen) (cons "E" temp))]
      [(eq? 15 (car imagen)) (conversion->hex (cdr imagen) (cons "F" temp))]
      (else (conversion->hex (cdr imagen) (cons (number->string(car imagen)) temp))))))

;Descripcion: funcion que uno dos string en uno solo. 
;Dominio: List X List.
;Recorrido: String.

(define hex->string
  (lambda (hex temp)
      (cons (string-append (first hex) (second hex)) temp)))



