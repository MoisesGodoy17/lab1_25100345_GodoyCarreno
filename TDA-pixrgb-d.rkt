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

(define pixrgb-d ;;contructor del bit de la imagen 
  (lambda (x y r g b depth)
    (list x y r g b depth)))

(define get-R
  (lambda (imagen);;recibe una lista de pixeles rgb (250 0 250 10)
    (first imagen)))

(define get-G
  (lambda (imagen)
    (second imagen)));;retorna el G del RGB

(define get-B
  (lambda (imagen)
    (third imagen)));; retorna el B del RGB

(define get-D
  (lambda (imagen)
    (last imagen)))

;Descripcion: funcion que suma la cantidad de elementos que hay en un pixel
;Dominio; Lista
;Recorrido; Num
;Tipo de recursion: Natural

(define cant-elementos-pixrgb
  (lambda (pixel)
    (cond
      [(null? pixel) 0]
      (else (+ 1 (cant-elementos-pixrgb (cdr pixel)))))))


;Descripcion: funcion que determina si el pixel de entrada es un pixrgb
;Dominio; Lista
;Recorrido; Num
;Tipo de recursion: Natural

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

(define resto-num;;como idea se puede aplicar un map a todos los elementos
  (lambda (numero lista)
    (resta-resto (cons (quotient numero 16) lista) numero)));;(cons (quotient numero 16) lista

(define resta-resto
  (lambda (lista numero)
    (conversion->hex (cons (- numero (* 16 (car lista))) lista) '())))

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

(define hex->string
  (lambda (hex temp)
      (cons (string-append (first hex) (second hex)) temp)))



