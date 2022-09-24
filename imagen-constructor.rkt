#lang racket

(require "TDA-pixbit-d.rkt")
(require "TDA-pixrgb-d.rkt")
(require "TDA-pixhex-d.rkt")

;Descripcion: imagen, recibe los datos del largo y ancho de la imagen, crea los pixeles de la imagen y guarda los datos en una lista,
;luego llama a la funcion envo-imagen, que da forma y contruye la imagen final.
;Dominio: num X num X [pixbit-d |  pixrgb-d | pixhex-d]
;Recorrido: Imagen

(define imagen 
  (lambda (largo ancho  . imagen); recibe n parametros despues del punto
    (envo-imagen (list largo ancho imagen) (get-fil (list largo ancho imagen)) (get-col (list largo ancho imagen)))));;list largo ancho  imagen

;;(imagen 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4))
;;(imagen 2 2 (pixbit-d  0 1 0 20) (pixbit-d  0 0 1 10) (pixbit-d 1 1 1 4) (pixbit-d 1 0 0 30))
;;(imagen 3 2 (pixbit-d 1 2 1 99) (pixbit-d  0 1 0 20) (pixbit-d  0 0 1 10) (pixbit-d 1 1 1 4) (pixbit-d 1 0 0 30) (pixbit-d 0 2 0 39))

;Descripcion: funcion que obtiene el ancho de la imagen
;Dominio: lista
;Recorrido: numero

(define get-fil ;;seclectores 
  (lambda (constructor-imagen)
    (car constructor-imagen)))

;Descripcion: funcion que obtiene el largo de la imagen
;Dominio: lista
;Recorrido: numero

(define get-col ;;selectores 
  (lambda (constructor-imagen)
    (car (cdr constructor-imagen))))

;Descripcion: funcion envoltorio de imagen que llama a la funcion que construye la imagen final
;Dominio: pixeles X num X num
;Recorrido: imagen

(define envo-imagen
  (lambda (pixeles fil col)
    (constru-imagen (car (cddr pixeles)) '() '() fil col 0 0)))

;Descripcion: funcnion que construye una imagen apartir de los datos extraidos,
;representa la imagen como una matriz, compuesta por filas y columnas.
;Dominio: lista X lista X lista X num X num X num X num X num
;Recorrido: imagen
;Tipo de recursion: recursion de cola

(define constru-imagen
  (lambda (pixeles imagen lista-aux fil col cant-f cant-c)
    (cond
      [(eq? cant-f fil) (constru-imagen pixeles (cons (reverse lista-aux) imagen) '() fil col 0 (+ 1 cant-c))]; reinicio la lista "aux", agrego sus datos a la lista imagen, reinicio "cant-fil" y sumo una a "cant-col"
      [(eq? cant-c col) (reverse imagen)]
      (else (constru-imagen (cdr pixeles) imagen (cons (get-pixel-datos(car pixeles)) lista-aux) fil col (+ 1 cant-f) cant-c)))));guardo los pixeles en la lista "aux" y suma 1 a "cant-fil"

;Descripcion: getters que extrar el pixel y la dimencion, sin incluir la posicion del bit en la imagen.
;Dominio: lista
;Recorrido: lista

(define get-pixel-datos;;selector. cambiar el nombre
  (lambda (pixeles)
    (cddr pixeles)))

;;-----Funciones de pertenecia: Bitmap, Pixmap? y hexmap.-----;;

;Descripcion: funcion envoltorio de es-bitmap?
;Recorrido: Imagen
;Dom: Boolean

(define bitmap?
  (lambda (imagen)
    (es-bitmap? (descontructor imagen)))); cambia el formato de la imagen, dejando todos los pixeles en una lista -> '((0 10)(1 20)(1 40)(1 8))

;Descripcion: Funcion que determina si es un bitmap o no. esta recorre toda la imgen y determina si los pixeles
;corresponden a un pixbit, si llega al final de la imagen, significa que la imagen es un bitmap
;y retorna #t de no ser asi retorna #f.
;Dominio: List
;Recorrido: Boolean
;Tipo de recursion: Recursion de cola

(define es-bitmap?
  (lambda (imagen-descontruida)
    (cond
      [(null? imagen-descontruida) #t]
      [(eq? #f (es-pixbit? (car imagen-descontruida))) #f]
      (else (es-bitmap? (cdr imagen-descontruida))))))

;Descripcion: funcion envoltorio de es-pixmap?
;Recorrido: Imagen
;Dominio: Boolean

(define pixmap?
  (lambda (imagen)
    (es-pixmap? (descontructor imagen))))

;Descripcion: Funcion que determina si es un pixmap o no. esta recorre toda la imgen y determina si los pixeles
;corresponden a un pixrgb, si llega al final de la imagen, significa que la imagen es un pixmap
;y retorna #t de no ser asi retorna #f.
;Dominio: List
;Recorrido: Boolean
;Tipo de recursion: Recursion de cola

(define es-pixmap?
  (lambda (imagen-descontruida)
    (cond
      [(null? imagen-descontruida) #t]
      [(eq? #f (es-pixrgb? (car imagen-descontruida) 0 (cant-elementos-pixrgb (car imagen-descontruida)))) #f]
      (else (es-pixmap? (cdr imagen-descontruida))))))

;Descripcion: funcion envoltorio de es-maphex?
;Recorrido: Imagen
;Dominio: Boolean

(define maphex?
  (lambda (imagen)
    (es-maphex? (descontructor imagen))))

;Descripcion: Funcion que determina si es un pixrgb o no. esta recorre toda la imgen y determina si los pixeles
;corresponden a un pixmap, si llega al final de la imagen, significa que la imagen es un pixmap
;y retorna #t de no ser asi retorna #f.
;Dominio: List
;Recorrido: Boolean
;Tipo de recursion: Recursion de cola

(define es-maphex?
  (lambda (imagen-descontruida)
    (cond
      [(null? imagen-descontruida) #t]
      [(eq? 2 (cant-elementos-pixhex (car imagen-descontruida)))
       (cond
         [(eq? (es-pixhex? (car imagen-descontruida) 0) #t) (es-maphex? (cdr imagen-descontruida))]
         (else #f))]
      (else #f))))

;(imagen 2 2 (pixhex-d  0 0 "#AF00AA" 10) (pixhex-d  0 1 "#FF00FF" 20) (pixhex-d 1 0 "#00AAEE" 30) (pixhex-d 1 1 "#DBFF00" 4))
;(maphex? (imagen 2 2 (pixhex-d  0 0 "#AF00AA" 10) (pixhex-d  0 1 "#FF00FF" 20) (pixhex-d 1 0 "#00AAEE" 30) (pixhex-d 1 1 "#DBFF00")))

;Descripcion: funcion envoltorio de compressed-envo
;Dominio: Imagen
;Recorrido: Boolean

(define compressed?
  (lambda (image)
    compressed-envo (descontructor image) (get-cant-col imagen) (get-cant-fil imagen) 0))

;Descripcion: funcion que determina si una imagen esta comprimida o no. Para hacerlo cuenta los elementos que hay
;en la imagen, si estos son iguales a producto de "* fil col" ebtoces no esta comprimida y retorna #f, en caso contrario
;retornara un #f
;Dominio: Imagen X Num X Num X Num
;Recorrido: Boolean
;Tipo de recursion: Recursion de cola

(define compressed-envo
  (lambda (imagen fil col contador)
    (cond
      [(null? imagen)
       (cond
         [((< contador (* fil col)) #t)];si la cantidad de elementos que hay en la imagen es menor al las que forman * fil col, entoces esta comprimida (#t)
         (else #f))] ;si la cantidad es igual, entonces es una imagen sin modificar
      (else (compressed-envo (cdr imagen) (+ 1 contador)))))) ;suma los elementos de la lista de pixeles
  
;Descripcion: funcion envoltorio de flipV-envo
;Dominio: List
;Recorrido: Imagen

(define flipV ;funcion flipV-> recive la imagen e invierte las filas
  (lambda (imagen)
    (flipV-envo imagen '())))

;Descripcion: funcion que invierte el orden de las filas haciendo uso de la recursion de cola. Se pasan las filas de
;la imagen original a una imagen aux, al hacerlo con recursion de cola lo que antes era la primera fila en la nueva
;imagen sera la ultima y asi sucesivamente, hasta retornar una image invertida de forma no declarativa
;Dominio: Imagen X List
;Recorrido: Imagen
;Tipo de recursion: Recursion de cola

(define flipV-envo
  (lambda (imagen flip-list)
    (cond
      [(null? imagen) flip-list]; retorno la nueva imagen invertida
      (else (flipV-envo (cdr imagen) (cons (car imagen) flip-list)))))); paso lo elementos a una nueva imagen

;Descripcion: funcion envoltorio de get-filas
;Dominio: Imagen
;Recorrido: Imagen

(define flipH
  (lambda (imagen)
    (get-filas imagen '())))

;Descripcion: funcion que invierte las filas de una imagen, al hacerlo llamara a la funcion flipV-envo que hara el trabajo de
;invertir la imagen final.
;Dominio: Imagen X List
;Recorrido: Imagen
;Tipo de recursion: de cola

(define get-filas;;get
  (lambda (imagen flip-list)
          (cond
            [(null? imagen) (flipV-envo flip-list '())];;llamo a la funcion flipV, esta me invierte la lista
            (else (get-filas (cdr imagen) (cons (flipH-invierte-filas (car imagen) '()) flip-list))))))

;Descripcion: funcion que invierte los elementos de las filas, uno a uno, haciendo uso de la recursion de cola.
;Dominio: Lista X List
;Recorrido: Lista
;Tipo de recursion: de cola

(define flipH-invierte-filas
  (lambda (filas list-aux)
    (cond
      [(null? filas) list-aux];retorna mi lista con los elementos invertidos
      (else (flipH-invierte-filas (cdr filas) (cons (car filas) list-aux)))))); copia los elementos de la lista de forma invertida

;---getters---;

;Descripcion: obtiene la cantidad de columnas que hay
;Dominio: Imagen
;Recorrido: Num
;Tipo de recursion: Natural

(define get-cant-col;;get cololumnas ***
  (lambda (imagen)
    (cond
      [(null? imagen) 0]
      (else (+ 1 (get-cant-col (cdr imagen))))))); suma 1 mientras no sea NULL
;recorre la imagen fila a fila, de esa forma retornara la cantidad de columnas que hay.

;Descripcion: obtiene la cantidad de filas que hay, haciendo uso de la funcion "get-cant-col"
;Dominio: Imagen
;Recorrido: Num

(define get-cant-fil;;get filas ***
  (lambda (imagen)
    (get-cant-col (car imagen)))) ; le doy la fila entera de imagen ej: ((1 1)(1 0)(0 0))-> de esta froma recorre es lista
;y retorna la cantidad de elementos que hay en una fila.

    
;;(flipV (constructor-imagen 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4)))
;;(flipH (constructor-imagen 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4)))
;;(flipH (constructor-imagen 3 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 0 1 1 49) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4) (pixbit-d 1 2 1 50)))


;;funcion que recorta el cuadrante de una imagen y retorna una imagen recortada

;Descripcion: funcion que selecciona una cuadrante y retorna los pixeles que hay en dicho cuadrante
;Dominio: Imagen
;Recorrido: Imagen
;Tipo de recursion: envoltorio de la funcion toma-pixeles

(define crop
  (lambda (imagen x1 y1 x2 y2);X es Y e Y es X, esta al revez
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



;;(crop img1 0 1 1 1)

;;funcion que toma un pixel y lo tranforma a un hex

;(provide get-R)
;(provide get-G)
;(provide get-B)

;Descripcion: funcion que tranforma una imagen con representacion de pixeles pixrgb a una imagen
;representada por hexpix, haciendo uso de funciones del "TDA-pixrgb-d"
;Dominio: Imagen
;Recorrido: Imagen
;Tipo de recursion: envoltorio de la funcion "get-filas-rgb"

(define imgRGB->imgHex
  (lambda (imagen)
    (get-filas-rgb imagen '())));;funion que obtiene las filas de una imagen

;Descripcion: funcion que extare las filas de una imagen pixmap y crea una nueva imagen con una representacion de hexmap
;donde los pixeles de la imagen son son pixhex.
;Dominio: Imagen X List
;Recorrido: Imagen
;Tipo de recursion: de cola

(define get-filas-rgb
  (lambda (imagen lista-temp)
    (cond
      [(null? imagen) (reverse lista-temp)]; la invierte producto de la recursion de cola
      (else (get-filas-rgb (cdr imagen) (cons (recorre-pixeles (car imagen) '()) lista-temp)))))); toma la fila de una imagen

;Descripcion: funcion que pasa tranforma un pixel rgbpix a un pixel pixhex, tranformando las componentes
;de RGB a hex, haciendo uno de las funciones alojadas en "TDA-Pixrgb-d".
;donde los pixeles de la imagen son son pixhex.
;Dominio: List X List
;Recorrido: List
;Tipo de recursion: de cola

(define recorre-pixeles
  (lambda (fila lista)
    (cond
      [(null? fila) (reverse lista)] ;retorma una lista o fila con pixhex
      (else (recorre-pixeles (cdr fila) (cons (cons (string-append "#" ;extrae las componentes de la imagen y la tranforma a numeros hex, luego une las componenetes formando un pixhex
      (string-append (car (resto-num (get-R (car fila)) '())) (string-append (car (resto-num (get-G (car fila)) '())) (car (resto-num (get-B (car fila)) '()))))) (list (get-D (car fila)))) lista))))))


;;(imgRGB->imgHex (constructor-imagen 2 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40)))

;;funcion que unifica todas las columnas, dejando una imagen de una columna y una fila con conpuesta por todas las columnas
;;recursion natural

;Descripcion: funcion que deconstruye una imagen a una lista de pixeles, pasa lso pixeles de la imagen a una lista,
;perdiendo el formato dado por la funcion "image"
;donde los pixeles de la imagen son son pixhex.
;Dominio: Image
;Recorrido: List
;Tipo de recursion: Natural

(define descontructor
  (lambda (imagen)
    (cond
     [(null? imagen) '()]; caso base
     (else (append (get-filas-def (car imagen)) (descontructor (cdr imagen))))))); llama a la funcion "gets-filas-def" y une el contenido a una nueva lista

;Descripcion: funcion que recive una fila y retorna una lista con los pixeles
;Dominio: List
;Recorrido: List
;Tipo de recursion: Natural

(define get-filas-def
  (lambda (fila-pixeles)
    (cond
      [(null? fila-pixeles) '()] ;caso base
      (else (cons (car fila-pixeles) (get-filas-def (cdr fila-pixeles))))))); los disuelve las filas y crea una lista

;;elimina la profundidad

;Descripcion: funcio que elimina la profuncidad de una imagen, eliminando el ultimo elemento de un pixel
;Dominio: List
;Recorrido: List
;Tipo de recursion: Natural

(define filtro-d
  (lambda (imagen-descontruida)
    (cond
      [(null? imagen-descontruida) '()]
      [else (cons (remove (last (car imagen-descontruida)) (car imagen-descontruida)) (filtro-d (cdr imagen-descontruida)))])))


;Descripcion: funcio que llama a la funcion "filtro-d" y le entrega una imagen deconstruida
;Dominio: Imagen
;Recorrido: List

(define usa-filtros-descontruidos
  (lambda (imagen)
    (filtro-d(descontructor imagen)))); le aplica la funcion deconstructor y se lo pasa como parametro a la funcion "filtro-d"

;;(constructor-imagen 2 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40))

;;histograma

(define histograma
  (lambda (imagen)
    (histograma-envo (filtro-d(descontructor imagen)) '())))

(define histograma-envo
  (lambda (imagen histo-imagen)
    (cond
      [(null? imagen) histo-imagen]
      [(repetidos (car imagen) histo-imagen) (histograma-envo (cdr imagen) (cons (obtiene-frecuencias (car imagen) imagen '() 0) histo-imagen))]
       (else (histograma-envo (cdr imagen) histo-imagen)))))

(define obtiene-frecuencias
  (lambda (pixel imagen aux contador)
    (cond
      [(null? imagen) (append pixel (cons contador aux))]
      [(equal? pixel (car imagen)) (obtiene-frecuencias pixel (cdr imagen) aux (+ 1 contador))]
      (else (obtiene-frecuencias pixel (cdr imagen) aux contador)))))

(define repetidos
  (lambda (pixel histograma)
    (cond
      [(null? histograma) #t]
      [(null? pixel) #t]
      [(equal? (car pixel) (car (car histograma))) #f]
      (else (repetidos pixel (cdr histograma))))))

(define img1 '(((1 10) (0 20))
               ((0 30) (1 4))
               ((0 50) (1 60))
               ((1 80) (1 90))))

(define img4 '(((10 10 10 10) (20 20 20 20))
               ((30 30 30 30) (40 40 40 40))))

;;funcion rotate90

(define rotate90
  (lambda (imagen)
    (envo90 imagen (get-cant-col imagen) (get-cant-fil imagen))))

(define envo90
  (lambda (imagen col fil)
    (envo-rotate90 (descontructor imagen) (descontructor imagen) '() '() col fil 0 0 0 0)))

(define envo-rotate90
  (lambda (imagen imagen-copia imagen-aux temp cant-col cant-fil fil col aux contador)
    (cond
      [(eq? cant-fil col) (reverse imagen-aux)]
      [(null? imagen) (envo-rotate90 imagen-copia imagen-copia (cons temp imagen-aux) '() cant-col cant-fil (+ 1 aux) (+ 1 col) (+ 1 aux) 0)]
      [(eq? fil contador) (envo-rotate90 (cdr imagen) imagen-copia imagen-aux (cons (car imagen) temp) cant-col cant-fil (+ fil cant-fil) col aux (+ 1 contador))]
      (else (envo-rotate90 (cdr imagen) imagen-copia imagen-aux temp cant-col cant-fil fil col aux (+ 1 contador))))))


;;edit

;(define edit
  ;(lambda (funcion imagen)
    ;(map funcion (descontructor imagen))))


(define edit
  (lambda (funcion imagen)
    (envo-edit imagen funcion (get-cant-fil imagen) (get-cant-col imagen))))

(define envo-edit
  (lambda (imagen funcion fil col)
    (contru-imagenv2 (map funcion (descontructor imagen)) '() '() fil col 0 0)))

(define contru-imagenv2
  (lambda (imagen-descontru aux new-imagen cant-fil cant-col fil col)
    (cond
      ;[(null? imagen-descontru) (reverse new-imagen)]
      [(eq? cant-col col) (reverse new-imagen)]
      [(eq? cant-fil fil) (contru-imagenv2 imagen-descontru '() (cons (reverse aux) new-imagen) cant-fil cant-col 0 (+ 1 col))]
      (else (contru-imagenv2 (cdr imagen-descontru) (cons (car imagen-descontru) aux) new-imagen cant-fil cant-col (+ 1 fil) col)))))


(define invertColorBit;;funciona bien
  (lambda (pixbit-d)
    (cond
      [(eq? (car pixbit-d) 1) (cons 0 (remove (car pixbit-d) pixbit-d))]
      (else (cons  1 (remove (car pixbit-d) pixbit-d))))))

(define invertColorRGB
  (lambda (pixrgb-d)
    (cons (- 255 (car pixrgb-d)) (cons (- 255 (second pixrgb-d)) (cons (- 255 (third pixrgb-d)) (cons (last pixrgb-d)'()))))))

;;(edit invertColorBit img1)

;(define constru-imagen
  ;(lambda (pixeles imagen lista-aux fil col cant-f cant-c)


;;constru-imagen (car (cddr pixeles)) '() '() fil col 0 0


;;---image->string---;;

(define imagen->string
  (lambda (imagen funcion)
    (envo-imagen->string (get-cant-fil imagen) (get-cant-col imagen) (map funcion (descontructor (flipV (flipH imagen)))))))

(define envo-imagen->string;;esto se podria eliminar
  (lambda (fil col imagen-string)
    (make-string imagen-string "" "" fil col 0 0)))

(define make-string ;;caso en que es un bit-map
  (lambda (imagen string-imagen string-aux fil col cant-fil cant-col)
    (cond
      [(eq? cant-col col) string-imagen]
      [(eq? cant-fil fil) (make-string imagen (string-append string-aux (string-append "\n" string-imagen)) "" fil col 0 (+ 1 cant-col))]
      (else (make-string (cdr imagen) string-imagen (string-append (car imagen) string-aux) fil col (+ 1 cant-fil) cant-col)))))


;;---getters and filter---;;

(define pixbit->string ;;pasarla al TDA pixbit, es de ahi
  (lambda (pixbit-d)
    (string-append(number->string (first pixbit-d)) "\t")))

(define pixrgb->string
  (lambda (pixrgb-d)
    (string-append "#" (string-append (car (resto-num (get-R pixrgb-d) '())) (string-append (car (resto-num (get-G pixrgb-d) '())) (string-append (car (resto-num (get-B pixrgb-d) '()))) "\t"))))) 

(define pixhex->string ;;pasarla al TDA pixbit, es de ahi
  (lambda (pixhex-d)
    (string-append (first pixhex-d) (string-append (number->string (second pixhex-d))  "\t"))))


(define img12 (imagen 2 2 (pixrgb-d 0 0 255 0 0 10) (pixrgb-d 0 1 0 255 0 20)(pixrgb-d 1 0 0 0 255 10)(pixrgb-d 1 1 255 255 255  1)))


;;(display(imagen->string img1 pixbit->string))
;;(display(imagen->string img1 pixrgb->string))
;(imagen->string img1 string->pixbit)



(define depthLayers
  (lambda (imagen)
    (depthLayers-seleciona imagen (descontructor imagen) '() '() (que-pixel-es? (car (descontructor imagen))))))

(define depthLayers-seleciona
  (lambda (imagen image-decons lista-imagenes lista-revisados tipo-pixel)
    (cond
      [(null? image-decons) lista-imagenes]
      [(eq? #f (revisado-depth (car image-decons) lista-revisados)) (depthLayers-seleciona imagen (cdr image-decons)
                                                                                           (cons (crea-imagen-depth imagen '() (car image-decons) tipo-pixel) lista-imagenes) (cons (car image-decons) lista-revisados) tipo-pixel)]
      (else (depthLayers-seleciona imagen (cdr image-decons) lista-imagenes lista-revisados tipo-pixel)))))

(define crea-imagen-depth
  (lambda (imagen new-image profundidad tipo-pixel)
    (cond
      [(null? imagen) (reverse new-image)]
      (else (crea-imagen-depth (cdr imagen) (cons (crea-filas-d (car imagen) '() profundidad tipo-pixel) new-image) profundidad tipo-pixel)))))

(define crea-filas-d
  (lambda (fila lista-aux profundidad tipo-pixel)
    (cond
      [(null? fila) (reverse lista-aux)]
      [(equal? (last profundidad) (last (car fila))) (crea-filas-d (cdr fila) (cons (car fila) lista-aux) profundidad tipo-pixel)]
      (else (crea-filas-d (cdr fila) (cons tipo-pixel lista-aux) profundidad tipo-pixel)))))

(define revisado-depth
  (lambda (pixel lista-pixeles)
    (cond
      [(null? lista-pixeles) #f]
      [(eq? (last pixel) (last (car lista-pixeles))) #t]
      (else (revisado-depth pixel (cdr lista-pixeles))))))

;(depthLayers img1)

(define img5 '(((1 10) (0 10))
               ((0 10) (1 10))
               ((0 10) (1 10))
               ((1 10) (1 10))))

(define img50 '(((1 10) (0 10))
               ((0 20) (1 20))))

(define que-pixel-es?
  (lambda (pixel)
    (cond
      [(eq? (cant-elementos-pixhex pixel) 2)
       (cond
         [(number? (car pixel)) (list 1)]
         (else (list "#FFFFFF")))]
      (else (list 255 255 255)))))



