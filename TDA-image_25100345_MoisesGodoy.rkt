#lang racket

(require "TDA-pixbit-d_25100345_MoisesGodoy")
(require "TDA-pixrgb-d_25100345_MoisesGodoy")
(require "TDA-pixhex-d_25100345_MoisesGodoy")

(provide image)
(provide bitmap?)
(provide pixmap?)
(provide hexmap?)
(provide compressed?)
(provide flipH)
(provide flipV)
(provide crop)
(provide imgRGB->imgHex)
(provide histogram)
(provide rotate90)
(provide edit)
(provide invertColorBit)
(provide invertColorRGB)
(provide image->string)
(provide depthLayers)
(provide pixbit->string)
(provide pixrgb->string)
(provide pixhex->string)




;Descripcion: imagen, recibe los datos del largo y ancho de la imagen, crea los pixeles de la imagen y guarda los datos en una lista,
;luego llama a la funcion envo-imagen, que da forma y contruye la imagen final.
;Dominio: Num X Num X [pixbit-d |  pixrgb-d | pixhex-d]
;Recorrido: Image

(define image
  (lambda (largo ancho  . imagen); recibe n parametros despues del punto
    (envo-imagen (list largo ancho imagen) (get-fil (list largo ancho imagen)) (get-col (list largo ancho imagen)))));;list largo ancho  imagen

;;(imagen 2 2 (pixbit-d  0 0 1 10) (pixbit-d  0 1 0 20) (pixbit-d 1 0 0 30) (pixbit-d 1 1 1 4))
;;(imagen 2 2 (pixbit-d  0 1 0 20) (pixbit-d  0 0 1 10) (pixbit-d 1 1 1 4) (pixbit-d 1 0 0 30))
;;(imagen 3 2 (pixbit-d 1 2 1 99) (pixbit-d  0 1 0 20) (pixbit-d  0 0 1 10) (pixbit-d 1 1 1 4) (pixbit-d 1 0 0 30) (pixbit-d 0 2 0 39))

;;---------- Funciones Getters ----------;;

;Descripcion: funcion que obtiene el ancho de la imagen
;Dominio: List
;Recorrido: Num

(define get-fil ;;seclectores 
  (lambda (constructor-imagen)
    (car constructor-imagen)))

;Descripcion: funcion que obtiene el largo de la imagen
;Dominio: List
;Recorrido: Num

(define get-col ;;selectores 
  (lambda (constructor-imagen)
    (car (cdr constructor-imagen))))

;Descripcion: funcion envoltorio de imagen que llama a la funcion que construye la imagen final
;Dominio: pixeles X num X num
;Recorrido: image

(define envo-imagen
  (lambda (pixeles fil col)
    (constru-imagen (car (cddr pixeles)) '() '() fil col 0 0)))

;Descripcion: funcnion que construye una imagen apartir de los datos extraidos,
;representa la imagen como una matriz, compuesta por filas y columnas.
;Dominio: List X List X List X Num X Num X Num X Num X Num
;Recorrido: image
;Tipo de recursion: recursion de cola

(define constru-imagen
  (lambda (pixeles imagen lista-aux fil col cant-f cant-c)
    (cond
      [(eq? cant-f fil) (constru-imagen pixeles (cons (reverse lista-aux) imagen) '() fil col 0 (+ 1 cant-c))]; reinicio la lista "aux", agrego sus datos a la lista imagen, reinicio "cant-fil" y sumo una a "cant-col"
      [(eq? cant-c col) (reverse imagen)]
      (else (constru-imagen (cdr pixeles) imagen (cons (get-pixel-datos(car pixeles)) lista-aux) fil col (+ 1 cant-f) cant-c)))));guardo los pixeles en la lista "aux" y suma 1 a "cant-fil"

;Descripcion: getters que extrar el pixel y la dimencion, sin incluir la posicion del bit en la imagen.
;Dominio: List
;Recorrido: List

(define get-pixel-datos;;selector. cambiar el nombre
  (lambda (pixeles)
    (cddr pixeles)))

;;-----Funciones de pertenecia: Bitmap, Pixmap? y hexmap.-----;;

;Descripcion: funcion envoltorio de es-bitmap?
;Recorrido: Image
;Dom: Boolean

(define bitmap?
  (lambda (imagen)
    (es-bitmap? (deconstructor imagen)))); cambia el formato de la imagen, dejando todos los pixeles en una lista -> '((0 10)(1 20)(1 40)(1 8))

;Descripcion: Funcion que determina si es un bitmap o no. esta recorre toda la imgen y determina si los pixeles
;corresponden a un pixbit, si llega al final de la imagen, significa que la imagen es un bitmap
;y retorna #t de no ser asi retorna #f.
;Dominio: List
;Recorrido: Boolean
;Tipo de recursion: Recursion de cola

(define es-bitmap?
  (lambda (imagen-deconstruida)
    (cond
      [(null? imagen-deconstruida) #t]
      [(eq? #f (es-pixbit? (car imagen-deconstruida))) #f]
      (else (es-bitmap? (cdr imagen-deconstruida))))))

;Descripcion: funcion envoltorio de es-pixmap?
;Recorrido: Image
;Dominio: Boolean

(define pixmap?
  (lambda (imagen)
    (es-pixmap? (deconstructor imagen))))

;Descripcion: Funcion que determina si es un pixmap o no. esta recorre toda la imgen y determina si los pixeles
;corresponden a un pixrgb, si llega al final de la imagen, significa que la imagen es un pixmap
;y retorna #t de no ser asi retorna #f.
;Dominio: List
;Recorrido: Boolean
;Tipo de recursion: Recursion de cola

(define es-pixmap?
  (lambda (imagen-deconstruida)
    (cond
      [(null? imagen-deconstruida) #t]
      [(eq? #f (es-pixrgb? (car imagen-deconstruida) 0 (cant-elementos-pixrgb (car imagen-deconstruida)))) #f]
      (else (es-pixmap? (cdr imagen-deconstruida))))))

;Descripcion: funcion envoltorio de es-maphex?
;Recorrido: Image
;Dominio: Boolean

(define hexmap?
  (lambda (imagen)
    (es-hexmap? (deconstructor imagen))))

;Descripcion: Funcion que determina si es un pixrgb o no. esta recorre toda la imgen y determina si los pixeles
;corresponden a un pixmap, si llega al final de la imagen, significa que la imagen es un pixmap
;y retorna #t de no ser asi retorna #f.
;Dominio: List
;Recorrido: Boolean
;Tipo de recursion: Recursion de cola

(define es-hexmap?
  (lambda (imagen-deconstruida)
    (cond
      [(null? imagen-deconstruida) #t]
      [(eq? 2 (cant-elementos-pixhex (car imagen-deconstruida)))
       (cond
         [(eq? (es-pixhex? (car imagen-deconstruida) 0) #t) (es-hexmap? (cdr imagen-deconstruida))]
         (else #f))]
      (else #f))))

;(define img555 (imagen 2 2 (pixhex-d  0 0 "#AF00AA" 10) (pixhex-d  0 1 "#FF00FF" 20) (pixhex-d 1 0 "#00AAEE" 30) (pixhex-d 1 1 "#DBFF00" 4)))
;(maphex? (imagen 2 2 (pixhex-d  0 0 "#AF00AA" 10) (pixhex-d  0 1 "#FF00FF" 20) (pixhex-d 1 0 "#00AAEE" 30) (pixhex-d 1 1 "#DBFF00")))

;Descripcion: funcion envoltorio de compressed-envo
;Dominio: Image
;Recorrido: Boolean

(define compressed?
  (lambda (imagen)
    (compressed-envo (deconstructor imagen) (get-cant-col imagen) (get-cant-fil imagen) 0)))

;Descripcion: funcion que determina si una imagen esta comprimida o no. Para hacerlo cuenta los elementos que hay
;en la imagen, si estos son iguales a producto de "* fil col" ebtoces no esta comprimida y retorna #f, en caso contrario
;retornara un #f
;Dominio: Image X Num X Num X Num
;Recorrido: Boolean
;Tipo de recursion: Recursion de cola

(define compressed-envo
  (lambda (imagen fil col contador)
    (cond
      [(null? imagen)
       (cond
         [(< (* fil col) contador) #t];si la cantidad de elementos que hay en la imagen es menor al las que forman * fil col, entoces esta comprimida (#t)
         (else #f))] ;si la cantidad es igual, entonces es una imagen sin modificar
      (else (compressed-envo (cdr imagen) fil col (+ 1 contador)))))) ;suma los elementos de la lista de pixeles
  
;Descripcion: funcion envoltorio de flipV-envo
;Dominio: List
;Recorrido: Image

(define flipV ;funcion flipV-> recive la imagen e invierte las filas
  (lambda (imagen)
    (flipV-envo imagen '())))

;Descripcion: funcion que invierte el orden de las filas haciendo uso de la recursion de cola. Se pasan las filas de
;la imagen original a una imagen aux, al hacerlo con recursion de cola lo que antes era la primera fila en la nueva
;imagen sera la ultima y asi sucesivamente, hasta retornar una image invertida de forma no declarativa
;Dominio: Imagen X List
;Recorrido: Image
;Tipo de recursion: Recursion de cola

(define flipV-envo
  (lambda (imagen flip-list)
    (cond
      [(null? imagen) flip-list]; retorno la nueva imagen invertida
      (else (flipV-envo (cdr imagen) (cons (car imagen) flip-list)))))); paso lo elementos a una nueva imagen

;Descripcion: funcion envoltorio de get-filas
;Dominio: Image
;Recorrido: Image

(define flipH
  (lambda (imagen)
    (get-filas imagen '())))

;Descripcion: funcion que invierte las filas de una imagen, al hacerlo llamara a la funcion flipV-envo que hara el trabajo de
;invertir la imagen final.
;Dominio: Image X List
;Recorrido: Image
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
;Dominio: Image
;Recorrido: Num
;Tipo de recursion: Natural

(define get-cant-col;;get cololumnas ***
  (lambda (imagen)
    (cond
      [(null? imagen) 0]
      (else (+ 1 (get-cant-col (cdr imagen))))))); suma 1 mientras no sea NULL
;recorre la imagen fila a fila, de esa forma retornara la cantidad de columnas que hay.

;Descripcion: obtiene la cantidad de filas que hay, haciendo uso de la funcion "get-cant-col"
;Dominio: Image
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
;Dominio: Image
;Recorrido: Image
;Tipo de recursion: envoltorio de la funcion toma-pixeles

(define crop
  (lambda (imagen x1 y1 x2 y2);X es Y e Y es X, esta al revez -> x1 y1 x2 y2
    (toma-pixeles imagen y1 x1 y2 x2 '() (get-cant-col imagen) (get-cant-fil imagen) 0 0)))

(define toma-pixeles
  (lambda (imagen x1 y1 x2 y2 recorte fil col cant-col cant-fil)
    (cond
      [(< y2 cant-col) (reverse recorte)]
      [(null? imagen) recorte]
      [(<= y1 cant-col) (toma-pixeles (cdr imagen) x1 y1 x2 y2 (cons (get-puntos (car imagen) '() x1 x2 fil 0) recorte) fil col (+ 1 cant-col) 0)]
      (else (toma-pixeles (cdr imagen) x1 y1 x2 y2 recorte fil col (+ 1 cant-col) cant-fil)))))

(define get-puntos
  (lambda (fila recorte x1 x2 fil cant-fil)
    (cond
      [(< x2 cant-fil) (reverse recorte)]
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
;Dominio: Image
;Recorrido: Image
;Tipo de recursion: envoltorio de la funcion "get-filas-rgb"

(define imgRGB->imgHex
  (lambda (imagen)
    (get-filas-rgb imagen '())));;funion que obtiene las filas de una imagen

;Descripcion: funcion que extare las filas de una imagen pixmap y crea una nueva imagen con una representacion de hexmap
;donde los pixeles de la imagen son son pixhex.
;Dominio: Image X List
;Recorrido: Image
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

(define deconstructor;;deconstructor <- descontructor
  (lambda (imagen)
    (cond
     [(null? imagen) '()]; caso base
     (else (append (get-filas-def (car imagen)) (deconstructor (cdr imagen))))))); llama a la funcion "gets-filas-def" y une el contenido a una nueva lista

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
  (lambda (imagen-deconstruida)
    (cond
      [(null? imagen-deconstruida) '()]
      [else (cons (remove (last (car imagen-deconstruida)) (car imagen-deconstruida)) (filtro-d (cdr imagen-deconstruida)))])))


;Descripcion: funcion que llama a la funcion "filtro-d" y le entrega una imagen deconstruida
;Dominio: Image
;Recorrido: List

(define usa-filtros-deconstruidos;-----
  (lambda (imagen)
    (filtro-d(deconstructor imagen)))); le aplica la funcion deconstructor y se lo pasa como parametro a la funcion "filtro-d"

;;(constructor-imagen 2 2 (pixrgb-d  0 0 10 10 10 10) (pixrgb-d  0 1 20 20 20 20) (pixrgb-d 1 0 30 30 30 30) (pixrgb-d 1 1 40 40 40 40))

;;histograma

;Descripcion: funcion envoltorio de histograma-envo
;Dominio: Image
;Recorrido: List

(define histogram
  (lambda (imagen)
    (histograma-envo (filtro-d(deconstructor imagen)) '())))

;Descripcion: funcion que lee la imagen y obtiene la cantidad de repeticiones que tiene todos los pixeles
;retornando en una lista los pixeles y su frecuencia.
;Dominio: Imagen X List.
;Recorrido: List.
;Tipo de Recursion: Recursion de cola.

(define histograma-envo
  (lambda (imagen histo-imagen)
    (cond
      [(null? imagen) histo-imagen]
      [(repetidos (car imagen) histo-imagen) (histograma-envo (cdr imagen) (cons (obtiene-frecuencias (car imagen) imagen '() 0) histo-imagen))]
       (else (histograma-envo (cdr imagen) histo-imagen)))))

;Descripcion: funcion que obtiene la cantidad de repeticiones que tiene un pixel, sin contar los que ya ha comparado,
;para ello usa una funcion axuliar que determina si ya se comparo o no.
;Dominio: List X Imagen X List X Num.
;Recorrido: List.
;Tipo de Recursion: Recursion de cola.

(define obtiene-frecuencias
  (lambda (pixel imagen aux contador)
    (cond
      [(null? imagen) (append pixel (cons contador aux))]
      [(equal? pixel (car imagen)) (obtiene-frecuencias pixel (cdr imagen) aux (+ 1 contador))]
      (else (obtiene-frecuencias pixel (cdr imagen) aux contador)))))

;Descripcion: funcion que recibe un pixel y determina si el pixel ya ha sido leido en la lista del histograma.
;Dominio: List X List.
;Recorrido: List.
;Tipo de Recursion: Recursion de cola.

(define repetidos
  (lambda (pixel histograma)
    (cond
      [(null? histograma) #t]; si el NULL significa que no hay ningun pixel en esta lista
      [(null? pixel) #t]; si llega hasta aca entoces no hay pixeles repetidos
      [(equal? (car pixel) (car (car histograma))) #f]; hay un pixel repitedi, entoces se retorna #f y se busca otro
      (else (repetidos pixel (cdr histograma))))))

(define img111 '(((1 10) (0 20))
               ((0 30) (1 4))
               ((0 50) (1 60))
               ((1 80) (1 90))))

(define img444 '(((10 10 10 10) (20 20 20 20))
               ((30 30 30 30) (40 40 40 40))))

;;funcion rotate90

;Descripcion: funcion que envoltorio de "envo90", esta funcion retorna una imagen rotada 90 grados a la derecha
;Dominio: Image
;Recorrido: Image

(define rotate90
  (lambda (imagen)
    (envo90 imagen (get-cant-col imagen) (get-cant-fil imagen))))

;Descripcion: funcion envoltorio de "envo-rotate90", esta funcion envia los parametros necesarios para
;rotar la imagen, al final retorna una imagen rotada
;Dominio: Image
;Recorrido: List

(define envo90
  (lambda (imagen col fil)
    (envo-rotate90 (deconstructor imagen) (deconstructor imagen) '() '() col fil 0 0 0 0))) ;contado filas, contador columnas, auxiliar y contador o iterador

;Descripcion: funcion que rota ima unagen 90 grados a la derecha.
;Funcionamiento: esta funcion recibe dos imagenes deconstruidas, donde una sirve de copia, mientras que la otra es la que se recorre,
;al llegar esta a NULL, se reemplaza por la copia. Para rotar la imagen se uso rompio el formato dado por "image" y paso a ser una lista de pixeles
;de esta forma se recorre la lista y tomara elementos en la posicion "contador + filas", asi hasta llegar a NULL, en ese caso se llamara a la funcion
;con la imagen copia y "(contador= aux) + filas" con esta formula seguira recorriendo la imagen y tomando los elementos correspondientes para
;formas uan imagen rotada a la derecha, hasta que "cant-fil = col".
;Dominio: Image.
;Recorrido: Image.

(define envo-rotate90
  (lambda (imagen imagen-copia imagen-aux temp cant-col cant-fil fil col aux contador)
    (cond
      [(eq? cant-fil col) (reverse imagen-aux)]; si es igual entoces ya reviso la lista y invierte la imagen
      [(null? imagen) (envo-rotate90 imagen-copia imagen-copia (cons temp imagen-aux) '() cant-col cant-fil (+ 1 aux) (+ 1 col) (+ 1 aux) 0)]
      ;al llegar a null vuelve a leer la imagen desde el incio, pero ahora buscara los elementos contador + 1
      [(eq? fil contador) (envo-rotate90 (cdr imagen) imagen-copia imagen-aux (cons (car imagen) temp) cant-col cant-fil (+ fil cant-fil) col aux (+ 1 contador))]
      ;va sumando uno al contador, hasta que este sea igual a la cantidad de filas, y toma el elemento que esta en esa posicion y pasa a formar parte de una fila
      (else (envo-rotate90 (cdr imagen) imagen-copia imagen-aux temp cant-col cant-fil fil col aux (+ 1 contador))))))

;;edit

;(define edit
  ;(lambda (funcion imagen)
    ;(map funcion (descontructor imagen))))


;Descripcion: fucnion envoltorio que recibe una funcion de entrada y aplica esa fucnion a los elementos de la imagen.
;Dominio: Image
;Recorrido: List

(define edit
  (lambda (funcion imagen)
    (edit-image imagen funcion (get-cant-fil imagen) (get-cant-col imagen))))

;Descripcion: funcion que aplica un map con la funcion ingresada como parametro y retorna una nueva imagen editada.
;Dominio: Image X Funtion X Num X Num.
;Recorrido: Image.

(define edit-image
  (lambda (imagen funcion fil col)
    (contru-imagenv2 (map funcion (deconstructor imagen)) '() '() fil col 0 0))); mapeo la funcion ingresada como parametro, de esa forma edito la imagen, luego la reconstruyo

;Descripcion: funcion que reconstruye una imagen editada y retorna una imagen con su formato original y editada.
;Dominio: List X List X List X Num X Num X Num X Num.
;Recorrido: Image.
;Tipo de Recursion: Recursion de cola.

(define contru-imagenv2
  (lambda (imagen-descontru aux new-imagen cant-fil cant-col fil col)
    (cond
      ;[(null? imagen-descontru) (reverse new-imagen)]
      [(eq? cant-col col) (reverse new-imagen)]; si es igual retorna la imagen ivertida (para correguir la inversion de la recursion de cola).
      [(eq? cant-fil fil) (contru-imagenv2 imagen-descontru '() (cons (reverse aux) new-imagen) cant-fil cant-col 0 (+ 1 col))]; si la filas son igual al contador, paso a crear una columna nueva.
      (else (contru-imagenv2 (cdr imagen-descontru) (cons (car imagen-descontru) aux) new-imagen cant-fil cant-col (+ 1 fil) col)))))

;Funcion: fucnion que edita los bits de ua imagen compuesta por pixbit, invierte los 0 por 1 y retorna un pixbit-d editado
;Dominio: Pixbit-d.
;Recorrido: Pixbit-d.

(define invertColorBit;;funciona bien
  (lambda (pixbit-d)
    (cond
      [(eq? (car pixbit-d) 1) (cons 0 (remove (car pixbit-d) pixbit-d))]; si es un 1, lo cambio por un 0 y creo un nuevo pixel.
      (else (cons  1 (remove (car pixbit-d) pixbit-d)))))); caso contrario.

;Funcion: fucnion que edita los Pixrgb de ua imagen compuesta por Pixrgb, esta funcion invierte las componenetes del RGB
;restandole 255 al pixel actual y "creando" un nuevo Pixrgb.
;Dominio: Pixrgb-d.
;Recorrido: Pixrgb-d.

(define invertColorRGB
  (lambda (pixrgb-d)
    (cons (- 255 (car pixrgb-d)) (cons (- 255 (second pixrgb-d)) (cons (- 255 (third pixrgb-d)) (cons (last pixrgb-d)'())))))); le resto el pixel a 255 y luego los uno con las demas componentes.

;;(edit invertColorBit img1)

;(define constru-imagen
  ;(lambda (pixeles imagen lista-aux fil col cant-f cant-c)


;;constru-imagen (car (cddr pixeles)) '() '() fil col 0 0


;;---image->string---;;

;Funcion: funcion que crea una representacion de una imagen en formato string, recibe una imagen
;y una funcion que determinara como tranformar los bits de la imagen.
;Dominio: Image.
;Recorrido: String.

(define image->string
  (lambda (imagen funcion)
    (envo-imagen->string (get-cant-fil imagen) (get-cant-col imagen) (map funcion (deconstructor (flipV (flipH imagen)))))));aplica un map a la imagen con la funcion dada como parametro.

;Funcion: funcion envoltorio de "make-string"
;Dominio: Num X Num X Image.
;Recorrido: String.

(define envo-imagen->string;;esto se podria eliminar
  (lambda (fil col imagen-string)
    (make-string imagen-string "" "" fil col 0 0)))

;Funcion: funcion que contruye una imagen en formato string, crea una imagen con todos los saltos de linea y espacios necesarios.
;Dominio: Image.
;Recorrido: String.
;Tipo de Recursion: Recursion de cola.

(define make-string
  (lambda (imagen string-imagen string-aux fil col cant-fil cant-col)
    (cond
      [(eq? cant-col col) string-imagen]; retorna el string.
      [(eq? cant-fil fil) (make-string imagen (string-append string-aux (string-append "\n" string-imagen)) "" fil col 0 (+ 1 cant-col))];al completar la fila colola al final el salto de linea.
      (else (make-string (cdr imagen) string-imagen (string-append (car imagen) string-aux) fil col (+ 1 cant-fil) cant-col)))));una los pixelespara respetando el formato original.


;;---getters and filter---;;

;Funcion: funcion que tranforma un pixel pixbit-d a un string.
;Dominio: Pixbit-d.
;Recorrido: String.

(define pixbit->string ;;pasarla al TDA pixbit, es de ahi
  (lambda (pixbit-d)
    (string-append(number->string (first pixbit-d)) "\t")));pasa el pixel a string y le agrega un espacio al final.

;Funcion: funcion que tranforma un pixel pixrgb-d a un string.
;Dominio: Pixrgb-d.
;Recorrido: String.

(define pixrgb->string
  (lambda (pixrgb-d)
    (string-append "#" (string-append (car (resto-num (get-R pixrgb-d) '())) (string-append (car (resto-num (get-G pixrgb-d) '())) (string-append (car (resto-num (get-B pixrgb-d) '()))) "\t")))))
;separa el RGB y lo pasa a hex, luego lo une y agrega un espacio.

;Funcion: funcion que tranforma un pixel Pixhex-d a un string.
;Dominio: Pixhext-d.
;Recorrido: String.

(define pixhex->string
  (lambda (pixhex-d)
    (string-append (first pixhex-d) (string-append (number->string (second pixhex-d))  "\t")))); transforma la profundidad a string y lo une con el numero hex, luego le agrega el espacio.


(define img12 (image 2 2 (pixrgb-d 0 0 255 0 0 10) (pixrgb-d 0 1 0 255 0 20)(pixrgb-d 1 0 0 0 255 10)(pixrgb-d 1 1 255 255 255  1)))

;;(display(imagen->string img1 pixbit->string))
;;(display(imagen->string img1 pixrgb->string))
;(imagen->string img1 string->pixbit)

;Funcion: funcion que separa en niveles una imagen en base a su profundidad, dejando una lista de imagenes 2d.
;Dominio: Image.
;Recorrido: Image X List.

(define depthLayers
  (lambda (imagen)
    (depthLayers-seleciona imagen (deconstructor imagen) '() '() (que-pixel-es? (car (deconstructor imagen))))))

;Funcion: funcion que obtiene un pixel de la imagen y toma la profundidad para crear una imagen con los pixeles que compartan la profundidad de tal pixel.
;Dominio: Image X List X List X List X [Num | String].
;Recorrido: Image X List.
;Tipo de Recursion: Recursion de cola

(define depthLayers-seleciona
  (lambda (imagen image-decons lista-imagenes lista-revisados tipo-pixel)
    (cond
      [(null? image-decons) lista-imagenes]
      [(eq? #f (revisado-depth (car image-decons) lista-revisados)) (depthLayers-seleciona imagen (cdr image-decons)
                                                                                           (cons (crea-imagen-depth imagen '() (car image-decons) tipo-pixel) lista-imagenes) (cons (car image-decons) lista-revisados) tipo-pixel)]
      (else (depthLayers-seleciona imagen (cdr image-decons) lista-imagenes lista-revisados tipo-pixel)))))

;Funcion: funcion que crea un imagen en base a los pixeles que comparten profundidad con el pixel e muestra.
;Dominio: Image X List X [List X Image] X [Num | String].
;Recorrido: List
;Tipo de Recursion: Recursion de cola.

(define crea-imagen-depth
  (lambda (imagen new-image profundidad tipo-pixel)
    (cond
      [(null? imagen) (reverse new-image)]
      (else (crea-imagen-depth (cdr imagen) (cons (crea-filas-d (car imagen) '() profundidad tipo-pixel) new-image) profundidad tipo-pixel)))))

;Funcion: funcion que obtiene los pixeles que comparten con el pixel de muestra en una fila dada. 
;Dominio: Image X List X List X [Num | String]
;Recorrido: List
;Tipo de Recursion: Recursion de cola.

(define crea-filas-d
  (lambda (fila lista-aux profundidad tipo-pixel)
    (cond
      [(null? fila) (reverse lista-aux)]
      [(equal? (last profundidad) (last (car fila))) (crea-filas-d (cdr fila) (cons (remove (last (car fila)) (car fila)) lista-aux) profundidad tipo-pixel)]
      (else (crea-filas-d (cdr fila) (cons tipo-pixel lista-aux) profundidad tipo-pixel)))))

;Funcion: funcion que verifica si la profundidad dada no se haya revisado antes, esto para evitar cear imagenes con profunidades repetidas.
;Dominio: List X List.
;Recorrido: Boolean.
;Tipo de Recursion: Recursion de cola.

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

;Funcion: funcion que determina que tipo de pixel es el ingresado, para asi determinar que pixel agregar al no haber pixeles que
;comprtan el mismo nivel de profuncididad.
;Dominio: List.
;Recorrido: [Num | String].

(define que-pixel-es?
  (lambda (pixel)
    (cond
      [(eq? (cant-elementos-pixhex pixel) 2)
       (cond
         [(number? (car pixel)) (list 1)]
         (else (list "#FFFFFF")))]
      (else (list 255 255 255)))))



