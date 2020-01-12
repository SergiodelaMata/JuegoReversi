#lang racket/gui
;Se permite exportar las funciones del programa para realizar las pruebas unitarias desde un archivo externo
(provide get_lista_pos_validas
         contar_piezas
         cambiar_color
         select_oponente
         get_max_minimax
         get_max_poda)

(define alfa -inf.0)
(define beta +inf.0)
         
(define matrizInicial
  (list
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "B" "N" "-" "-" "-")
   (list "-" "-" "-" "N" "B" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   ))

; Obtiene una lista del número de piezas negras y blancas
(define jugadorMax "N") ;He quitado la lista, y que simplemente te de las piezas dependiendo del jugador que recibe
(define jugadorMin "B") ;Por parámetro

; Obtiene la lista de casillas que se encuentran de un color en la matriz
(define (get_lista_pos_validas matriz color)
  (define lista_pos_validas '())
  (for*/list ([i 8] [j 8]
    #:when(and (equal? (is_empty matriz  (list i j)) #t) (or (equal? (comprobar_fila matriz color (list i j)) #t) (equal? (comprobar_columna matriz color (list i j)) #t) (equal? (comprobar_diagonales matriz color (list i j)) #t))))
    (append lista_pos_validas (list i j))))

; Comprueba si una casilla se encuentra vacía o no
(define (is_empty matriz pos)
  (if (equal? (list-ref(list-ref matriz (car pos)) (list-ref pos 1)) "-")
     #t #f))

;Obtiene una lista de todas posiciones de las piezas blancas
(define (get_lista_pos_piezas_blancas matriz)
  (define pos_piezas_blancas '())
  (for*/list ([i 8]
             [j 8]
             #:when (equal? (list-ref (list-ref matriz i) j) "B"))
    (list i j)))
    
;Obtiene una lista de todas posiciones de las piezas negras
(define (get_lista_pos_piezas_negras matriz)
  (define pos_piezas_negras '())
  (for*/list ([i 8]
             [j 8]
             #:when (equal? (list-ref (list-ref matriz i) j)"N"))
    (list i j)))

; Cuenta el número de piezas de un color en el tablero
(define (contar_piezas matriz jugador)
  (if (equal? jugador "B")
      (length (get_lista_pos_piezas_blancas matriz))
      (length (get_lista_pos_piezas_negras matriz))))
      

;(get_lista_pos_piezas_blancas matrizInicial)
;(get_lista_pos_piezas_negras matrizInicial)

;Obtener la matriz ajustando el valor pasado por parámetro en la posición indicada por parámetro (la posición se encuentra en una lista que proporciona la fila, la columna y el cuadrante). 
(define (ajustar_matriz matriz pos_elem_val_0 valor)
  (ajustar_matriz_a_fila matriz (list-ref pos_elem_val_0 0) (list-ref pos_elem_val_0 1) valor))

;Obtiene la matriz ajustada de manera recursiva teniendo en cuenta que si la fila es la que tiene el elemento que se desea cambiar, en esa fila uno de sus elementos será cambiado.
(define (ajustar_matriz_a_fila matriz fila columna valor)
  ;El resto de filas no se modificarán.
  (cond
    [(empty? matriz) '()]
    [(equal? fila 0) (append (list (ajustar_matriz_a_columna (car matriz) columna valor)) (cdr matriz))]
    [else (append (list (car matriz)) (ajustar_matriz_a_fila (cdr matriz) (- fila 1) columna valor))]))

;Obtiene la lista que representa una fila de la matriz con el valor cambiado en uno de sus elementos
(define (ajustar_matriz_a_columna lista columna valor) 
  (cond
    [(empty? lista) '()]
    [(equal? columna 0) (append (list valor) (cdr lista))] ; Sino funciona cdr lista, probar  (ajustar_matriz_a_columna (cdr lista) (- columna 1) valor)
    [else (append (list (car lista)) (ajustar_matriz_a_columna (cdr lista) (- columna 1) valor))]))

; Comprueba si se puede obtener fichas del jugador contrario en la fila
(define (comprobar_fila matriz color pos)
  (cond
    ;Situación en la que se tiene en cuenta la primera o la segunda posición de la fila
    [(or (equal? (list-ref pos 1) 0) (equal? (list-ref pos 1) 1))
     (comp_fila_asc matriz color pos (+ (list-ref pos 1) 1) #f)] ; Estudia las posiciones posteriores
    ;Situación en la que se tiene en cuenta la última o la penúltima posición de la fila
    [(or (equal? (list-ref pos 1) 6) (equal? (list-ref pos 1) 7))
     (comp_fila_desc matriz color pos (- (list-ref pos 1) 1) #f)] ; Estudia las posiciones previas
    [else
     (or (comp_fila_desc matriz color pos (- (list-ref pos 1) 1) #f) (comp_fila_asc matriz color pos (+ (list-ref pos 1) 1) #f))]; Estudia las posiciones previas y posteriores
  ))

; Comprueba si se puede obtener fichas del jugador contrario en la fila a medida que aumenta el valor de la fila
(define (comp_fila_asc matriz color pos num_col ver_opuesto)
  (cond
    [(equal? num_col 8) #f] ;No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz (car pos)) num_col) "-") #f] ;La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz (car pos)) num_col) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_fila_asc matriz color pos (+ num_col 1) #t)]
  ))

; Comprueba si se puede obtener fichas del jugador contrario en la fila a medida que disminuye el valor de la fila
(define (comp_fila_desc matriz color pos num_col ver_opuesto)
  (cond
    [(equal? num_col -1) #f] ;No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz (car pos)) num_col) "-") #f] ;La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz (car pos)) num_col) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_fila_desc matriz color pos (- num_col 1) #t)]
  ))

; Comprueba si se puede obtener fichas del jugador contrario en la columna
(define (comprobar_columna matriz color pos)
  (cond
    ;Situación en la que se tiene en cuenta la primera o la segunda posición de la columna
    [(or (equal? (car pos) 0) (equal? (car pos) 1))
     (comp_columna_asc matriz color pos (+ (car pos) 1) #f)] ; Estudia las posiciones posteriores
    ;Situación en la que se tiene en cuenta la última o la penúltima posición de la columna
    [(or (equal? (car pos) 6) (equal? (car pos) 7))
     (comp_columna_desc matriz color pos (- (car pos) 1) #f)] ; Estudia las posiciones previas
    [else
     (or (comp_columna_desc matriz color pos (- (car pos) 1) #f) (comp_columna_asc matriz color pos (+ (car pos) 1) #f))]; Estudia las posiciones previas y posteriores
  ))

; Comprueba si se puede obtener fichas del jugador contrario en la columna a medida que aumenta el valor de la columna
(define (comp_columna_asc matriz color pos num_fila ver_opuesto)
  (cond
    [(equal? num_fila 8) #f] ;No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz num_fila) (list-ref pos 1)) "-") #f] ;La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz num_fila) (list-ref pos 1)) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_columna_asc matriz color pos (+ num_fila 1) #t)]
  ))

; Comprueba si se puede obtener fichas del jugador contrario en la columna a medida que disminuye el valor de la columna
(define (comp_columna_desc matriz color pos num_fila ver_opuesto)
  (cond
    [(equal? num_fila -1) #f] ;No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz num_fila) (list-ref pos 1)) "-") #f] ;La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz num_fila) (list-ref pos 1)) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_columna_desc matriz color pos (- num_fila 1) #t)]
  ))

; Comprueba si se puede obtener fichas del jugador contrario en las diagonales
(define (comprobar_diagonales matriz color pos)
  (or (comprobar_diagonal_principal matriz color pos)(comprobar_diagonal_secundaria matriz color pos))
  )

; Comprueba si se puede obtener fichas del jugador contrario en la diagonal principal
(define (comprobar_diagonal_principal matriz color pos)
  (cond
    [(and (equal? (car pos) 0) (or (equal? (list-ref pos 1) 6) (equal? (list-ref pos 1) 7))) #f]; La esquina superior derecha no interesa estudiar puesto que no va aportar nada
    [(and (equal? (car pos) 1) (equal? (list-ref pos 1) 7)) #f]
    [(and (equal? (car pos) 7) (or (equal? (list-ref pos 1) 0) (equal? (list-ref pos 1) 1))) #f]; La esquina inferior izquierda no interesa estudiar puesto que no va aportar nada
    [(and (equal? (car pos) 6) (equal? (list-ref pos 1) 0)) #f]
    [(or (equal? (car pos) 0) (equal? (car pos) 1)) (comp_diag_princ_desc matriz color (+ (car pos) 1) (+ (list-ref pos 1) 1) #f)]
    [(or (equal? (car pos) 6) (equal? (car pos) 7)) (comp_diag_princ_asc matriz color (- (car pos) 1) (- (list-ref pos 1) 1) #f)]
    [else (or (comp_diag_princ_desc matriz color (- (car pos) 1) (- (list-ref pos 1) 1) #f) (comp_diag_princ_asc matriz color (+ (car pos) 1) (+ (list-ref pos 1) 1) #f))]
    )
  )

; Comprueba si se puede obtener fichas del jugador contrario en la diagonal principal de manera ascendente
(define (comp_diag_princ_asc matriz color num_fila num_col ver_opuesto)
  (cond
    [(or (equal? num_fila 8) (equal? num_col 8)) #f] ;No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz num_fila) num_col) "-") #f];La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz num_fila) num_col) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_diag_princ_asc matriz color (+ num_fila 1) (+ num_col 1) #t)]
    ))

; Comprueba si se puede obtener fichas del jugador contrario en la diagonal principal de manera descendente
(define (comp_diag_princ_desc matriz color num_fila num_col ver_opuesto)
  (cond
    [(or (equal? num_fila -1) (equal? num_col -1)) #f];No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz num_fila) num_col) "-") #f];La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz num_fila) num_col) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_diag_princ_desc matriz color (- num_fila 1) (- num_col 1) #t)]
    ))

; Comprueba si se puede obtener fichas del jugador contrario en la diagonal secundaria
(define (comprobar_diagonal_secundaria matriz color pos)
  (cond
    [(and (equal? (car pos) 0) (or (equal? (list-ref pos 1) 0) (equal? (list-ref pos 1) 1))) #f]; La esquina superior izquierda no interesa estudiar puesto que no va aportar nada
    [(and (equal? (car pos) 1) (equal? (list-ref pos 1) 0)) #f]
    [(and (equal? (car pos) 7) (or (equal? (list-ref pos 1) 6) (equal? (list-ref pos 1) 7))) #f]; La esquina inferior derecha no interesa estudiar puesto que no va aportar nada
    [(and (equal? (car pos) 6) (equal? (list-ref pos 1) 7)) #f]
    [(or (equal? (car pos) 0) (equal? (car pos) 1)) (comp_diag_sec_desc matriz color (+ (car pos) 1) (- (list-ref pos 1) 1) #f)]
    [(or (equal? (car pos) 6) (equal? (car pos) 7)) (comp_diag_sec_asc matriz color (- (car pos) 1) (+ (list-ref pos 1) 1) #f)]
    [else (or (comp_diag_sec_desc matriz color (+ (car pos) 1) (- (list-ref pos 1) 1) #f) (comp_diag_sec_asc matriz color (- (car pos) 1) (+ (list-ref pos 1) 1) #f))]
    )
  )

; Comprueba si se puede obtener fichas del jugador contrario en la diagonal secundaria de manera ascendente
(define (comp_diag_sec_asc matriz color num_fila num_col ver_opuesto)
  (cond
    [(or (equal? num_fila -1) (equal? num_col 8)) #f] ;No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz num_fila) num_col) "-") #f];La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz num_fila) num_col) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_diag_sec_asc matriz color (- num_fila 1) (+ num_col 1) #t)]
    ))

; Comprueba si se puede obtener fichas del jugador contrario en la diagonal secundaria de manera descendente
(define (comp_diag_sec_desc matriz color num_fila num_col ver_opuesto)
  (cond
    [(or (equal? num_fila 8) (equal? num_col -1)) #f];No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz num_fila) num_col) "-") #f];La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz num_fila) num_col) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_diag_sec_desc matriz color (+ num_fila 1) (- num_col 1) #t)]
    ))

; Devuelve una lista en la que una de sus elementos va a sufrir un cambio
(define (ajustar_columna lista columna color)
  (cond
    [(empty? lista) '()] ; No hay más elementos
    [(equal? columna 0)(append (list color) (cdr lista))] ; Se ha alcanzado el elemento a modificar, se modifica y se une con el resto de elementos de la lista
    [else (append (list (car lista)) (ajustar_columna (cdr lista) (- columna 1) color))])) ; Se va reconstruyendo la lista mientras que se va buscando el elemento buscado

; Devuelve una matriz en la que una de sus filas va a sufrir un cambio
(define (ajustar_fila matriz fila columna color)
  (cond
    [(empty? matriz) '()] ; No hay más filas
    [(equal? fila 0) (append (list (ajustar_columna (car matriz) columna color)) (cdr matriz))] ; Se ha alcanzado la fila a modificar, se modifica y se une con el resto de filas de la matriz
    [else (append (list(car matriz)) (ajustar_fila (cdr matriz) (- fila 1) columna color))])) ; Se va reconstruyendo la matriz mientras que se va buscando la fila buscado

; Proporciona la matriz pintada en la fila de manera que va aumentando el valor de la fila
(define (pintar_fila_asc matriz color pos num_col verificar)
  (cond
    ;Devuelve la matriz pasada por parámetro si no es la primera ficha pasada del mismo color al pasado por parámetro o se sale de rango los valores de la columna
    [(or (equal? num_col 8) (and (equal? (list-ref (list-ref matriz (car pos)) num_col) color) (equal? verificar #t))) matriz]
    ;Pasa al siguiente elemento a pintar si es el primer elemento del color pasado por parámetro
    [(and (equal? (list-ref (list-ref matriz (car pos)) num_col) color) (equal? verificar #f))
     (pintar_fila_asc matriz color pos (+ num_col 1) #t)]
    ;Cambia el color de una casilla y pasa al siguiente elemento a estudiar
    [else (pintar_fila_asc (ajustar_fila matriz (car pos) num_col color) color pos (+ num_col 1) verificar)]))

; Proporciona la matriz pintada en la fila de manera que va disminuyendo el valor de la fila
(define (pintar_fila_desc matriz color pos num_col verificar)
  (cond
    ;Devuelve la matriz pasada por parámetro si no es la primera ficha pasada del mismo color al pasado por parámetro o se sale de rango los valores de la columna
    [(or (equal? num_col -1) (and (equal? (list-ref (list-ref matriz (car pos)) num_col) color) (equal? verificar #t))) matriz]
    ;Pasa al siguiente elemento a pintar si es el primer elemento del color pasado por parámetro
    [(and (equal? (list-ref (list-ref matriz (car pos)) num_col) color) (equal? verificar #f))
     (pintar_fila_desc matriz color pos (- num_col 1) #t)]
    ;Cambia el color de una casilla y pasa al siguiente elemento a estudiar
    [else (pintar_fila_desc (ajustar_fila matriz (car pos) num_col color) color pos (- num_col 1) verificar)]))
  
; Proporciona la matriz pintada en la columna de manera que va aumentando el valor de la columna
(define (pintar_col_asc matriz color pos num_fila verificar)
  (cond
    ;Devuelve la matriz pasada por parámetro si no es la primera ficha pasada del mismo color al pasado por parámetro o se sale de rango los valores de la fila
    [(or (equal? num_fila 8) (and (equal? (list-ref (list-ref matriz num_fila) (list-ref pos 1)) color) (equal? verificar #t))) matriz]
    ;Pasa al siguiente elemento a pintar si es el primer elemento del color pasado por parámetro
    [(and (equal? (list-ref (list-ref matriz num_fila) (list-ref pos 1)) color) (equal? verificar #f))
     (pintar_col_asc matriz color pos (+ num_fila 1) #t)]
    ;Cambia el color de una casilla y pasa al siguiente elemento a estudiar
    [else (pintar_col_asc (ajustar_fila matriz num_fila (list-ref pos 1) color) color pos (+ num_fila 1) verificar)]))
  
; Proporciona la matriz pintada en la columna de manera que va disminuyendo el valor de la columna
(define (pintar_col_desc matriz color pos num_fila verificar)
  (cond
    ;Devuelve la matriz pasada por parámetro si no es la primera ficha pasada del mismo color al pasado por parámetro o se sale de rango los valores de la fila
    [(or (equal? num_fila -1) (and (equal? (list-ref (list-ref matriz num_fila) (list-ref pos 1)) color) (equal? verificar #t))) matriz]
    ;Pasa al siguiente elemento a pintar si es el primer elemento del color pasado por parámetro
    [(and (equal? (list-ref (list-ref matriz num_fila) (list-ref pos 1)) color) (equal? verificar #f))
     (pintar_col_desc matriz color pos (- num_fila 1) #t)]
    ;Cambia el color de una casilla y pasa al siguiente elemento a estudiar
    [else (pintar_col_desc (ajustar_fila matriz num_fila (list-ref pos 1) color) color pos (- num_fila 1) verificar)]))

; Proporciona la matriz pintada en la diagonal principal de manera ascendente
(define (pintar_diag_princ_asc matriz color pos num_fila num_col verificar)
  (cond
    ;Devuelve la matriz pasada por parámetro si no es la primera ficha pasada del mismo color al pasado por parámetro o se sale de rango los valores de la fila o la columna
    [(or (equal? num_col 8) (equal? num_fila 8) (and (equal? (list-ref (list-ref matriz num_fila) num_col) color) (equal? verificar #t))) matriz]
    ;Pasa al siguiente elemento a pintar si es el primer elemento del color pasado por parámetro
    [(and (equal? (list-ref (list-ref matriz num_fila) num_col) color) (equal? verificar #f))
     (pintar_diag_princ_asc matriz color pos (+ num_fila 1) (+ num_col 1) #t)]
    ;Cambia el color de una casilla y pasa al siguiente elemento a estudiar
    [else (pintar_diag_princ_asc (ajustar_fila matriz num_fila num_col color) color pos (+ num_fila 1) (+ num_col 1) verificar)]))

; Proporciona la matriz pintada en la diagonal principal de manera descendente
(define (pintar_diag_princ_desc matriz color pos num_fila num_col verificar)
  (cond
    ;Devuelve la matriz pasada por parámetro si no es la primera ficha pasada del mismo color al pasado por parámetro o se sale de rango los valores de la fila o la columna
    [(or (equal? num_col -1) (equal? num_fila -1) (and (equal? (list-ref (list-ref matriz num_fila) num_col) color) (equal? verificar #t))) matriz]
    ;Pasa al siguiente elemento a pintar si es el primer elemento del color pasado por parámetro
    [(and (equal? (list-ref (list-ref matriz num_fila) num_col) color) (equal? verificar #f))
     (pintar_diag_princ_desc matriz color pos (- num_fila 1) (- num_col 1) #t)]
    ;Cambia el color de una casilla y pasa al siguiente elemento a estudiar
    [else (pintar_diag_princ_desc (ajustar_fila matriz num_fila num_col color) color pos (- num_fila 1) (- num_col 1) verificar)]))

; Proporciona la matriz pintada en la diagonal secundaria de manera ascendente
(define (pintar_diag_sec_asc matriz color pos num_fila num_col verificar)
  (cond
    ;Devuelve la matriz pasada por parámetro si no es la primera ficha pasada del mismo color al pasado por parámetro o se sale de rango los valores de la fila o la columna
    [(or (equal? num_col 8) (equal? num_fila -1) (and (equal? (list-ref (list-ref matriz num_fila) num_col) color) (equal? verificar #t))) matriz]
    ;Pasa al siguiente elemento a pintar si es el primer elemento del color pasado por parámetro
    [(and (equal? (list-ref (list-ref matriz num_fila) num_col) color) (equal? verificar #f))
     (pintar_diag_sec_asc matriz color pos (- num_fila 1) (+ num_col 1) #t)]
    ;Cambia el color de una casilla y pasa al siguiente elemento a estudiar
    [else (pintar_diag_sec_asc (ajustar_fila matriz num_fila num_col color) color pos (- num_fila 1) (+ num_col 1) verificar)]))

; Proporciona la matriz pintada en la diagonal secundaria de manera descendente
(define (pintar_diag_sec_desc matriz color pos num_fila num_col verificar)
  (cond
    ;Devuelve la matriz pasada por parámetro si no es la primera ficha pasada del mismo color al pasado por parámetro o se sale de rango los valores de la fila o la columna
    [(or (equal? num_col -1) (equal? num_fila 8) (and (equal? (list-ref (list-ref matriz num_fila) num_col) color) (equal? verificar #t))) matriz]
    ;Pasa al siguiente elemento a pintar si es el primer elemento del color pasado por parámetro
    [(and (equal? (list-ref (list-ref matriz num_fila) num_col) color) (equal? verificar #f))
     (pintar_diag_sec_desc matriz color pos (+ num_fila 1) (- num_col 1) #t)]
    ;Cambia el color de una casilla y pasa al siguiente elemento a estudiar
    [else (pintar_diag_sec_desc (ajustar_fila matriz num_fila num_col color) color pos (+ num_fila 1) (- num_col 1) verificar)]))

;Proporciona una matriz con los cambios sufridos en una fila del tablero a partir de la colocación de una pieza en una posición
(define (cambiar_color_fila matriz pos color)
  (cond
    ;Considera que puede avanzar hacia la izquierda o hacia la derecha en la propia fila
    [(and (comp_fila_asc matriz color pos (+ (list-ref pos 1) 1) #f)(comp_fila_desc matriz color pos (- (list-ref pos 1) 1) #f))
     (pintar_fila_desc (pintar_fila_asc matriz color pos (list-ref pos 1) #f) color pos (list-ref pos 1) #f)]
    ;Considera que puede avanzar hacia la derecha en la propia fila
    [(comp_fila_asc matriz color pos (+ (list-ref pos 1) 1) #f)
     (pintar_fila_asc matriz color pos (list-ref pos 1) #f)]
    ;Considera que puede avanzar hacia la izquierda en la propia fila
    [(comp_fila_desc matriz color pos (- (list-ref pos 1) 1) #f)
     (pintar_fila_desc matriz color pos (list-ref pos 1) #f)]
    ;Considera que no puede avanzar por ningún sentido de la fila
    [else matriz]))

;Proporciona una matriz con los cambios sufridos en una columna del tablero a partir de la colocación de una pieza en una posición
(define (cambiar_color_col matriz pos color)
  (cond
    ;Considera que puede avanzar hacia arriba o hacia abajo en la propia columna
    [(and (comp_columna_asc matriz color pos (+ (car pos) 1) #f)(comp_columna_desc matriz color pos (- (car pos) 1) #f))
     (pintar_col_desc (pintar_col_asc matriz color pos (car pos) #f) color pos (car pos) #f)]
    ;Considera que puede avanzar hacia abajo en la propia columna
    [(comp_columna_asc matriz color pos (+ (car pos) 1) #f)
     (pintar_col_asc matriz color pos (car pos) #f)]
    ;Considera que puede avanzar hacia arriba en la propia columna
    [(comp_columna_desc matriz color pos (- (car pos) 1) #f)
     (pintar_col_desc matriz color pos (car pos) #f)]
    ;Considera que no puede avanzar por ningún sentido de la columna
    [else matriz]))

;Proporciona una matriz con los cambios sufridos en una diagonal principal del tablero a partir de la colocación de una pieza en una posición
(define (cambiar_color_diag_princ matriz pos color)
  (cond
    ;Considera que puede avanzar hacia arriba o hacia abajo en la propia diagonal principal
    [(and (comp_diag_princ_asc matriz color (+ (car pos) 1) (+ (list-ref pos 1) 1) #f)(comp_diag_princ_desc matriz color (+ (car pos) 1) (- (list-ref pos 1) 1) #f))
     (pintar_diag_princ_asc (pintar_diag_princ_desc matriz color pos (car pos) (list-ref pos 1) #f) color pos (car pos) (list-ref pos 1) #f)]
    ;Considera que puede avanzar hacia arriba en la propia diagonal principal
    [(comp_diag_princ_asc matriz color (+ (car pos) 1) (+ (list-ref pos 1) 1) #f)
     (pintar_diag_princ_asc matriz color pos (car pos) (list-ref pos 1) #f)]
    ;Considera que puede avanzar hacia abajo en la propia diagonal principal
    [(comp_diag_princ_desc matriz color (- (car pos) 1) (- (list-ref pos 1) 1) #f)
     (pintar_diag_princ_desc matriz color pos (car pos) (list-ref pos 1) #f)]
    ;Considera que no puede avanzar por ningún sentido de la diagonal principal
    [else matriz]))

;Proporciona una matriz con los cambios sufridos en una diagonal secundaria del tablero a partir de la colocación de una pieza en una posición
(define (cambiar_color_diag_sec matriz pos color)
  (cond
    ;Considera que puede avanzar hacia arriba o hacia abajo en la propia diagonal secundaria
    [(and (comp_diag_sec_asc matriz color (- (car pos) 1) (+ (list-ref pos 1) 1) #f)(comp_diag_sec_desc matriz color (+ (car pos) 1) (- (list-ref pos 1) 1) #f))
     (pintar_diag_sec_asc (pintar_diag_sec_desc matriz color pos (car pos) (list-ref pos 1) #f) color pos (car pos) (list-ref pos 1) #f)]
    ;Considera que puede avanzar hacia arriba en la propia diagonal secundaria
    [(comp_diag_sec_asc matriz color (- (car pos) 1) (+ (list-ref pos 1) 1) #f)
     (pintar_diag_sec_asc matriz color pos (car pos) (list-ref pos 1) #f)]
    ;Considera que puede avanzar hacia abajo en la propia diagonal secundaria
    [(comp_diag_sec_desc matriz color (+ (car pos) 1) (- (list-ref pos 1) 1) #f)
     (pintar_diag_sec_desc matriz color pos (car pos) (list-ref pos 1) #f)]
    ;Considera que no puede avanzar por ningún sentido de la diagonal secundaria
     [else matriz]))

;Proporciona una matriz con los cambios sufridos en el tablero a partir de la colocación de una pieza en una posición
(define (cambiar_color matriz pos color)
  (cambiar_color_diag_sec (cambiar_color_diag_princ (cambiar_color_col (cambiar_color_fila (ajustar_fila matriz (car pos) (list-ref pos 1) color) pos color) pos color) pos color) pos color))


; Realiza cualquiera de los algoritmos para que la máquina proporcione la mejor jugada y su puntuación
(define (jugar_maquina matriz algoritmo puntos_jugadores_iniciales jugador prof alfa beta)
  (cond
    [(equal? prof 0) (list (get_puntos matriz jugador puntos_jugadores_iniciales) null)] ; Devuelve el resultado de la partida
    [else ; Se estudia los niveles de profundidas intermedios
     (define jugadas_jugador (get_lista_pos_validas matriz jugador))
     (define jugadas_oponente (get_lista_pos_validas matriz (select_oponente jugador)))
     (cond
       [(empty? jugadas_jugador)
        (cond
          [(empty? jugadas_oponente) (list (get_puntos matriz jugador puntos_jugadores_iniciales) null)] ; No hay más posibles movimientos por parte de ninguno de los dos jugadores
          [else ; Se sigue estudiando el siguiente nivel de profundidad de acuerdo con el otro jugador
           (if (equal? algoritmo "minimax") 
               ; Nos interesa guardar los puntos como negativos ya que son a favor del contrincante
               ; La forma de obtención de resultado de acuerdo a poda alfa beta y minimax sería la misma
               (list (- (list-ref (jugar_maquina matriz algoritmo puntos_jugadores_iniciales (select_oponente jugador) (- prof 1) null null) 0)) null)
               (list (- (list-ref (jugar_maquina matriz algoritmo puntos_jugadores_iniciales (select_oponente jugador) (- prof 1) (- beta) (- alfa)) 0)) null))])]
       [else ; Se pueden introducir más movimientos
        (cond
          [(equal? algoritmo "minimax")
           ; Se obtiene la lista de puntos obtenido por cada una de las jugadas posibles del jugador
           (define lista_puntos
             (for/list [(i (length jugadas_jugador))]
               (- (list-ref (jugar_maquina (cambiar_color matriz (list-ref jugadas_jugador i)  jugador) algoritmo puntos_jugadores_iniciales (select_oponente jugador) (- prof 1) null null) 0)))) ;El list-ref coge la max puntuacion
           ; Se busca la mayor puntuación entre todos elementos
           (define max_puntuacion (get_max_minimax lista_puntos))
           ; Se obtiene la posición con la mayor puntuación previamente obtenida
           (list max_puntuacion (get_pos_puntos lista_puntos jugadas_jugador max_puntuacion))]
          [else
           (cond
             ; Se comprueba si alfa es mayor o igual que beta y en dicho caso devuelve una lista con el valor de alfa y la primera posición de la lista de las jugadas posibles del jugador
             ; puesto que aún no se ha guardado ninguna jugada
             [(>= alfa beta) (list alfa (car jugadas_jugador))]
             ; Se busca entre la lista de jugadas la jugada que más puntos le puede dar al jugador
             [else (rec_poda jugadas_jugador matriz algoritmo puntos_jugadores_iniciales jugador prof alfa beta  (car jugadas_jugador))])])])]))


; Devuelve la puntuación más alta junto con la mejor jugada que se puede realizar dentro de las jugadas que se pueden realizar en un nivel de profundidad
(define (rec_poda jugadas_jugador matriz algoritmo puntos_jugadores_iniciales jugador prof alfa beta best_pos)
  (cond
    ; Se sale de la función si se produce una poda o si la lista de jugadas se acaba
    [(or (>= alfa beta) (empty? jugadas_jugador)) (list alfa best_pos)]
    [else
     ; Obtiene el valor de la jugada actual de estudio
      (define val (- (list-ref(jugar_maquina (cambiar_color matriz (car jugadas_jugador) jugador) algoritmo puntos_jugadores_iniciales jugador (- prof) (- beta) (- alfa))0)))
      ; Comprueba si el valor obtenido es mayor al que había de alfa
      (if (> val alfa)
          ; Se sustituye alfa por el valor obtenido de val, se cambia la mejor jugada por la que acabamos de estudiar y se pasa al siguiente elemento de la lista de jugadas del jugador
          (rec_poda (cdr jugadas_jugador) matriz algoritmo puntos_jugadores_iniciales jugador prof val beta (car jugadas_jugador))
          ; Se pasa al siguiente elemento de la lista de jugadas del jugador sin realizar ningún cambio
          (rec_poda (cdr jugadas_jugador) matriz algoritmo puntos_jugadores_iniciales jugador prof alfa beta best_pos))]))

; Obtener el color de la ficha del oponente
(define (select_oponente jugador)
  (if (equal? jugador "B") "N" "B"))

; Obtiene la máxima puntuación que se puede obtener de toda la lista con búsqueda minimax
(define (get_max_minimax lista_puntos)
  (if(empty? lista_puntos) -9999999999999 
  (max (car lista_puntos) (get_max_minimax (cdr lista_puntos)))))

; Obtiene la máxima puntuación que se puede obtener de toda la lista con búsqueda minimax
(define (get_max_poda lista_puntos alfa)
  (if (empty? lista_puntos) -9999999999999 
  (max alfa (car lista_puntos) (get_max_poda (cdr lista_puntos) alfa))))


; Obtiene los puntos totales entre los de la posición inicial y los de la profundidad máxima a la que ha llegado
(define (get_puntos matriz color puntos_jugadores_iniciales)
  (if (equal? color "N")
      (+ (- (contar_piezas matriz color) (car puntos_jugadores_iniciales)) (- (list-ref puntos_jugadores_iniciales 1) (contar_piezas matriz (select_oponente color)))) ; Si es el jugador
      (+ (- (contar_piezas matriz color) (list-ref puntos_jugadores_iniciales 1)) (- (car puntos_jugadores_iniciales) (contar_piezas matriz (select_oponente color)))))); Si es la máquina

; Obtiene las posiciones de los puntos obtenidos
(define (get_pos_puntos lista_puntos lista_pos_validas puntos)
  (cond
    ;[(equal? (length lista_puntos) 1) lista_pos_validas]
    [(equal? puntos (car lista_puntos)) (car lista_pos_validas)]
    [else (get_pos_puntos (cdr lista_puntos) (cdr lista_pos_validas) puntos)]))

(define matrizInicial1
  (list
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "N" "-" "-" "-" "-")
   (list "-" "-" "-" "N" "N" "-" "-" "-")
   (list "-" "-" "-" "N" "B" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   ))

(define listaPrueba1 (list (contar_piezas matrizInicial1 "N") (contar_piezas matrizInicial1 "B")))

;(jugar_maquina matrizInicial1 "minimax" listaPrueba1 jugadorMin 5 null null)

;[PARTE GRÁFICA]

(require (lib "graphics.ss""graphics"))
(require (lib "image.ss" "teachpack/htdp"))
(open-graphics)
(define Reversi (open-viewport "Reversi" 800 800))

; Define el avance de la partida de acuerdo a los movimientos de cada jugador
(define (juego matriz algoritmo num_jugadores turno prof)
  (tableroJugador matriz "N" "B" turno) ;Actualiza el tablero
  (cond
    [(and (equal? (length (get_lista_pos_validas matriz "N")) 0) (equal? (length (get_lista_pos_validas matriz "B")) 0)) ; Comprueba si no quedan más movimientos por cada jugador
      (define puntos_jugador (contar_piezas matriz "N"))
      (define puntos_maquina (contar_piezas matriz "B"))
      (cond
        ; Pantallas de resultados
        [(and (> puntos_jugador puntos_maquina) (equal? num_jugadores 1)) (ver_resultado "Victoria") "Victoria"]
        [(and (> puntos_jugador puntos_maquina) (equal? num_jugadores 2)) (ver_resultado "Victoria Jugador 1")"Victoria Jugador 1"]
        [(equal? puntos_jugador puntos_maquina) (ver_resultado "Empate")"Empate"]
        [(and (< puntos_jugador puntos_maquina) (equal? num_jugadores 2)) (ver_resultado "Victoria Jugador 2") "Victoria Jugador 2"]
        [else (ver_resultado "Derrota") "Derrota"])]
    [else
      (cond
        [(equal? (modulo turno 2) 0) ; Turno primer jugador
         (cond
           [(empty? (get_lista_pos_validas matriz "N")) ; No puede poner ninguna ficha el jugador uno
            ;(put_aviso) ; Aviso de que no puede mover
            (juego matriz algoritmo num_jugadores (+ turno 1) prof); Juega ahora la máquina o el segundo jugador
            ]
           [else
            (define casilla (botones  (get-mouse-click Reversi) Reversi "N" "B" matriz )) ; Obtiene la posición pulsada
            (insertar_posicion_posible(get_lista_pos_validas matriz "N") "N");El jugador ya ha insertado, se resaltan las posiciones del oponente
            (juego (cambiar_color matriz casilla "N") algoritmo num_jugadores (+ turno 1) prof)  ; Pasa el turno al siguiente jugador
            ])] 

        [else ; Turno segundo jugador o máquina
         (cond
           [(empty? (get_lista_pos_validas matriz "B"))
            (juego matriz algoritmo num_jugadores (+ turno 1) prof); Juega ahora el primer jugador
            ]
           [else
            (cond
              [(equal? num_jugadores 2) ; Comprueba si el modo de juego elegido es el de dos jugadores
               (cond
                 [(empty? (get_lista_pos_validas matriz "B")); Comprueba si puede poner alguna ficha
                  ;(put_aviso); Aviso de que no puede mover
                  (juego matriz algoritmo num_jugadores (+ turno 1) prof); Juega ahora la máquina o el segundo jugador
                  ]
                 [else
                  (define casilla (botones  (get-mouse-click Reversi) Reversi "B" "N" matriz )) ; Obtiene la posición pulsada
                  (insertar_posicion_posible(get_lista_pos_validas matriz "B") "B");El jugador ya ha insertado, se resaltan las posiciones del oponente
                  (juego (cambiar_color matriz casilla "B") algoritmo num_jugadores (+ turno 1) prof)])
               ]
              [else
               (define casilla (list-ref (jugar_maquina matriz algoritmo (list (contar_piezas matriz "N") (contar_piezas matriz "B")) "B" prof alfa beta) 1)) ;Casilla elegida por la máquina
               (cond
                 [(equal? casilla null); Comprueba si lo ha conseguido encontrar ninguna ficha para poner
                  (insertar_ficha(get_lista_pos_piezas_blancas (cambiar_color matriz casilla "B"))"B")
                   (insertar_posicion_posible(get_lista_pos_validas matriz "B") "B")
                   (juego matriz algoritmo num_jugadores (+ turno 1) prof)]
                 [else
                    (juego (cambiar_color matriz casilla "B") algoritmo num_jugadores (+ turno 1) prof)])] ;Se pasa el turno, mirar si la matriz que recibe jugar va cambiando bien al ejecutar los prints de tablero
            )])])]
    ))

(define (tableroJugador matriz jugador oponente turno) ; Coloca las piezas de un jugador en el tablero de la interfaz gráfica

  ((draw-viewport Reversi) (make-rgb 0.6 0.8 0.3)) ; 0.6 0.8 0.3 == Verde
  (for* ([i 9][j 9]) ;Pongo 9 porque 0*75 es 0 y no me imprime nada
    ((draw-rectangle Reversi)(make-posn 0 0)(* 100 i)(* 100 j)"blue") ;El tamaño de cada casilla y colocacion en la ventana
  )
   (insertar_ficha (get_lista_pos_piezas_blancas matriz) oponente) ;Inserta en el tablero las fichas de la lista de piezas blancas
   (insertar_ficha (get_lista_pos_piezas_negras matriz) jugador)  ;Inserta en el tablero las fichas de la lista de piezas negras
  (if (equal? (modulo turno 2) 0)
      (insertar_posicion_posible(get_lista_pos_validas matriz jugador) jugador)  ;Inserta los lugares de posibles posiciones de insercion de piezas negras
      (insertar_posicion_posible(get_lista_pos_validas matriz oponente) oponente))  ;Inserta los lugares de posibles posiciones de insercion de piezas blancas
  )

(define (tableroMaquina matriz jugador oponente) ; Coloca las piezas de la máquina en el tablero de la interfaz gráfica

  ((draw-viewport Reversi) (make-rgb 0.6 0.8 0.3)) ; 0.6 0.8 0.3 == Verde
  (for* ([i 9][j 9]) ;Pongo 9 porque 0*75 es 0 y no me imprime nada
    ((draw-rectangle Reversi)(make-posn 0 0)(* 100 i)(* 100 j)"blue") ;El tamaño de cada casilla y colocacion en la ventana
  )
   ;Piezas que tienen cada jugador
   (insertar_ficha (get_lista_pos_piezas_blancas matriz) oponente) ;Inserta en el tablero las fichas de la lista de piezas blancas
   (insertar_ficha (get_lista_pos_piezas_negras matriz) jugador)  ;Inserta en el tablero las fichas de la lista de piezas negras
   (insertar_posicion_posible(get_lista_pos_validas matriz jugador) jugador) ;Inserta las opciones posibles del jugador en el turno
  )

(define (insertar_ficha lista jugador) ;Permite meter una ficha a partir de una lista
  (cond [(empty? lista) 0]
        [else (posiciones_tablero (list-ref(car lista )0) (list-ref(car lista )1) jugador) (insertar_ficha (cdr lista) jugador) ]
  )
 )

(define (botones click Reversi jugador oponente matriz)
  (cond
    ;Primera columna
    ((and (<= 0 (posn-x (mouse-click-posn click)) 100)
          (<= 0 (posn-y (mouse-click-posn click)) 100))
     '(0 0))
    ((and (<= 0 (posn-x (mouse-click-posn click)) 100)
          (<= 100 (posn-y (mouse-click-posn click)) 200))
     '(0 1))
    ((and (<= 0 (posn-x (mouse-click-posn click)) 100)
          (<= 200 (posn-y (mouse-click-posn click)) 300))
     '(0 2))
    ((and (<= 0 (posn-x (mouse-click-posn click)) 100)
          (<= 300 (posn-y (mouse-click-posn click)) 400))
     '(0 3))
    ((and (<= 0 (posn-x (mouse-click-posn click)) 100)
          (<= 400 (posn-y (mouse-click-posn click)) 500))
     '(0 4))
    ((and (<= 0 (posn-x (mouse-click-posn click)) 100)
          (<= 500 (posn-y (mouse-click-posn click)) 600))
     '(0 5))
    ((and (<= 0 (posn-x (mouse-click-posn click)) 100)
          (<= 600 (posn-y (mouse-click-posn click)) 700))
     '(0 6))
    ((and (<= 0 (posn-x (mouse-click-posn click)) 100)
          (<= 700 (posn-y (mouse-click-posn click)) 800))
     '(0 7))
    ;Segunda columna
    ((and (<= 100 (posn-x (mouse-click-posn click)) 200)
          (<= 0 (posn-y (mouse-click-posn click)) 100))
     '(1 0))
    ((and (<= 100 (posn-x (mouse-click-posn click)) 200)
          (<= 100 (posn-y (mouse-click-posn click)) 200))
     '(1 1))
    ((and (<= 100 (posn-x (mouse-click-posn click)) 200)
          (<= 200 (posn-y (mouse-click-posn click)) 300))
     '(1 2))
    ((and (<= 100 (posn-x (mouse-click-posn click)) 200)
          (<= 300 (posn-y (mouse-click-posn click)) 400))
     '(1 3))
    ((and (<= 100 (posn-x (mouse-click-posn click)) 200)
          (<= 400 (posn-y (mouse-click-posn click)) 500))
     '(1 4))
    ((and (<= 100 (posn-x (mouse-click-posn click)) 200)
          (<= 500 (posn-y (mouse-click-posn click)) 600))
     '(1 5))
    ((and (<= 100 (posn-x (mouse-click-posn click)) 200)
          (<= 600 (posn-y (mouse-click-posn click)) 700))
     '(1 6))
    ((and (<= 100 (posn-x (mouse-click-posn click)) 200)
          (<= 700 (posn-y (mouse-click-posn click)) 800))
     '(1 7))
    ;Tercera columna
    ((and (<= 200 (posn-x (mouse-click-posn click)) 300)
          (<= 0 (posn-y (mouse-click-posn click)) 100))
     '(2 0))
    ((and (<= 200 (posn-x (mouse-click-posn click)) 300)
          (<= 100 (posn-y (mouse-click-posn click)) 200))
     '(2 1))
    ((and (<= 200 (posn-x (mouse-click-posn click)) 300)
          (<= 200 (posn-y (mouse-click-posn click)) 300))
     '(2 2))
    ((and (<= 200 (posn-x (mouse-click-posn click)) 300)
          (<= 300 (posn-y (mouse-click-posn click)) 400))
     '(2 3))
    ((and (<= 200 (posn-x (mouse-click-posn click)) 300)
          (<= 400 (posn-y (mouse-click-posn click)) 500))
     '(2 4))
    ((and (<= 200 (posn-x (mouse-click-posn click)) 300)
          (<= 500 (posn-y (mouse-click-posn click)) 600))
     '(2 5))
    ((and (<= 200 (posn-x (mouse-click-posn click)) 300)
          (<= 600 (posn-y (mouse-click-posn click)) 700))
     '(2 6))
    ((and (<= 200 (posn-x (mouse-click-posn click)) 300)
          (<= 700 (posn-y (mouse-click-posn click)) 800))
     '(2 7))
    ;Cuarta columna
    ((and (<= 300 (posn-x (mouse-click-posn click)) 400)
          (<= 0 (posn-y (mouse-click-posn click)) 100))
     '(3 0))
    ((and (<= 300 (posn-x (mouse-click-posn click)) 400)
          (<= 100 (posn-y (mouse-click-posn click)) 200))
     '(3 1))
    ((and (<= 300 (posn-x (mouse-click-posn click)) 400)
          (<= 200 (posn-y (mouse-click-posn click)) 300))
     '(3 2))
    ((and (<= 300 (posn-x (mouse-click-posn click)) 400)
          (<= 300 (posn-y (mouse-click-posn click)) 400))
     '(3 3))
    ((and (<= 300 (posn-x (mouse-click-posn click)) 400)
          (<= 400 (posn-y (mouse-click-posn click)) 500))
     '(3 4))
    ((and (<= 300 (posn-x (mouse-click-posn click)) 400)
          (<= 500 (posn-y (mouse-click-posn click)) 600))
     '(3 5))
    ((and (<= 300 (posn-x (mouse-click-posn click)) 400)
          (<= 600 (posn-y (mouse-click-posn click)) 700))
     '(3 6))
    ((and (<= 300 (posn-x (mouse-click-posn click)) 400)
          (<= 700 (posn-y (mouse-click-posn click)) 800))
     '(3 7))
    ;Quinta columna
    ((and (<= 400 (posn-x (mouse-click-posn click)) 500)
          (<= 0 (posn-y (mouse-click-posn click)) 100))
     '(4 0))
    ((and (<= 400 (posn-x (mouse-click-posn click)) 500)
          (<= 100 (posn-y (mouse-click-posn click)) 200))
     '(4 1))
    ((and (<= 400 (posn-x (mouse-click-posn click)) 500)
          (<= 200 (posn-y (mouse-click-posn click)) 300))
     '(4 2))
    ((and (<= 400 (posn-x (mouse-click-posn click)) 500)
          (<= 300 (posn-y (mouse-click-posn click)) 400))
     '(4 3))
    ((and (<= 400 (posn-x (mouse-click-posn click)) 500)
          (<= 400 (posn-y (mouse-click-posn click)) 500))
     '(4 4))
    ((and (<= 400 (posn-x (mouse-click-posn click)) 500)
          (<= 500 (posn-y (mouse-click-posn click)) 600))
     '(4 5))
    ((and (<= 400 (posn-x (mouse-click-posn click)) 500)
          (<= 600 (posn-y (mouse-click-posn click)) 700))
     '(4 6))
    ((and (<= 400 (posn-x (mouse-click-posn click)) 500)
          (<= 700 (posn-y (mouse-click-posn click)) 800))
     '(4 7))
    ;Sexta columna
    ((and (<= 500 (posn-x (mouse-click-posn click)) 600)
          (<= 0 (posn-y (mouse-click-posn click)) 100))
     '(5 0))
    ((and (<= 500 (posn-x (mouse-click-posn click)) 600)
          (<= 100 (posn-y (mouse-click-posn click)) 200))
     '(5 1))
    ((and (<= 500 (posn-x (mouse-click-posn click)) 600)
          (<= 200 (posn-y (mouse-click-posn click)) 300))
     '(5 2))
    ((and (<= 500 (posn-x (mouse-click-posn click)) 600)
          (<= 300 (posn-y (mouse-click-posn click)) 400))
     '(5 3))
    ((and (<= 500 (posn-x (mouse-click-posn click)) 600)
          (<= 400 (posn-y (mouse-click-posn click)) 500))
     '(5 4))
    ((and (<= 500 (posn-x (mouse-click-posn click)) 600)
          (<= 500 (posn-y (mouse-click-posn click)) 600))
     '(5 5))
    ((and (<= 500 (posn-x (mouse-click-posn click)) 600)
          (<= 600 (posn-y (mouse-click-posn click)) 700))
     '(5 6))
    ((and (<= 500 (posn-x (mouse-click-posn click)) 600)
          (<= 700 (posn-y (mouse-click-posn click)) 800))
     '(5 7))
    ;Septima columna
    ((and (<= 600 (posn-x (mouse-click-posn click)) 700)
          (<= 0 (posn-y (mouse-click-posn click)) 100))
     '(6 0))
    ((and (<= 600 (posn-x (mouse-click-posn click)) 700)
          (<= 100 (posn-y (mouse-click-posn click)) 200))
     '(6 1))
    ((and (<= 600 (posn-x (mouse-click-posn click)) 700)
          (<= 200 (posn-y (mouse-click-posn click)) 300))
     '(6 2))
    ((and (<= 600 (posn-x (mouse-click-posn click)) 700)
          (<= 300 (posn-y (mouse-click-posn click)) 400))
     '(6 3))
    ((and (<= 600 (posn-x (mouse-click-posn click)) 700)
          (<= 400 (posn-y (mouse-click-posn click)) 500))
     '(6 4))
    ((and (<= 600 (posn-x (mouse-click-posn click)) 700)
          (<= 500 (posn-y (mouse-click-posn click)) 600))
     '(6 5))
    ((and (<= 600 (posn-x (mouse-click-posn click)) 700)
          (<= 600 (posn-y (mouse-click-posn click)) 700))
     '(6 6))
    ((and (<= 600 (posn-x (mouse-click-posn click)) 700)
          (<= 700 (posn-y (mouse-click-posn click)) 800))
     '(6 7))
    ;Octava columna
    ((and (<= 700 (posn-x (mouse-click-posn click)) 800)
          (<= 0 (posn-y (mouse-click-posn click)) 100))
     '(7 0))
    ((and (<= 700 (posn-x (mouse-click-posn click)) 800)
          (<= 100 (posn-y (mouse-click-posn click)) 200))
     '(7 1))
    ((and (<= 700 (posn-x (mouse-click-posn click)) 800)
          (<= 200 (posn-y (mouse-click-posn click)) 300))
     '(7 2))
    ((and (<= 700 (posn-x (mouse-click-posn click)) 800)
          (<= 300 (posn-y (mouse-click-posn click)) 400))
     '(7 3))
    ((and (<= 700 (posn-x (mouse-click-posn click)) 800)
          (<= 400 (posn-y (mouse-click-posn click)) 500))
     '(7 4))
    ((and (<= 700 (posn-x (mouse-click-posn click)) 800)
          (<= 500 (posn-y (mouse-click-posn click)) 600))
     '(7 5))
    ((and (<= 700 (posn-x (mouse-click-posn click)) 800)
          (<= 600 (posn-y (mouse-click-posn click)) 700))
     '(7 6))
    ((and (<= 700 (posn-x (mouse-click-posn click)) 800)
          (<= 700 (posn-y (mouse-click-posn click)) 800))
     '(7 7))
))
    
(define (posiciones_tablero x y jugador)
    (cond
      [(and (equal? x 0) (equal? y 0)) ;Para x=0 e y=0
       (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "white.jpeg" (make-posn 10 10))] ;make-posn x y posicion de la ventana donde coloca la imagen
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "black.jpeg" (make-posn 10 10))])
       ]
      [(or(and (equal? x 1) (equal? y 0)) (and (equal? x 2) (equal? y 0)) ;Para 8>x>0 e y=0
              (and (equal? x 3) (equal? y 0))(and (equal? x 4) (equal? y 0))(and (equal? x 5) (equal? y 0))
              (and (equal? x 6) (equal? y 0))(and (equal? x 7) (equal? y 0)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "white.jpeg" (make-posn (+ (* 100 x) 10) 10))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "black.jpeg" (make-posn (+ (* 100 x) 10) 10))])]
      [(or(and (equal? x 0) (equal? y 1)) (and (equal? x 0) (equal? y 2)) ;Para x=0 e 8>y>0
              (and (equal? x 0) (equal? y 3))(and (equal? x 0) (equal? y 4))(and (equal? x 0) (equal? y 5))
              (and (equal? x 0) (equal? y 6))(and (equal? x 0) (equal? y 7)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "white.jpeg" (make-posn 10 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "black.jpeg" (make-posn 10 (+ (* 100 y) 10)))])]    
      
      [(or(and (equal? x 1) (equal? y 1)) (and (equal? x 1) (equal? y 2)) ;Para x=1 e 8>y>0
              (and (equal? x 1) (equal? y 3))(and (equal? x 1) (equal? y 4))(and (equal? x 1) (equal? y 5))
              (and (equal? x 1) (equal? y 6))(and (equal? x 1) (equal? y 7)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "white.jpeg" (make-posn 110 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "black.jpeg" (make-posn 110 (+ (* 100 y) 10)))])]
      
      [(or(and (equal? x 2) (equal? y 1)) (and (equal? x 2) (equal? y 2)) ;Para x=2 e 8>y>0
              (and (equal? x 2) (equal? y 3))(and (equal? x 2) (equal? y 4))(and (equal? x 2) (equal? y 5))
              (and (equal? x 2) (equal? y 6))(and (equal? x 2) (equal? y 7)))
          (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "white.jpeg" (make-posn 210 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "black.jpeg" (make-posn 210 (+ (* 100 y) 10)))])]
      
      [(or(and (equal? x 3) (equal? y 1)) (and (equal? x 3) (equal? y 2)) ;Para x=3 e 8>y>0
              (and (equal? x 3) (equal? y 3))(and (equal? x 3) (equal? y 4))(and (equal? x 3) (equal? y 5))
              (and (equal? x 3) (equal? y 6))(and (equal? x 3) (equal? y 7)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "white.jpeg" (make-posn 310 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "black.jpeg" (make-posn 310 (+ (* 100 y) 10)))])]
      
      [(or(and (equal? x 4) (equal? y 1)) (and (equal? x 4) (equal? y 2)) ;Para x=4 e 8>y>0
              (and (equal? x 4) (equal? y 3))(and (equal? x 4) (equal? y 4))(and (equal? x 4) (equal? y 5))
              (and (equal? x 4) (equal? y 6))(and (equal? x 4) (equal? y 7)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "white.jpeg" (make-posn 410 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "black.jpeg" (make-posn 410 (+ (* 100 y) 10)))])]
      
      [(or(and (equal? x 5) (equal? y 1)) (and (equal? x 5) (equal? y 2)) ;Para x=5 e 8>y>0
              (and (equal? x 5) (equal? y 3))(and (equal? x 5) (equal? y 4))(and (equal? x 5) (equal? y 5))
              (and (equal? x 5) (equal? y 6))(and (equal? x 5) (equal? y 7)))
          (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "white.jpeg" (make-posn 510 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "black.jpeg" (make-posn 510 (+ (* 100 y) 10)))])]
      
      [(or(and (equal? x 6) (equal? y 1)) (and (equal? x 6) (equal? y 2)) ;Para x=6 e 8>y>0
              (and (equal? x 6) (equal? y 3))(and (equal? x 6) (equal? y 4))(and (equal? x 6) (equal? y 5))
              (and (equal? x 6) (equal? y 6))(and (equal? x 6) (equal? y 7)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "white.jpeg" (make-posn 610 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "black.jpeg" (make-posn 610 (+ (* 100 y) 10)))])]
      
      [(or(and (equal? x 7) (equal? y 1)) (and (equal? x 7) (equal? y 2)) ;Para x=7 e 8>y>0
              (and (equal? x 7) (equal? y 3))(and (equal? x 7) (equal? y 4))(and (equal? x 7) (equal? y 5))
              (and (equal? x 7) (equal? y 6))(and (equal? x 7) (equal? y 7)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "white.jpeg" (make-posn 710 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "black.jpeg" (make-posn 710 (+ (* 100 y) 10)))])]))

(define (insertar_todo jugador) ;Funcion para comprobar rapidamente si se insertan correctamente las piezas
  (for* ([i 8][j 8])
    (posiciones_tablero i j jugador)
  )
)

; Resalta las posiciones válidas que puede realizar el jugador
(define (resaltar_posiciones_validas x y jugador) 
  (cond
      [(and (equal? x 0) (equal? y 0)) ;Para x=0 e y=0
       (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "place-white.jpeg" (make-posn 10 10))
              ] ;make-posn x y posicion de la ventana donde coloca la imagen
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "place-black.jpeg" (make-posn 10 10)) ])
       ]
      [(or(and (equal? x 1) (equal? y 0)) (and (equal? x 2) (equal? y 0)) ;Para 8>x>0 e y=0
              (and (equal? x 3) (equal? y 0))(and (equal? x 4) (equal? y 0))(and (equal? x 5) (equal? y 0))
              (and (equal? x 6) (equal? y 0))(and (equal? x 7) (equal? y 0)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "place-white.jpeg" (make-posn (+ (* 100 x) 10) 10))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "place-black.jpeg" (make-posn (+ (* 100 x) 10) 10))])]
      [(or(and (equal? x 0) (equal? y 1)) (and (equal? x 0) (equal? y 2)) ;Para x=0 e 8>y>0
              (and (equal? x 0) (equal? y 3))(and (equal? x 0) (equal? y 4))(and (equal? x 0) (equal? y 5))
              (and (equal? x 0) (equal? y 6))(and (equal? x 0) (equal? y 7)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "place-white.jpeg" (make-posn 10 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "place-black.jpeg" (make-posn 10 (+ (* 100 y) 10)))])]    
      
      [(or(and (equal? x 1) (equal? y 1)) (and (equal? x 1) (equal? y 2)) ;Para x=1 e 8>y>0
              (and (equal? x 1) (equal? y 3))(and (equal? x 1) (equal? y 4))(and (equal? x 1) (equal? y 5))
              (and (equal? x 1) (equal? y 6))(and (equal? x 1) (equal? y 7)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "place-white.jpeg" (make-posn 110 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "place-black.jpeg" (make-posn 110 (+ (* 100 y) 10)))])]
      
      [(or(and (equal? x 2) (equal? y 1)) (and (equal? x 2) (equal? y 2)) ;Para x=2 e 8>y>0
              (and (equal? x 2) (equal? y 3))(and (equal? x 2) (equal? y 4))(and (equal? x 2) (equal? y 5))
              (and (equal? x 2) (equal? y 6))(and (equal? x 2) (equal? y 7)))
          (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "place-white.jpeg" (make-posn 210 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "place-black.jpeg" (make-posn 210 (+ (* 100 y) 10)))])]
      
      [(or(and (equal? x 3) (equal? y 1)) (and (equal? x 3) (equal? y 2)) ;Para x=3 e 8>y>0
              (and (equal? x 3) (equal? y 3))(and (equal? x 3) (equal? y 4))(and (equal? x 3) (equal? y 5))
              (and (equal? x 3) (equal? y 6))(and (equal? x 3) (equal? y 7)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "place-white.jpeg" (make-posn 310 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "place-black.jpeg" (make-posn 310 (+ (* 100 y) 10)))])]
      
      [(or(and (equal? x 4) (equal? y 1)) (and (equal? x 4) (equal? y 2)) ;Para x=4 e 8>y>0
              (and (equal? x 4) (equal? y 3))(and (equal? x 4) (equal? y 4))(and (equal? x 4) (equal? y 5))
              (and (equal? x 4) (equal? y 6))(and (equal? x 4) (equal? y 7)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "place-white.jpeg" (make-posn 410 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "place-black.jpeg" (make-posn 410 (+ (* 100 y) 10)))])]
      
      [(or(and (equal? x 5) (equal? y 1)) (and (equal? x 5) (equal? y 2)) ;Para x=5 e 8>y>0
              (and (equal? x 5) (equal? y 3))(and (equal? x 5) (equal? y 4))(and (equal? x 5) (equal? y 5))
              (and (equal? x 5) (equal? y 6))(and (equal? x 5) (equal? y 7)))
          (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "place-white.jpeg" (make-posn 510 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "place-black.jpeg" (make-posn 510 (+ (* 100 y) 10)))])]
      
      [(or(and (equal? x 6) (equal? y 1)) (and (equal? x 6) (equal? y 2)) ;Para x=6 e 8>y>0
              (and (equal? x 6) (equal? y 3))(and (equal? x 6) (equal? y 4))(and (equal? x 6) (equal? y 5))
              (and (equal? x 6) (equal? y 6))(and (equal? x 6) (equal? y 7)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "place-white.jpeg" (make-posn 610 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "place-black.jpeg" (make-posn 610 (+ (* 100 y) 10)))])]
      
      [(or(and (equal? x 7) (equal? y 1)) (and (equal? x 7) (equal? y 2)) ;Para x=7 e 8>y>0
              (and (equal? x 7) (equal? y 3))(and (equal? x 7) (equal? y 4))(and (equal? x 7) (equal? y 5))
              (and (equal? x 7) (equal? y 6))(and (equal? x 7) (equal? y 7)))
           (cond [(equal? jugador "B")
              ((draw-pixmap Reversi) "place-white.jpeg" (make-posn 710 (+ (* 100 y) 10)))]
             [(equal? jugador "N")
              ((draw-pixmap Reversi) "place-black.jpeg" (make-posn 710 (+ (* 100 y) 10)))])]    
           
    )
  )
(define (insertar_posicion_posible lista jugador) ;Permite meter una ficha a partir de una lista
  (cond [(empty? lista) 0]
        [else (resaltar_posiciones_validas (list-ref(car lista )0) (list-ref(car lista )1) jugador) (insertar_posicion_posible (cdr lista) jugador) ]
  )
 )


(require embedded-gui)

;Ventana de Inicio de modo de juego
(define mi-ventana (new frame%
                        [label "Reversi"]
                        [width 300]
                        [height 300]
                        [style '(fullscreen-button)]
                        [alignment '(center center)]))
(define mensaje (new message%
                 [parent mi-ventana]
                 [label "Elija el modo de juego:"]
                 [vert-margin 20]
                 [horiz-margin 20]))
; Panel para colocar los botones
(define panel (new horizontal-pane%
                   [parent mi-ventana]
                   [vert-margin 20]
                   [horiz-margin 20]
                   [alignment '(left bottom)]
                   [stretchable-width #t]
                   [stretchable-height #t]))
(send mi-ventana show #t);Para visualizar la ventana
(new button%
     [label "&Contra maquina"]
     [parent panel]
     [vert-margin 10]
     [horiz-margin 10]
     [callback (lambda (button event) (begin (send mi-ventana show #f)(select_algoritmo)))])
(new button%
     [label "&Contra jugador"]
     [parent panel]
     [vert-margin 10]
     [horiz-margin 10]
     [callback (lambda (button event) (begin (send mi-ventana show #f) (juego matrizInicial "minimax" 2 0 5)))])

(define (select_algoritmo) ; Abre una interfaz para elegir el algoritmo de la máquina
  (define ventana-algoritmo (new frame%
                        [label "Reversi"]
                        [width 300]
                        [height 300]
                        [style '(fullscreen-button)]
                        [alignment '(center center)]))
  (define txt-algoritmo (new message%
                 [parent ventana-algoritmo]
                 [label "Elija un algoritmo:"]
                 [vert-margin 20]
                 [horiz-margin 20]))
  (define panel-algoritmo (new horizontal-pane%
                   [parent ventana-algoritmo]
                   [vert-margin 20]
                   [horiz-margin 20]
                   [alignment '(left bottom)]
                   [stretchable-width #t]
                   [stretchable-height #t]))
  (send ventana-algoritmo show #t)
  (new button%
       [label "&Minimax"]
       [parent panel-algoritmo]
       [vert-margin 10]
       [horiz-margin 10]
       [callback (lambda (button event) (begin (send ventana-algoritmo show #f)(select_dificultad "minimax")))])
  
  (new button%
     [label "&Poda alfa-beta"]
     [parent panel-algoritmo]
     [vert-margin 10]
     [horiz-margin 10]
     [callback (lambda (button event) (begin (send ventana-algoritmo show #f) (select_dificultad "poda")))]))

(define (select_dificultad algoritmo) ; Abre una interfaz para elegir la dificultad del algoritmo que utiliza la máquina
  (define ventana-dificultad (new frame%
                        [label "Reversi"]
                        [width 300]
                        [height 300]
                        [style '(fullscreen-button)]
                        [alignment '(center center)]))
  (define txt-dificultad (new message%
                 [parent ventana-dificultad]
                 [label "Elija una dificultad:"]
                 [vert-margin 20]
                 [horiz-margin 20]))
  (define panel-dificultad (new horizontal-pane%
                   [parent ventana-dificultad]
                   [vert-margin 20]
                   [horiz-margin 20]
                   [alignment '(left bottom)]
                   [stretchable-width #t]
                   [stretchable-height #t]))
  (send ventana-dificultad show #t)
  (new button%
       [label "&Fácil"]
       [parent panel-dificultad]
       [vert-margin 10]
       [horiz-margin 10]
       [callback (lambda (button event) (begin (send ventana-dificultad show #f)(juego matrizInicial algoritmo 1 0 3)))])
  
  (new button%
     [label "&Media"]
     [parent panel-dificultad]
     [vert-margin 10]
     [horiz-margin 10]
     [callback (lambda (button event) (begin (send ventana-dificultad show #f) (juego matrizInicial algoritmo 1 0 4)))])
  (new button%
     [label "&Difícil"]
     [parent panel-dificultad]
     [vert-margin 10]
     [horiz-margin 10]
     [callback (lambda (button event) (begin (send ventana-dificultad show #f) (juego matrizInicial algoritmo 1 0 5)))]))

(define (put_aviso); Muestra un aviso de no poder realizar un movimiento
  (define ventana-aviso (new frame%
                        [label "Reversi"]
                        [width 400]
                        [height 200]
                        [style '(fullscreen-button)]
                        [alignment '(center center)]))
  (define txt-aviso (new message%
                 [parent ventana-aviso]
                 [label "No dispone de movimientos"]
                 [vert-margin 20]
                 [horiz-margin 20]))
  (define panel-aviso (new horizontal-pane%
                   [parent ventana-aviso]
                   [vert-margin 20]
                   [horiz-margin 20]
                   [alignment '(left bottom)]
                   [stretchable-width #t]
                   [stretchable-height #t]))
  (send ventana-aviso show #t)
  (new button%
       [label "&Aceptar"]
       [parent panel-aviso]
       [vert-margin 10]
       [horiz-margin 10]
       [callback (lambda (button event) (begin (send ventana-aviso show #f)))]))

(define (ver_resultado texto) ; Muestra el resultado de la partida
  (define ventana-resultado (new frame%
                        [label "Reversi"]
                        [width 400]
                        [height 200]
                        [style '(fullscreen-button)]
                        [alignment '(center center)]))
  (define txt-resultado (new message%
                 [parent ventana-resultado]
                 [label texto]
                 [vert-margin 20]
                 [horiz-margin 20]))
  (define panel-resultado (new horizontal-pane%
                   [parent ventana-resultado]
                   [vert-margin 20]
                   [horiz-margin 20]
                   [alignment '(left bottom)]
                   [stretchable-width #t]
                   [stretchable-height #t]))
  (send ventana-resultado show #t)
  (new button%
       [label "&Aceptar"]
       [parent panel-resultado]
       [vert-margin 10]
       [horiz-margin 10]
       [callback (lambda (button event) (begin (send ventana-resultado show #f)))]))