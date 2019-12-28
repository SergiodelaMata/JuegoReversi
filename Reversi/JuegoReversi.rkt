#lang racket
(define matrizInicial
  (list
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "B" "N" "-" "-" "-")
   (list "-" "-" "-" "N" "B" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   ))

(define (get_lista_pos_validas matriz color)
  (define lista_pos_validas '())
  (for*/list ([i 8] [j 8]
    #:when(and (equal? (is_empty matriz  (list i j)) #t) (or (equal? (comprobar_fila matriz color (list i j)) #t) (equal? (comprobar_columna matriz color (list i j)) #t) (equal? (comprobar_diagonales matriz color (list i j)) #t))))
    (append lista_pos_validas (list i j))))

(define (is_empty matriz pos)
  (if (equal? (list-ref(list-ref matriz (car pos)) (cdr pos)) "-")
     #t #f))

(define (comprobar_fila matriz color pos)
  (cond
    ;Situación en la que se tiene en cuenta la primera o la segunda posición de la fila
    [(or (equal? (cdr pos) 0) (equal? (cdr pos) 1))
     (comp_fila_asc matriz color pos (+ (cdr pos) 1) #f)] ; Estudia las posiciones posteriores
    ;Situación en la que se tiene en cuenta la última o la penúltima posición de la fila
    [(or (equal? (cdr pos) 0) (equal? (cdr pos) 1))
     (comp_fila_desc matriz color pos (- (cdr pos) 1) #f)] ; Estudia las posiciones previas
    [else
     (or (comp_fila_desc matriz color pos (- (cdr pos) 1) #f) (comp_fila_asc matriz color pos (+ (cdr pos) 1) #f))]; Estudia las posiciones previas y posteriores
  ))

(define (comp_fila_asc matriz color pos num_col ver_opuesto)
  (cond
    [(equal? num_col 8) #f] ;No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz (car pos)) num_col) "-") #f] ;La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz (car pos)) num_col) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_fila_asc matriz color pos (+ num_col 1) #t)]
  ))

(define (comp_fila_desc matriz color pos num_col ver_opuesto)
  (cond
    [(equal? num_col -1) #f] ;No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz (car pos)) num_col) "-") #f] ;La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz (car pos)) num_col) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_fila_desc matriz color pos (- num_col 1) #t)]
  ))

(define (comprobar_columna matriz color pos)
  (cond
    ;Situación en la que se tiene en cuenta la primera o la segunda posición de la columna
    [(or (equal? (car pos) 0) (equal? (car pos) 1))
     (comp_columna_asc matriz color pos (+ (car pos) 1) #f)] ; Estudia las posiciones posteriores
    ;Situación en la que se tiene en cuenta la última o la penúltima posición de la columna
    [(or (equal? (car pos) 0) (equal? (car pos) 1))
     (comp_columna_desc matriz color pos (- (car pos) 1) #f)] ; Estudia las posiciones previas
    [else
     (or (comp_columna_desc matriz color pos (- (car pos) 1) #f) (comp_columna_asc matriz color pos (+ (car pos) 1) #f))]; Estudia las posiciones previas y posteriores
  ))

(define (comp_columna_asc matriz color pos num_fila ver_opuesto)
  (cond
    [(equal? num_fila 8) #f] ;No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz num_fila) (cdr pos)) "-") #f] ;La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz num_fila) (cdr pos)) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_columna_asc matriz color pos (+ num_fila 1) #t)]
  ))

(define (comp_columna_desc matriz color pos num_fila ver_opuesto)
  (cond
    [(equal? num_fila -1) #f] ;No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz num_fila) (cdr pos)) "-") #f] ;La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz num_fila) (cdr pos)) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_columna_desc matriz color pos (- num_fila 1) #t)]
  ))

(define (comprobar_diagonales matriz color pos)
  (or (comprobar_diagonal_principal matriz color pos)(comprobar_diagonal_secundaria matriz color pos))
  )

(define (comprobar_diagonal_principal matriz color pos)
  (cond
    [(and (equal? (car pos) 0) (or (equal? (cdr pos) 6) (equal? (cdr pos) 7))) #f]; La esquina superior derecha no interesa estudiar puesto que no va aportar nada
    [(and (equal? (car pos) 1) (equal? (cdr pos) 7)) #f]
    [(and (equal? (car pos) 7) (or (equal? (cdr pos) 0) (equal? (cdr pos) 1))) #f]; La esquina inferior izquierda no interesa estudiar puesto que no va aportar nada
    [(and (equal? (car pos) 6) (equal? (cdr pos) 0)) #f]
    [(or (equal? (car pos) 0) (equal? (car pos) 1)) (comp_diag_princ_desc matriz color (+ (car pos) 1) (+ (cdr pos) 1) #f)]
    [(or (equal? (car pos) 6) (equal? (car pos) 7)) (comp_diag_princ_desc matriz color (- (car pos) 1) (- (cdr pos) 1) #f)]
    [else (or (comp_diag_princ_desc matriz color (- (car pos) 1) (- (cdr pos) 1) #f) (comp_diag_princ_asc matriz color (+ (car pos) 1) (+ (cdr pos) 1) #f))]
    )
  )

(define (comp_diag_princ_asc matriz color num_fila num_col ver_opuesto)
  (cond
    [(or (equal? num_fila 8) (equal? num_col 8)) #f] ;No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz num_fila) num_col) "-") #f];La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz num_fila) num_col) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_diag_princ_asc matriz color (- num_fila 1) (- num_col 1) #t)]
    ))

(define (comp_diag_princ_desc matriz color num_fila num_col ver_opuesto)
  (cond
    [(or (equal? num_fila -1) (equal? num_col -1)) #f];No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz num_fila) num_col) "-") #f];La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz num_fila) num_col) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_diag_princ_desc matriz color (+ num_fila 1) (+ num_col 1) #t)]
    ))

(define (comprobar_diagonal_secundaria matriz color pos)
  (cond
    [(and (equal? (car pos) 0) (or (equal? (cdr pos) 0) (equal? (cdr pos) 1))) #f]; La esquina superior izquierda no interesa estudiar puesto que no va aportar nada
    [(and (equal? (car pos) 1) (equal? (cdr pos) 0)) #f]
    [(and (equal? (car pos) 7) (or (equal? (cdr pos) 6) (equal? (cdr pos) 7))) #f]; La esquina inferior derecha no interesa estudiar puesto que no va aportar nada
    [(and (equal? (car pos) 6) (equal? (cdr pos) 7)) #f]
    [(or (equal? (car pos) 0) (equal? (car pos) 1)) (comp_diag_sec_desc matriz color (+ (car pos) 1) (- (cdr pos) 1) #f)]
    [(or (equal? (car pos) 6) (equal? (car pos) 7)) (comp_diag_sec_asc matriz color (- (car pos) 1) (+ (cdr pos) 1) #f)]
    [else (or (comp_diag_sec_desc matriz color (+ (car pos) 1) (- (cdr pos) 1) #f) (comp_diag_sec_asc matriz color (- (car pos) 1) (+ (cdr pos) 1) #f))]
    )
  )

(define (comp_diag_sec_asc matriz color num_fila num_col ver_opuesto)
  (cond
    [(or (equal? num_fila -1) (equal? num_col 8)) #f] ;No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz num_fila) num_col) "-") #f];La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz num_fila) num_col) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_diag_princ_asc matriz color (- num_fila 1) (+ num_col 1) #t)]
    ))

(define (comp_diag_sec_desc matriz color num_fila num_col ver_opuesto)
  (cond
    [(or (equal? num_fila 8) (equal? num_col -1)) #f];No ha terminado con un valor del color del jugador
    [(equal? (list-ref(list-ref matriz num_fila) num_col) "-") #f];La posición estudiada es vacía
    [(equal? (list-ref(list-ref matriz num_fila) num_col) color) ver_opuesto] ; Hay una cadena de al menos una ficha del color del otro jugador y termina con otra del color del jugador
    [else (comp_diag_princ_desc matriz color (+ num_fila 1) (- num_col 1) #t)]
    ))