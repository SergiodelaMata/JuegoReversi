#lang racket
(require rackunit
         "Reversi.rkt")
         
(define TurnoBlancas
  (list
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "B" "N" "N" "-" "-" "-")
   (list "-" "-" "-" "N" "N" "-" "-" "-")
   (list "-" "-" "N" "N" "B" "N" "-" "-")
   (list "-" "-" "-" "B" "-" "B" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   ))

(define TurnoNegras
  (list
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "B" "N" "N" "-" "-" "-")
   (list "-" "-" "-" "N" "N" "-" "-" "-")
   (list "-" "B" "B" "B" "B" "N" "-" "-")
   (list "-" "-" "-" "B" "-" "B" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   ))

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



;Prueba unitaria para las posiciones validas para piezas Blancas
(check-equal? (get_lista_pos_validas TurnoBlancas "B") '((1 3) (1 4) (2 5) (3 1) (3 5) (4 1) (4 6)))

;Prueba unitaria para las posiciones validas para piezas Negras
(check-equal? (get_lista_pos_validas TurnoNegras "N") '((2 1) (4 0) (5 1) (5 2) (5 4) (6 3) (6 5)))

;Prueba unitaria para ver si cuenta bien el número de piezas negras
(check-equal? (contar_piezas TurnoNegras "N") 5)

;Prueba unitaria para ver si cuenta bien el número de piezas blancas
(check-equal? (contar_piezas TurnoBlancas "B") 4)

;Prueba unitaria para ver si cambia correctamente el color de la matriz
(check-equal? (cambiar_color matrizInicial '(3 2) "N")
              (list
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "N" "N" "N" "-" "-" "-")
   (list "-" "-" "-" "N" "B" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   (list "-" "-" "-" "-" "-" "-" "-" "-")
   ))
;Prueba unitaria para ver si devuelve correctamente el color del oponente
(check-equal? (select_oponente "N") "B")

;Prueba unitaria para comprobar si devuelve correctamente el máximo valor para minimax
(check-equal? (get_max_minimax (list 2 4 6 -1 4 10 2)) 10)

;Prueba unitaria para comprobar si devuelve correctamente el máximo valor para poda alfa-beta
(check-equal? (get_max_poda (list 2 4 6 -1 4 10 2) 15) 15)

