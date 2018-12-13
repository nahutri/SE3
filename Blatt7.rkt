#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require racket/trace)

#|
  SE3 Funktionale Programmierung
  WiSe 2017/18, Uni Hamburg
  Aufgabenblatt 07
  Eingereicht von
    My Anna Nguyen
    Araf Sadeghi 6893272
    Daniel Djahang 6803168
  Beispielaufrufe befinden sich immer am Ende der Aufgabe
  
|#

; ##########################################################################################
; ## Aufgabe 1  ############################################################################
; ##########################################################################################
; Rekursive Funktion
(define (zaehlen x xs)
 (if (empty? xs)
      0
      (+ (if (= (car xs) x) 1 0) (zaehlen x (cdr xs)))))                 
(set! zaehlen zaehlen)
;>> Was macht ihr hier?

; Endrekursive Funktion
(define (zaehlen2 x xs (count 0)) 
  (if (empty? xs)
       count
      (zaehlen2 x (cdr xs)(if (= x (car xs)) (+ count 1) count))))
(set! zaehlen2 zaehlen2)

; Funktionen höherer Ordnung
(define (zaehlen3 x xs)
   (apply + (map (lambda (y) (if (false? y) 0 1)) (map (lambda (y)   (= x y))  xs)))
  )
;>> Etwas kompliziert, aber ok.
;>> (define (zaehlenHO2 x xs)
;>>  (length (filter (curry equal? x) xs)))

(trace zaehlen)
(trace zaehlen2)
(display "Rekursiv: ")
(zaehlen 3 '(2 4 3 2 3 4 1))
(display "Endrekursiv: ")
(zaehlen2 3 '(2 4 3 2 3 4 1))
(display "Funktionen höherer Ordnung: ")
(zaehlen3 3 '(2 4 3 2 3 4 1))

;>> 10 Pkt.

; ############################################################################################
; ## Aufgabe 2.1 #############################################################################
; ############################################################################################

; Eine Funktion um ein Spielbrett mit Anfangszustand für Game Of Life zu erzeugen
; X und Y Koordinaten sind wie in Python geregelt (also sind vertauscht)
; @param l Liste von Listen mit ánführender x Koordinate und den dazugehörigen Y-Koordinaten 
; @param a (Default 0) zur Erzeugung eines 30x30 Spielbrettes
; @param b (Default 30) zur Erzeugung eines 30x30 Spielbrettes
; @return Spielbrett mit Anfangszustand
(define (stage l (a 0) (b 30))
  (if (= a b)
      '()
      (if (and (not (equal? l '())) (= (+ 1 a) (caar l)) )
      (cons (width (cdar l)) (stage (cdr l) (+ a 1)))
      (cons (width '()) (stage l (+ a 1) )))          
      )
  )

; Eine Hilfsfunktion für stage um für jede X Koordinate 30 Y-Koordinaten zu erzeugen
; @param l Liste von Y-Koordinaten
; @param a (Default 0)
; @param b (Default 30)
; @return Liste mit 30 Y-Koordinaten für ein X Koordinat
(define (width l (a 0) (o 30))
  
  (if (= a o)
       '()
       (if (and (not (equal? l '())) (= (+ 1 a) (car  l)))
           (cons  1 (width (cdr l) (+ a 1)) )
           (cons  0  (width l (+ a 1) ) ))                     
      )
  )

; Eigene Stage
(define mystage (stage '((9 15) (10 16) (11 14 15 16))))

;>> Aufgabe 2.4
;>> 5 Pkt.

; ############################################################################################
; ## Aufgabe 2.2 #############################################################################
; ############################################################################################

; Hilfsfunktion für show, zeichnet die Y-Koordinaten für ein gegebens X
; @param brett Spielbrett
; @param x (Default 1)
; @param y (Default 1)
; @return Image von Quadraten für jedes Y für ein gegebenes X
(define (drawWidth brett (x 1) (y 1))
  (if (= y 31)
      empty-image    
      (beside (if (= (list-ref(list-ref brett (- x 1)) (- y 1)) 0) 
                  (square 10 "outline" "black")
                  (square 10 "solid" "black")) 
      (drawWidth brett x (+ y 1))))
  )

; Funktion um das Spielbrett anzuzeigen
; @param brett Spielbrett
; @param x (Default 1)
; @param y (Default 1)
; @return Image von Spielbrett
(define (show brett (x 1) (y 1))
  (if (= x 31)
      empty-image    
      (above/align "center" (drawWidth brett x y)  (show brett (+ x 1)))))

;>> Aufgabe 2.2
;>> 5 Pkt.

; ############################################################################################
; ## Aufgabe 2.3 #############################################################################
; ############################################################################################

; Funktion, um das resultierende Ereignis aus der Anzahl der Nachbarn auszuwerten
; @param x X-Koordinate
; @param y Y-Koordinate
; @param brett Spielbrett
; @param l Liste mit 8 booleschen Werten
; @return tot oder lebend ( 0 oder 1)
(define (event x y brett l)
  (let ([process (apply + l)])    
    (cond
      [(= process 3)  1]
      [(< process 2)  0]
      [(> process 3)  0]
      [(= process 2) (list-ref(list-ref brett (- x 1)) (- y 1))])
    )
  )

; Hilfsfunktion für update, updatet die Y-Koordinaten für ein gegebens X
; @param brett Spielbrett
; @param x (Default 1)
; @param y (Default 1)
; @return geupdatete Liste von Y-Koordinaten für ein gegebenes X
(define (uWidth brett (x 1) (y 1))
  (if (= y 31)
      '()    
      (cons (event x y brett (neighbor x y brett)) (uWidth brett x (+ y 1)))))
  
; Funktion zum updaten des Zustandes des Spielbrettes
; @param brett Spielbrett
; @param x (Default 1)
; @param y (Default 1)
; @return geupdatetes Spielbrett
(define (update brett (x 1) (y 1))
  (if (= x 31)
      '()
      (cons (uWidth brett x y) (update brett (+ x 1)))))


; Funktion zur Ermittlung aller 8 Nachbarn mit Einbezug von Spezialfällen wie Corner und Edges
; @param x X-Koordinate beginnend mit 1 und endend mit 30
; @param y X-Koordinate beginnen mit 1 und endend mit 30
; @param brett Spielbrett
; @param m und m2 Matrix zur Betrachtung der Nachbarn
; @return Liste mit booleschen Werten 0 für tot und 1 für lebend
(define (neighbor x y brett (m 3) (m2 3))
  (if (or (= x 1) (= x 30) (= y 1) (= y 30))      
   (specHandler x y brett)           
    (if (and (= m 0) (= m2 0))
        '()
        (if (> m 0)
            (if (> m2 0)
                (cons (list-ref (list-ref brett (+(- x 1) (- m 2))) (+ (- y 1) (- m2 2))) (neighbor  x  y brett m (- m2 1)))
                (neighbor x y brett (- m 2)))   
            (if (= m -1)                 
                (neighbor x y brett 0 4)
                (cons (list-ref (list-ref brett (+(- x 1)  m )) (+ (- y 1) (- m2 3))) (neighbor x y brett m (- m2 2)))))
      )
   )
 )

;>> Aufgabe 2.3
;>> 15 Pkt.

; ############################################################################################
; ## Aufgabe 2.5 #############################################################################
; ############################################################################################

; Hilfsfunktion für neighbor um alle Spezialfälle zu handlen
; @param x X-Koordinate
; @param y Y-Koordinate
; @param brett Spielbrett
; @return Liste mit booleschen Werten für den zutreffenden Spezialfall
(define (specHandler x y brett)
  
  (if (and (= x 1) (= y 1))
        (corner brett)
        
       (if (and (= x 1) (= y 30))
        (corner2 brett)
        
       (if (and (= x 30) (= y 1))
        (corner3  brett)
        
       (if (and (= x 30) (= y 30))
        (corner4  brett)
        
       (if (= y 1)
        (edge x y brett)
        
       (if (= y 30)
        (edge2 x y brett)
        
       (if (= x 1)
        (edge3 x y brett)
        
       (edge4 x y brett))  ) ) ) ) )
     )
  )



; Spezialfall 1: 0 < x < 30   y = 1
; @param x X-Koordinate beginnend mit 1 und endend mit 30
; @param y X-Koordinate beginnen mit 1 und endend mit 30
; @param brett Spielbrett
; @param m und m2 Matrix zur Betrachtung der Nachbarn
; @return Liste mit booleschen Werten
(define (edge x y brett (m 3) (m2 3))
  (if (and (= m 0) (= m2 0))
        '()
        (if (> m2 0)
            (if (> m 0)
                (cons (list-ref (list-ref brett (+(- x 1) (- m 2))) (+ (- y 1) (- m2 2))) (edge  x  y brett ( - m 1) m2))
                (edge x 31 brett 3 (- m2 2)))   
            (if (= m2 -1)                 
                (edge x 1 brett 4 0)
                (cons (list-ref (list-ref brett (+(- x 1)  (- m 3) )) (+ (- y 1)  m2)) (edge x y brett (- m 2) m2))))
      )
   )

; Spezialfall 2: 0 < x < 30 y = 30
; @param x X-Koordinate beginnend mit 1 und endend mit 30
; @param y X-Koordinate beginnen mit 1 und endend mit 30
; @param brett Spielbrett
; @param m und m2 Matrix zur Betrachtung der Nachbarn
; @return Liste mit booleschen Werten
(define (edge2 x y brett (m 3) (m2 3))
  (if (and (= m 0) (= m2 0))
        '()
        (if (and (< m2 7 )(> m2 0))
            (if (> m 0)
                (cons (list-ref (list-ref brett (+(- x 1) (- m 2))) (+ (- y 1) (- m2 4))) (edge2  x  y brett ( - m 1) m2))
                (edge2 x 0 brett 3 (+ m2 2)))   
            (if (= m2 7)                 
                (edge2 x 30 brett 4 0)
                (cons (list-ref (list-ref brett (+(- x 1)  (- m 3) )) (+ (- y 1)  m2)) (edge2 x y brett (- m 2) m2))))
      )
   )

; Spezialfall 3: 0 < y < 30 x = 1
; @param x X-Koordinate beginnend mit 1 und endend mit 30
; @param y X-Koordinate beginnen mit 1 und endend mit 30
; @param brett Spielbrett
; @param m und m2 Matrix zur Betrachtung der Nachbarn
; @return Liste mit booleschen Werten
(define (edge3 x y brett (m 3) (m2 3))
   (if (and (= m 0) (= m2 0))
        '()
        (if (> m 0)
            (if (> m2 0)
                (cons (list-ref (list-ref brett (+(- x 1) (- m 2))) (+ (- y 1) (- m2 2))) (edge3  x  y brett m (- m2 1)))
                (edge3 31 y brett (- m 2)))   
            (if (= m -1)                 
                (edge3 1 y brett 0 4)
                (cons (list-ref (list-ref brett (+(- x 1)  m )) (+ (- y 1) (- m2 3))) (edge3 x y brett m (- m2 2)))))
      )
  )

; Spezialfall 4: 0 < y < 30 x = 30
; @param x X-Koordinate beginnend mit 1 und endend mit 30
; @param y X-Koordinate beginnen mit 1 und endend mit 30
; @param brett Spielbrett
; @param m und m2 Matrix zur Betrachtung der Nachbarn
; @return Liste mit booleschen Werten
(define (edge4 x y brett (m 3) (m2 3))
    (if (and (= m 0) (= m2 0))
        '()
        (if (and (> m 0) (< m 7))
            (if (> m2 0)
                (cons (list-ref (list-ref brett (+(- x 1) (- m 4))) (+ (- y 1) (- m2 2))) (edge4  x  y brett m (- m2 1)))
                (edge4 0 y brett (+ m 2)))   
            (if (= m 7)                 
                (edge4 30 y brett 0 4)
                (cons (list-ref (list-ref brett (+(- x 1)  m )) (+ (- y 1) (- m2 3))) (edge4 x y brett m (- m2 2)))))
      )
  )

; Spezialfall 5: x = 1 y = 1
; @param brett Spielbrett
; @return Liste mit booleschen Werten
(define (corner brett)
  (list
  (list-ref (list-ref brett 29) 29)
  (list-ref (list-ref brett 0) 1)
  (list-ref (list-ref brett 1) 0)
  (list-ref (list-ref brett 1) 1)
  (list-ref (list-ref brett 29) 0)
  (list-ref (list-ref brett 29) 1)
  (list-ref (list-ref brett 0) 29)
  (list-ref (list-ref brett 1) 29)
  )
 )

; Spezialfall 6: x = 1 y = 30
; @param brett Spielbrett
; @return Liste mit booleschen Werten
(define (corner2 brett)
  (list
  (list-ref (list-ref brett 29) 29)
  (list-ref (list-ref brett 29) 28)
  (list-ref (list-ref brett 0) 0)
  (list-ref (list-ref brett 1) 0)
  (list-ref (list-ref brett 29) 0)
  (list-ref (list-ref brett 0) 28)
  (list-ref (list-ref brett 1) 28)
  (list-ref (list-ref brett 1) 29)
  )
 )

; Spezialfall 7: x = 30 y = 1
; @param brett Spielbrett
; @return Liste mit booleschen Werten
(define (corner3 brett)
  (list
  (list-ref (list-ref brett 29) 29)
  (list-ref (list-ref brett 28) 29)
  (list-ref (list-ref brett 29) 1)
  (list-ref (list-ref brett 28) 0)
  (list-ref (list-ref brett 28) 1)
  (list-ref (list-ref brett 0) 0)
  (list-ref (list-ref brett 0) 29)
  (list-ref (list-ref brett 0) 1)
  )
 )

; Spezialfall 8: x = 30 y = 30
; @param brett Spielbrett
; @return Liste mit booleschen Werten
(define (corner4 brett)
  (list
  (list-ref (list-ref brett 0) 0)
  (list-ref (list-ref brett 28) 0)
  (list-ref (list-ref brett 29) 0)
  (list-ref (list-ref brett 0) 28)
  (list-ref (list-ref brett 0) 29)
  (list-ref (list-ref brett 29) 28)
  (list-ref (list-ref brett 28) 28)
  (list-ref (list-ref brett 28) 29)
  )
 )

;>> Aufgabe 2.5
;>> Das ist unnötig kompliziert.
;>> Ihr könnt die x/y-Werte einfach modulo 30 rechnen und alles andere bleibt wie gehabt.
;>> 5 Pkt.

; ############################################################################################
; ## Aufgabe 2.4 #############################################################################
; ############################################################################################

; Funktion zur Animation mit - wie anefordert - 4 Bildern pro Sekunde 
(big-bang  mystage
        (on-tick update (/ 1 4))
         (to-draw show ))

;>> Aufgabe 2.4
;>> 5 Pkt.
         

;>> Gesamt: 45 Pkt.
