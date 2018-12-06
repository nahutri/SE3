#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require racket/trace)

#|
  SE3 Funktionale Programmierung
  WiSe 2017/18, Uni Hamburg
  Aufgabenblatt 06
  Eingereicht von
    My Anna Nguyen
    Araf Sadeghi 6893272
    Daniel Djahang 6803168
  Beispielaufrufe befinden sich immer am Ende der Aufgabe
  
|#

; ##########################################################################################
; ## Aufgabe 1.1.1  ########################################################################
; ##########################################################################################

;Bei der Funktion take liegt eine direkte, lineare Rekursion vor, da es bei jeder Fallunterscheidung sich selbst nur
;einmal verwendet und ein Speicheraufwand im Stack durch das Nachklappern nötig ist.
(define (take n xs)
;; das Kopfst \" uck einer Liste: die ersten n Elemente
(cond
(( null? xs) '())
((= 0 n) '())
(else (cons (car xs)
(take (- n 1) (cdr xs ))))))

;Bei der Funktion drop handelt es sich um eine direkte Endrekursion, da das Ergebnis der Rekursion nicht mehr mit anderen Termen im Stack verknüpft werden muss.
(define (drop n xs)
;; das Endst \" uck einer Liste entfernen
(cond
((null? xs) '())
((= 0 n) xs)
(else (drop (- n 1) (cdr xs )))))

;Bei der Funktion merge handelt es sich um eine direkte, Baumrekursion, da in der Fallunterscheidung im if-clause mehrfach Bezug auf die Definition genommen wird undzwar
;wenn einmal die Bedingung erfüllt ist und wenn die Bedingung einmal nicht erfüllt ist.
(define (merge rel<? xs ys)
;; mische zwei vorsortierte Listen xs und ys
;; entsprechend der Ordnungsrelation rel<?
(cond (( null? xs) ys)
(( null? ys) xs)
(else
(let ((cx (car xs))
(cy (car ys )))
(if (rel<? cx cy)
(cons cx (merge rel<? (cdr xs) ys))
(cons cy (merge rel<? (cdr ys) xs )))))))

;Bei der Funktion merge-sort handelt es sich um eine indirekte, geschachtelte Rekursion, da zum einen auf mehrere Defintionen wechselseitig rekursiv Bezug genommen
;wird (take, drop, merge) und zum anderen, weil bei der rekursion als Parameter eine Rekursion übergeben wird.
(define ( merge-sort rel<? xs)
;; Sortiere eine Liste xs entsprechend
;; der Ordnungsrelation rel<?
(let ((n (length xs )))
(if (<= n 1) xs
(let* (( n/2 ( quotient n 2))
(part1 (take n/2 xs))
(part2 (drop n/2 xs )))
(merge
rel<?
( merge-sort rel<? part1)
( merge-sort rel<? part2 ))))))

;>> merge ist nicht Baumrekursiv, da die beiden rekursiven Aufrufe alternativ sind.
;>> 9 Pkt.


; ##########################################################################################
; ## Aufgabe 1.1.2 #########################################################################
; ##########################################################################################

; merge und merge-sort sind beides Funktionen höherer Ordnung, da sie eine Funktion rel<? als Parameter erhalten.

;>> Was sind allgemein Funktionen höherer Ordnung?
;>> 2 Pkt.

; ##########################################################################################
; ## Aufgabe 1.1.3 #########################################################################
; ##########################################################################################

;Um aus take eine endrekursive Funktion zu machen, muss das cons durch elementare Funktionen ersetzt werden
;Dabei wird die übergebene liste umgedreht und immer so viele Elemente aus dieser umgekehrten Liste genommen bis n erreicht ist
;Das Prinzip ist im wesentlichen wie bei drop nur das hier nicht n = 0 sein muss sondern die Laenge der Liste = n
(define (take2 n xs)
;; das Kopfst \" uck einer Liste: die ersten n Elemente
(cond
(( null? xs) '())
((= (length xs) n) xs)
(else (take2 n (reverse(cdr (reverse xs))) ))))

(display "Ergebnis aus take: ")
(take 4 '(1 2 3 4 5 6 7 8))

(display "Ergebnis aus take2: ")
(take2 4 '(1 2 3 4 5 6 7 8))

;>> 3 Pkt.

; ##########################################################################################
; ## Aufgabe 2 #############################################################################
; ##########################################################################################

;;;;;;;;;;Geschenke;;;;;;;;;;;;

; zufällige Auswahl aus einer liste
; @param liste eine Liste
; @return zufälliges Element aus der Liste
(define (aimless liste)
  (list-ref liste (random (length liste))))

; Farbliste für Geschenke
(define farbe '("violet" "green" "cornflowerblue" "red" "darkmagenta" "darkorange" "darkred"))
; Farbliste für Schmuck
(define farbe2 '("darkgoldenrod" "darkred" "darkmagenta" "cornflowerblue" "orange" "silver"))

; ein Geschenk
(define (geschenk)
  (above/align "center"
          (beside (ellipse 7 5 "solid" "goldenrod") (ellipse 7 5 "solid" "goldenrod"))     
 (overlay/align "center" "center"
                (rectangle 5 30 "solid" "darkgoldenrod")
                (rectangle 30 5 "solid" "darkgoldenrod")
  (square 30 "solid" (aimless farbe))))
  )
; Stapel von Geschenken
; @param n Anzahl der Geschenke
; @return Stapel von Geschenken
(define (stapeln n)
  (if (= n 0) empty-image
       (above/align "center" (geschenk) (stapeln (- n 1))))
  )
         
; Funktion um Geschenke im 3er stapel anzuzeigen
; @param n Anzahl der Geschenke
; @return Geschenke im 3er Stapel
(define (geschenke n)
  (let ([r (modulo n 3)])
  (cond [ ( = n 0) empty-image]
         [(= r 0)  (beside/align "bottom"
                (stapeln 3) (geschenke (- n 3)))]
         [(= r 1)  (beside/align "bottom"
                (geschenke (- n 1)) (geschenk)) ]
         [(= r 2) (beside/align "bottom"
                (geschenke (- n 2)) (stapeln 2) )])
    )
  )
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Baum;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
; Diese Funktion dient dazu einen Weihnachtsbaum zu erstellen
; @param n Anzahl der Ebenen des Baumes
; @return Weihnachtsbaum
(define (calltree n)

  (above/align
   "center"
    (star 20 "solid" "yellow")
    (underlay (tree n)
              (above/align
   "center"
   (schmuck 4)
   (schmuck 14)
   (schmuck 20)
   (schmuck 20)
   (schmuck 14)
   (schmuck 15)

   ))
    
    (rectangle 20 50 "solid" "brown")
   ))

; Diese Funktion dient als Hilfsfunktion für den Weihnachtsbaum, erzeugt wird ein Tannenbaum ohne Stamm
; @param n Höhe des Baumes
; @return Tannenbaum ohne Stamm
(define (tree n)

(if (= n 0)    
    (beside (flip-horizontal (right-triangle 50 60 "solid" "darkgreen"))
            (right-triangle 50 60 "solid" "darkgreen"))
(above/align
 "center"
  (tree (- n 1))
  
 (beside (flip-horizontal (right-triangle 20 20 "solid" "darkgreen")) (h1  (+(modulo n 5)2)) (right-triangle 20 20 "solid" "darkgreen") ) )


))

; Diese Funktionen dient als Hilfsfunktion für den Tannenbaum 
; @param n Anzahl der Quadrate pro Ebene
; @return Ebene
(define (h1 n)
  (if (> n 0)
      (beside 
              (h1 (- n 1))  
      (square 20 "solid" "darkgreen"))
      (square 20 "solid" "darkgreen")
      ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Schmuck;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Diese Funktion dient dazu Schmuck für den Weihnachtsbaum zu erzeugen
; @param s Länge des Schmucks
; @return Schmuck
(define (schmuck s)

  (if (> s 0)
      (overlay
        (circle s "solid" (aimless farbe2)) ;; zeichnet einen Kreis mit zufälliger Farbe
       (beside
        (schmuck (- (/ s 2)1))
       (schmuck (- (/ s 2)1))))    ;;ruft zweimal sich selbst auf (Baumrekursion 2^n) fügt also zwei Kugeln nebeneinander
      

       
      (circle 5 "solid" (aimless farbe2))  ;;nach gewisser Rekursionstiefe(s <= 0) werden Kreise mit konstanter Größe zurückgegeben (dies ist die goldene Kette des Schmucks)
     )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Animation;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
; Eine Schneeflocke
(define snowflake
  
  (circle 3 "solid" "white")
)
; Hintergrund für die Animation
(define background  ;;FÜge alles zusammen außer die sich bewegenden Schneeflocken
  
(underlay/xy
(underlay/xy
(underlay/xy
 (add-curve (square 400 "solid" "black")   ;;erstelle einen quadratischen Hintergrund mit einer Kurve(Hügel)
             0 320 0 1/3
             400 400 0 1/3
             
             (make-pen "white" 200 "solid" "butt" "bevel")) 
                                ;;erstelle einen quadratischen Hintergrund mit einer Kurve(Hügel)
       
  
50 70
  (calltree 8));;Füge Baum hinzu(größe 8)
  250 200
  (geschenke 10))          ;;Füge Geschenke hinzu
  350 20
  (circle 20 "solid" "yellow"))
)

; Animation in Abhängigkeit der Zeit t
(define (christmas-scene t)
  (let((mov (*(sin (/ t 4) )5))
      ( height (* (sin (/ t 20)) 50)) )  ;;speichere eine langsame sin-bewegung für alle Schneeflocken


   (underlay/xy 
  (underlay/xy
   (underlay/xy
   (underlay/xy
    (underlay/xy
     (underlay/xy
      (underlay/xy
     background
   (+ mov 50) (modulo t 250) snowflake

  )   
     (+ mov 100) (modulo  (+ t 50) 250)  snowflake) ;;benutzt modulo um immer im Bildrahmen zu bleiben 
     (+ mov 150) (modulo  (+ t 200) 250)  snowflake)
     (+ mov 200) (modulo (+ t 300) 250) snowflake)
     (+ mov 250) (modulo (+ t 100) 250) snowflake)
    (+ mov 300) (modulo (+ t 250) 250) snowflake)
    (+ mov 350) (modulo (+ t 400) 250) snowflake)
    
))

(animate christmas-scene)

;>> 20 Pkt.
;>> 10 Pkt.

;>> Gesamt: 44 Pkt.
