#lang racket

#|
  SE3 Funktionale Programmierung
  WiSe 2017/18, Uni Hamburg
  Aufgabenblatt 02
  Eingereicht von
    My Anna Nguyen 6704054
    Araf Sadeghi 6893272
    Daniel Djahang 6803168
  Beispielaufrufe befinden sich immer am Ende der Aufgabe
  und können einfach auskommentiert werden.
|#


; ##############################################################################
; ## Aufgabe 1 #################################################################
; ##############################################################################
(define wuff 'Flocki ) (define Hund wuff ) (define Wolf  'wuff )
( define ( welcherNameGiltWo PersonA PersonB ) (let ( ( PersonA  'Zaphod) (PersonC PersonA) ) PersonC) )
(define xs1 '(0 2 3 wuff Hund) ) (define xs2 (list wuff Hund)) (define xs3 (cons Hund wuff) )




;wuff
;evaluiert zu 'Flocki, da es als dieses definiert ist


;Hund
;evaluiert ebenfalls zu 'Flocki, da es als wuff definiert
;ist, welches als 'Flocki definiert ist

;Wolf
;evaluiert zu 'wuff, da dieses ein Symbol ist

;(quote Hund)
;gibt 'Hund zurück, da beides synonyme für einander sind

;(eval Hund)
;gibt einen Fehler zurück, da es wie bei dem vorherigen vorgeht,
;aber 'Flocki nicht definiert ist

;(eval Wolf)
;zuerst wird Wolf evaluiert, das auf wuff zeigt,
;worauf dann eval angewendet wird, welches 'Flocki zurück gibt

;>> Ne, es wird vorgezogen evaluiert, also wertet eval 'wuff aus.

;(eval 'Wolf)
;evaluiert das symbol 'Wolf, welches auf 'wuff verweist

;(welcherNameGiltWo 'lily 'potter)
;gibt 'lily zurück, da let PersonC als den ersten Parameter
;der Funktion definiert, welcher bei diesem Aufruf 'lily ist

;>> Unabhängigkeit der Definitionen

;(cdddr xs1)
;gibt die Liste '(wuff Hund) zurück da die cdddr den Rest der Liste nach dem dritten Element zurück gibt

;>> Rest der List vom Rest der Liste vom Rest der Liste ...

;;;(cdr xs2)
;Gibt '(Flocki) zurück, da cdr die Liste ohne das erste Element gibt und Hund zu '(Flocki) evaluiert

;>> Bereits zuvor evaluiert war ...

;(cdr xs3)
;Gibt  'Flocki zurück, da das Element von xs3 auf 'Flocki evaluiert.

;(sqrt 1/4)
;evaluiert die wurzel von 1/4, was bei 1/2 liegt

;(eval '( welcherNameGiltWo 'tiger 'katze))
;gibt 'Wolf zurück, da das symbol '( welcherNameGiltWo 'Wolf 'Hund)) durch eval ausgewertet wird

;(eval (welcherNameGiltWo 'katze 'tiger))
;Zunächst wird welcherNameGiltWo ausgewertet und eval wertet dann das Symbol 'Hund zu 'Flocki aus

;>> 6 Pkt.



; ##############################################################################
; ## Aufgabe 2.1 ###############################################################
; ##############################################################################

(define (fakultaet n)
  ( if(= n 0 )  1            
    ( * n( fakultaet(- n 1 ) ))))

;>> 2 Pkt.

;;; Aufgabe 2.2

(define (power r n)
  ( if
    ( zero? n ) 1           
    ( if ( odd? n )        
      ( * r( power r(-  n  1)) )
      ( sqr ( power r( / n 2 ) ) ))))

;>> 3 Pkt.

;;;Aufgabe 2.3


;; Gibt die Eulerzahl bis auf 1000 Stellen genau zurück

 (define (e)                                #|Funktion e ruft hf mit dem parameter 0 auf. Das Ergebnis wird erst durch 2 geteilt und dann mit 10¹⁰⁰⁰ multipliziert|#
   (*(/ (hf 0) 2) (expt 10 1001)))
                                            #|HIlfsfunktion addiert alle Therme der Reihe ab einem bestimmten n für (n+1)/n! auf.|#
(define(hf n)
  (let ([p (/ (+ n 1) (fakultaet n))])      #|Sei p = (n+1)/n! [unsere eigene Fakultätsfunktion]|#
    (if (>= p (/ 1 (expt 10 1000)))         #|Wenn p noch größer als 10¹⁰⁰⁰ ist...|#
        (+ p (hf (+ n 1)))                  #|...wird zu p h(n + 1) addiert--> rekursiver Aufruf |#
  0)                                        #|...0 addiert (damit bricht die Rekursion ab und "klappert" die restlichen Additionstherme zurück ab) |#
    )
  )

;;Beispielaufruf
    (e)

;>> 6 Pkt.

;;;Aufgabe 2.4 
(define (pi)
 ( *(pi-partial 0 100) 4))



(define (pi-partial step max-steps)
  ( let*
    (
      [ vorzeichen ( if (even? step) 1 -1 ) ]
      [ summand ( * ( / 1 (add1 ( * step 2 ) ) ) vorzeichen ) ]
    )
    ( if
      ( < step max-steps )
      ( +
        summand
        ( pi-partial ( add1 step ) max-steps )
      )
      summand )))

(* (pi) ( power 10 1000 ) )


;>> 1 Pkt.










; ##############################################################################
; ## Aufgabe 3 #################################################################
; ##############################################################################

(define (type-of n)
  (cond
    [(boolean? n) 'Bolean]
    [(list? n) 'List]
    [(pair? n) 'Pair]
    [(symbol? n) 'Symbol]
    [(number? n) 'Number]
    [(char? n) 'Char]
    [(string? n) 'String]
    [(vector? n) 'Vector]
    [(procedure? n) 'Procedure]))

;(type-of (* 2 3 4)) ---> Number, es ist eine Multiplikation
;(type-of (not 42)) ----> Boolean, not ist ein Boolean-Operator
;(type-of '(eins zwei drei))---->'List,esust eine Liste
;(type-of '()) ----> List, es ist eine leere LIste
;(define (id z) z)----->Procedure: Kurz die Identität definieren
;(type-of (id sin))-----> Procedure
;(type-of (string-ref "SE3" 2))------------> char:Das 2. Element (3) aus dem String ist ein Zeichen.
;( type-of ( lambda ( x ) x ) ) ; procedure. Lambda ist eine Funktion. Somit eine Procedure. (Wird nicht definiert. Somit nicht global nutzbar)
;( type-of type-of )     ; procedure. type-of ist eine Funktion. Somit procedure
;( type-of ( type-of type-of ) ) ; String. Wie eben, nur jetzt wird die Rückgabe "procedure" geprüpft, welches ein String ist.

;>> 5 Pkt.

;>> Gesamt: 23 Pkt.
