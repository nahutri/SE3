#lang racket


#|
  SE3 Funktionale Programmierung
  WiSe 2017/18, Uni Hamburg
  Aufgabenblatt 04
  Eingereicht von
    My Anna Nguyen
    Araf Sadeghi 6893272
    Daniel Djahang 6803168
  Beispielaufrufe befinden sich immer am Ende der Aufgabe
  und können einfach auskommentiert werden.
|#

; ##############################################################################
; ## Aufgabe 1 #################################################################
; ##############################################################################
(max (min 5 (- 8 7)) 6)
'(+ ,(- 13 11) 17) 
(cadr '(Good King Wenceslas))
(cdddr '(looked out (On the feast of Steven)))
(cons 'When '(the snow lay round about))
(cons '(Deep and) 'crisp)
(equal? (list 'and 'even) '(and even))
(eq? (list 'Rudolph ' the ' red-nosed ' reindeer ) (cons 'Rudolph '( the ' red-nosed ' reindeer)) )

#| 
1.(max (min 5 (- 8 7)) 6) evaluiert zu 6, weil das Maximum 6 ergibt

2.'(+ ,(- 13 11) 17) evaluiert zu '(+ ,(- 13 11) 17), weil ' (quote) die Evaluierung blockiert

;>> Es ist ein quasiquote gemeint, wo ihr ein einfaches quote lest (dafür die Fußmarke).
;>> In dem Fall ist das Verhalten anders.

3.(cadr '(Good King Wenceslas)) evaluiert zu 'King, weil cadr das erste Element der Restliste einer Liste bestimmt
4.(cddr '(kommt (das Weihnachtfest))) evaluiert zu '(), weil cddr die Restliste der Restliste bestimmt und die 
Restliste der ursprünglichen Liste aus nur einem Element bestimmt und die Restliste einer ein-elementigen Liste 
die leere Liste ist
5.(cdddr '(looked out (On the feast of Steven))) evaluiert zu leere Liste, sucht die dritte Verschachtelung.

;>> Das ist zu ungenau: es wird die Restliste von der Restliste von der Restliste berechnet.

6.(cons 'When '(the snow lay round about)) evaluiert zu '(When the snow lay round about), weil Paare deren zweites Element eine
Liste ist, in der vereinfachten Listennotaion dargestellt werden und durch Anwendung von cons ensteht ja ein Paar.
7.'((Deep and) . crisp)evaluiert zu '((Deep and) . crisp). Cons fügt am Anfang einer Liste ein Element an. Der dot-Operator
 ist eine Infix-Operation für den cons-Operator.
8.(equal? (list 'and 'even) '(and even)) evaluiert zu #t (true), weil die durch list erzeugte
Liste '('and 'even)und die durch die Angabe der einzelnen Element in Klammern mit dem quote erzeugte Liste
die gleichen Elemente enthalten (bzw. die Elemente den gleichen Wert haben), also eqv? auf diese Listen angewendet true 
liefern würde.


9.(eq? (list 'Rudolph ' the ' red-nosed ' reindeer ) (cons 'Rudolph '( the ' red-nosed ' reindeer)) ) evaluiert zu #f (false), da eq? testet ob es sich
bei beiden Argumenten um identische Objekte handelt, was bei diesen beiden Listen nicht der Fall ist. equal? oder eqv?
hätten bei diesen beiden Listen true geliefert, weil diese Listen die gleichen Elemente enthalten bzw. die Elemente 
den gleichen Wert haben.
|#

;>> Nein, sie sind auch nicht eqv? oder equal?, da die Elemente der Listen nicht äquivalent sind.

;>> 3 Pkt.

; ##############################################################################
; ## Aufgabe 2.1 ###############################################################
; ##############################################################################

#|<Notruf>::= <Überschrift> <Standort> <Meldung> <WeiterAngaben> <Signale> <Unterschrift> <Over>
  <Überschrift>::= MAYDAY MAYDAY MAYDAY
                   DELTA ECHO


;>> Da habt ihr die Aufgabe etwas vereinfacht, indem ihr nur DELTA ECHO ableitet.

                   <Wort> <Wort> <Wort> <WortB>
                   MAYDAY <Wort> ICH BUCHSTABIERE <WortB>

  <Standort>::= <Satz>
  <Meldung>::= <Satz>
  <WeiterAngaben>::= <Satz>
  <Signale>::= ICH SENDE DEN TRÄGER --
  <Unterschrift>::= MAYDAY <Wort> <WortB> <WortB>
  <Satz>::= <Wort>|<Wort> '' <Satz>
  <Wort>::= <Buchstabe> <Wort>|lambda

;>> Wo kommt das "lambda" her?
  <Wort>::= <Buchstabe> <Wort>| <Wort>

  <Buchstabe>::= A|B|C...|Z
  <WortB>::= <Buchstabiertafel> <WortB>|lambda
  <Buchstabiertafel>::= Alpha|Bravo|Charlie|...
  <Over>::= OVER
|#

;>> 2 Pkt.

; ##############################################################################
; ## Funktion aus dem letztem Blatt ############################################
; ##############################################################################

(define buchstabiertafel
  '( (#\A "ALPHA")
     (#\B "BRAVO")
     (#\C "CHARLIE")
     (#\D "DELTA")
     (#\E "ECHO")
     (#\F "FOXTROT")
     (#\G "GOLF")
     (#\H "HOTEL")
     (#\I "INDIA")
     (#\J "JULIET")
     (#\K "KILO")
     (#\L "LIMA")
     (#\M "MIKE")
     (#\N "NOVEMBER")
     (#\O "OSCAR")
     (#\P "PAPA")
     (#\Q "QUEBEC")
     (#\R "ROMEO")
     (#\S "SIERRA")
     (#\T "TANGO")
     (#\U "UNIFORM")
     (#\V "VIKTOR")
     (#\W "WHISKEY")
     (#\X "X-RAY")
     (#\Y "YANKEE")
     (#\Z "ZULU")
     (#\0 "NADAZERO")
     (#\1 "UNAONE")
     (#\2 "DUOTWO")
     (#\3 "TERRATHREE")
     (#\4 "CARREFOUR")
     (#\5 "PENTAFIVE")
     (#\6 "SOXISIX")
     (#\7 "SETTESEVEN")
     (#\8 "OKTOEIGHT")
     (#\9 "NOVONINE")
     (#\, "DECIMAL")
     (#\. "STOP")))



(define (btokey b)
  (
   cdr (assoc b buchstabiertafel)))

  



(define (string->board strInput)

  (if (string=? strInput "")
      '()
      (cons (car(btokey (car(string->list strInput)))) (string->board(list->string (cdr(string->list strInput))))))
  )


; ##############################################################################
; ## Aufgabe 2.2 ###############################################################
; ##############################################################################


(define(notmeldung name zeichen pos zeit art angaben)
(display(string-append
    ( ueberschrift name zeichen )
    ( standortangabe pos)
    ( zeitangabe zeit)
    ( artangabe art)
    ( extraangabe angaben)
    ( peilzeichen )
    ( unterschrift name zeichen )
    ( over )
  ))
 )

;>> z.B. zeitangabe kommt in eurer Grammatik gar nicht vor.

(define(ueberschrift schiffsname rufzeichen)

(string-append
   "MAYDAY MAYDAY MAYDAY\nDELTA ECHO\n" 
   schiffsname " " schiffsname " " schiffsname " " (string-join (string->board rufzeichen) " " )
   "\nMAYDAY " schiffsname " ICH BUCHSTABIERE " (string-join (string->board schiffsname) " " ) "\nRUFZEICHEN " (string-join (string->board rufzeichen) " " ))
  )

  
(define(standortangabe position)

(string-append
   "\nNOTFALLPOSITION " 
   position
  )

  )
(define(zeitangabe zeit)

(string-append
   "\nNOTFALLZEIT " 
   zeit
  )

  )
(define(artangabe art)

(string-append "\n" art)

  )
(define( extraangabe weitereAngaben)

  (string-append "\n" weitereAngaben)

  )
(define( peilzeichen)
  "\nSENDE DEN TRÄGER --\n"
  )
(define( unterschrift schiffsname rufzeichen)

(string-append schiffsname " " (string-join (string->board rufzeichen) " " ))
  

  )
(define(over)

  "\nOVER"

  )

;>> Keine Kommentare, fürchterliche Code-Formatierung.

;>> 5 Pkt.


; ##############################################################################
; Aufgaben 2.3  ################################################################
; ##############################################################################

;KOMMENTAR: 3 Pkt
;>> War das auch in der kopierten Lösung enthalten?

; Aufruf für Babette
( notmeldung "BABETTE"
             "DEJY"
             "UNGEFAEHR 10 SM NORDOESTLICH LEUCHTTURM KIEL"
             "1000 UTC"
             "SCHWERER WASSEREINBRUCH WIR SINKEN
             \nKEINE VERLETZTEN
             \nVIER MANN GEHEN IN DIE RETTUNGSINSEL
             \nSCHNELLE HILFE ERFORDERLICH"
             "ICH SENDE DEN TRAEGER"
             )


(display "\n\n")

; Aufruf für Amira
( notmeldung "AMIRA"
             "AMRY"
             "UNGEFAEHR AUF DER HOEHE 53°56'N UND DER BREITE 006°31'E"
             "1640 UTC"
             "NACH KENTERGANG AM SINKEN"
             "15 MANN AN BORD
               \nDAS SCHIFF IS 15M LANG
               \nROTER RUMPF"
             )


; ##############################################################################
; Aufgaben 3  ################################################################
; ##############################################################################


; 3. Funktionen vs Spezialformen
;; Innere und aeussere Reduktion
#|
Innere und aeussere Reduktion beschreiben das Verfahren wie Funktionen
ausgewertet werden. Bei der inneren Reduktion wird die Funktion und 
ihre Argumente von innen nach außen ausgewertet. Also zunaechst die
innersten Terme (etwa die Argumente der Funktion) und auf diese Werte
wird dann die jeweils naechste Ebene der Funktion angewandt.
Die aeussere Reduktion wertet eine Funktion von aussen nach innen aus. 
Bei einer Funktion wird z.B. zuerst ihr Funkionsrumpf ausgewertet und
erst dann nacheinander die Argumente
|#

(define (hoch3 x) (* x x x))
(display "\nAUFGABE 3: hoch3\n----------------\n")
(hoch3 (* 3 (+ 1 (hoch3 2))))

;; innere Reduktion: 
;; (hoch3 (* 3 (+ 1 (hoch3 2))))
;; (hoch3 (* 3 (+ 1 (* 2 2 2))))
;; (hoch3 (* 3 (+ 1 8)))
;; (hoch3 (* 3 9))
;; (hoch3 27)
;; (* 27 27 27)
;; 19683

;; aeussere Reduktion
;; (hoch3 (* 3 (+ 1 (hoch3 2))))
;; (* (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2))))
;; (* 27 (* 3 (+ 1 (hoch3 2))) (* 3 (+ 1 (hoch3 2)))
;; (* 27 27 (* 3 (+ 1 (hoch3 2)))
;; (* 27 27 27)
;; 19683
;;;;;um den linken Ausdruck (* 3 (+ 1 (hoch3 2))) weiter reduzieren zu können (3 * term kann so nicht ausgewertet werden)
;;;;;werden dann die nötigen inneren Terme reduziert
;;;;;also eigentlich nicht ein Schritt von (* 3 (+ 1 (hoch3 2))) zu 27

;; Reduktionsstrategie in Racket
;; Racket verwendet im Allgemeinen die innere Reduktion. Fuer special form expressions
;; wie define, if, etc. werden abhaengig vom Ausdruck spezielle Auswertungformen benutzt.
;; Bei einem if wird zum Beispiel zunaechst die Bedingung ausgewertet und dann entsprechend
;; der if oder else Zweig

;; new-if
(define (new-if condition? then-clause else-clause)
  (cond (condition? then-clause)
        (else else-clause)))


(define (faculty product counter max-count)
  (new-if (> counter max-count)
          product
          (faculty (* counter product)
                   (+ counter 1)
                   max-count)))

;; Wenn (faculty 1 1 5) aufgerufen wird, entsteht ein Speicherueberlauf. Der Ueberlauf
;; kommt zustande, da die new-if Funktion zwar durchaus entsprechend der condition?
;; korrekt verzweigt, aber da new-if eine normale Funktion ist verwendet Racket hier
;; inner Reduktion so dass bevor new-if ausgewertet wird, zunaechst die drei Argumente
;; ausgewertet werden  und eine endlos Rekursion durch den faculty-Aufruf in der else-clause
;; entsteht.
;; Mit dem special form if passiert diese Ueberlauf nicht, da zunaechst die gegebene Bedingung
;; geprueft wird bevor der if- oder else-Zweig durchlaufen wird. Fuer eine korrekte Rekursion 
;; ist eine Auswertung durch inner Reduktion nicht geeignet.

;>> Gesamt: 13 Pkt.
