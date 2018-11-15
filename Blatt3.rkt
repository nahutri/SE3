#lang racket
( require se3-bib/flaggen-module )
( require racket/gui/base )
#|
  SE3 Funktionale Programmierung
  WiSe 2017/18, Uni Hamburg
  Aufgabenblatt 03
  Eingereicht von
    My Anna Nguyen
    Araf Sadeghi 6893272
    Daniel Djahang 6803168
  
|#

; ##############################################################################
; ## Aufgabe 1.1 #################################################################
; ##############################################################################

;; Liste aus Paaren von char -> string
;; Wir wählen eine Liste von Paaren, weil wir mit assoc das erste Paar finden können, dessen erstes Element
;; gleich dem Schlüssel ist. Wir können also mit Hilfe eines Schlüssels das korrespondierende Element erhalten.


(define buchstabiertafel
  '( (#\A "Alpha")
     (#\B "Bravo")
     (#\C "Charlie")
     (#\D "Delta")
     (#\E "Echo")
     (#\F "Foxtrot")
     (#\G "Golf")
     (#\H "Hotel")
     (#\I "India")
     (#\J "Juliet")
     (#\K "Kilo")
     (#\L "Lima")
     (#\M "Mike")
     (#\N "November")
     (#\O "Oscar")
     (#\P "Papa")
     (#\Q "Quebec")
     (#\R "Romeo")
     (#\S "Sierra")
     (#\T "Tango")
     (#\U "Uniform")
     (#\V "Viktor")
     (#\W "Whiskey")
     (#\X "X-ray")
     (#\Y "Yankee")
     (#\Z "Zulu")
     (#\0 "Nadazero")
     (#\1 "Unaone")
     (#\2 "Duotwo")
     (#\3 "Terrathree")
     (#\4 "Carrefour")
     (#\5 "Pentafive")
     (#\6 "Soxisix")
     (#\7 "Setteseven")
     (#\8 "Oktoeight")
     (#\9 "Novonine")
     (#\, "Decimal")
     (#\. "Stop")))

;>> 4 Pkt.


; ##############################################################################
; ## Aufgabe 1.2 #################################################################
; ##############################################################################

;; b ist der Buchstabe, dessen Schlüssel man wissen möchte
;; Suche nach der ersten Übereinstimmung mit dem ersten Element der Paare
;; Eine Funktion, die Buchstaben mittels der Buchstabiertabelle auf ihre Schlüssel abbildet
(define (btokey b)
  (
  cdr (assoc b buchstabiertafel)))

;>> Ein bisschen unschön ist, dass eure Funktione eine einelementige Liste zurückgibt und nicht den gesuchten Wert.
;>> 2 Pkt.
  
; ##############################################################################
; ## Aufgabe 1.3 #################################################################
; ##############################################################################

;; strInput ist die String Eingabe die umzuwandeln ist
;; wenn der String leer ist wird die Rekursion abgebrochen
;; ansonsten wird der String in chars zerlegt und jeweils für den char der Schlüssel rausgesucht
;; vom input wird jeweils immer das erste element entfernt und rekursiv appended 
(define (string->board strInput)

  (if (string=? strInput "")
    '()
  (append(btokey (car(string->list strInput))) (string->board(list->string (cdr(string->list strInput))))))
  )

;>> Es ist effizienter, wenn ihr cons statt append verwendet und nicht in keder Iteration vorwärts und rückwärts konvertiert.
;>> 6 Pkt.

; ##############################################################################
; ## Aufgabe 2.1 #################################################################
; ##############################################################################

;; Liste aus Paaren von char -> flag
;; Bei dieser Funktion ist zu beachten, dass zur Listenbildung anstatt '(), (list ..) zu benutzen ist, da die Symbole zu den Flaggen ertmal evaluieren müssen
;; Wir wählen eine Liste von Paaren, weil wir mit assoc das erste Paar finden können, dessen erstes Element
;; gleich dem Schlüssel ist. Wir können also mit Hilfe eines Schlüssels das korrespondierende Element erhalten.

(define flaggentafel
  (list
     (list #\A A)
     (list #\B B)
     (list #\C C)
     (list #\D D)
     (list #\E E)
     (list #\F F)
     (list #\G G)
     (list #\H H)
     (list #\I I)
     (list #\J J)
     (list #\K K)
     (list #\L L)
     (list #\M M)
     (list #\N N)
     (list #\O O)
     (list #\P P)
     (list #\Q Q)
     (list #\R R)
     (list #\S S)
     (list #\T T)
     (list #\U U)
     (list #\V V)
     (list #\W W)
     (list #\X X)
     (list #\Y Y)
     (list #\Z Z)
     (list #\0 Z0)
     (list #\1 Z1)
     (list #\2 Z2)
     (list #\3 Z3)
     (list #\4 Z4)
     (list #\5 Z5)
     (list #\6 Z6)
     (list #\7 Z7)
     (list #\8 Z8)
     (list #\9 Z9)))

;>> 3 Pkt.


; ##############################################################################
; ## Aufgabe 2.2 #################################################################
; ##############################################################################

;; b ist der Buchstabe, dessen Schlüssel man wissen möchte
;; Suche nach der ersten Übereinstimmung mit dem ersten Element der Paare
;; Eine Funktion, die Buchstaben mittels der Flaggentabelle auf ihre Schlüssel abbildet
(define (btoflag b)
  (
   cdr (assoc b flaggentafel)))

;>> Jetzt wäre aber cadr angebrachter, da ihr die Flagge und nicht eine Liste mit der Flagge haben wollt.

; ##############################################################################
; ## Aufgabe 2.3 #################################################################
; ##############################################################################

;; strInput ist die String Eingabe die umzuwandeln ist
;; wenn der String leer ist wird die Rekursion abgebrochen
;; ansonsten wird der String in chars zerlegt und jeweils für den char der Schlüssel rausgesucht
;; vom input wird jeweils immer das erste element entfernt und rekursiv appended 
(define (string->flag strInput)

  (if (string=? strInput "")
    '()
  (append(btoflag (car(string->list strInput))) (string->flag(list->string (cdr(string->list strInput))))))
  )

;>> Siehe Aufgabe 1.3
;>> 4 Pkt.

; ##############################################################################
; ## Aufgabe 3.1 #################################################################
; ##############################################################################

;; Liste aus Paaren von char -> morsecode
;; Wir wählen eine Liste von Paaren, weil wir mit assoc das erste Paar finden können, dessen erstes Element
;; gleich dem Schlüssel ist. Wir können also mit Hilfe eines Schlüssels das korrespondierende Element erhalten. 
(define morsetafel
  '(
    (#\A (play-sound "Morse-A.wav" #f)) 
    (#\B (play-sound "Morse-B.wav" #f))
    (#\C (play-sound "Morse-C.wav" #f))
    (#\D (play-sound "Morse-D.wav" #f))
    (#\E (play-sound "Morse-E.wav" #f))
    (#\F (play-sound "Morse-F.wav" #f))
    (#\G (play-sound "Morse-G.wav" #f))
    (#\H (play-sound "Morse-H.wav" #f))
    (#\I (play-sound "Morse-I.wav" #f))
    (#\J (play-sound "Morse-J.wav" #f))
    (#\K (play-sound "Morse-K.wav" #f))
    (#\L (play-sound "Morse-L.wav" #f))
    (#\M (play-sound "Morse-M.wav" #f))
    (#\N (play-sound "Morse-N.wav" #f))
    (#\O (play-sound "Morse-O.wav" #f))
    (#\P (play-sound "Morse-P.wav" #f))
    (#\Q (play-sound "Morse-Q.wav" #f))
    (#\R (play-sound "Morse-R.wav" #f))
    (#\S (play-sound "Morse-S.wav" #f))
    (#\T (play-sound "Morse-T.wav" #f))
    (#\U (play-sound "Morse-U.wav" #f))
    (#\V (play-sound "Morse-V.wav" #f))
    (#\W (play-sound "Morse-W.wav" #f))
    (#\X (play-sound "Morse-X.wav" #f))
    (#\Y (play-sound "Morse-Y.wav" #f))
    (#\Z (play-sound "Morse-Z.wav" #f)  )))

  
; ##############################################################################
; ## Aufgabe 3.2 #################################################################
; ##############################################################################
;; b ist der Buchstabe, dessen Schlüssel wir wissen moechten
;; durch assoc finden wir unser Buchstaben-Morsecode-Paar und wählen durch list-ref den morseCode aus,
;; welchen wir durch eval zum ausführen bringen, da eval komplementär zu ' ist
(define (btomorse b)
  
    (eval(list-ref (assoc b morsetafel) 1)))


; ##############################################################################
; ## Aufgabe 3.3 #################################################################
; ##############################################################################
;; strInput ist die String Eingabe
;; Rekursion führt die MorseCodes aus 
;; Schleifenabbruch durch leeren String mit #t als Rückgabewert
(define (string->morse strInput)

  (if (string=? strInput "")
    #t
  (and (btomorse (car(string->list strInput))) (string->morse(list->string (cdr(string->list strInput)))))))

;>> Ein Aufrufbeispiel fehlt mir.
;>> Aufgabe 3
;>> 9 Pkt.

;>> Gesamt: 28 Pkt.
