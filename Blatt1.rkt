#lang racket

#|
  SE3 Funktionale Programmierung
  WiSe 2017/18, Uni Hamburg
  Aufgabenblatt 01
  Eingereicht von
    My Anna Nguyen
    Araf Sadeghi 6893272
    Daniel Djahang 6803168
  Beispielaufrufe befinden sich immer am Ende der Aufgabe
  und können einfach auskommentiert werden.
|#

; ##############################################################################
; ## Aufgabe 1.1 ###############################################################
; ##############################################################################

;Umrechnung von Grad ins Bogenmaß
(define (degrees->radians deg)
  (* deg ( / pi 180)))

;Umrechnung vom Bogenmaß zu Grad
(define (radians->degrees rad)
  (* rad ( / 180 pi ) ))

;>> 4 Pkt.

;Aufgabe 1.2

;; Berechnet den acos von einem cos-Wert
(define (my-acos alpha)(- (/ pi 2)
                          (* 2 (atan(/ alpha (+ 1 (sqrt(- 1 (* alpha alpha)))))))
                          ))

;>> 3 Pkt.

;Aufgabe 1.3

;Umrechnung von Nautischen Meilen zu Kilometern
(define (nm->km nm)
  (* nm 1.852))

;>> 1 Pkt.


; ##############################################################################
; ## Aufgabe 2.1 ###############################################################
; ##############################################################################


;; Berechnet die Entfernung zweier Punkte auf der Erde in Kilometer


(define (grosskreisentfernung latA lonA latB lonB)
  (my-acos (+
             (*
               (sin (degrees->radians latA))
               (sin (degrees->radians latB)))
             (*
               (cos (degrees->radians latA))
               (cos (degrees->radians latB))
               (cos (degrees->radians (- lonA lonB)))))))

(define (distanzAB latA lonA latB lonB)
  (nm->km
    (* 60 (radians->degrees (grosskreisentfernung latA lonA latB lonB)))))



; Beispielaufrufe:
 ( distanzAB  59.93   10.75  22.20  114.10 )  ; Oslo - Honkong
 ( distanzAB  37.75 -122.45  21.32 -157.83 )  ; San Francisco - Honolulu
 ( distanzAB -27.10 -109.40 -12.10  -77.05 )  ; Osterinseln - Lima


;>> 11 Pkt.

;Aufgabe 2.2

; Richtung des Kurses zwischen A und B mit Parametern
; Start-Breitengrad Start-Längengrad Ziel-Breitengrad Ziel-Längengrad
(define (Anfangskurs Aφ Aλ Bφ Bλ)
  (correctedcourse Aλ Bλ
                   (radians->degrees
                    (my-acos
                     (/ (- (sin (degrees->radians Bφ))
                           (* (cos (dG Aφ Aλ Bφ Bλ))
                              (sin (degrees->radians Aφ)))
                           )
                        (* (cos (degrees->radians Aφ))
                           (sin (dG Aφ Aλ Bφ Bλ)))
                       )))))

; Hilfsfunktion1 für 2.2
(define (goingEast? Aλ Bλ)
  (cond[(<= Aλ Bλ) #t]
       [else #f]
       )
  )
;Hilfsfunktion für 2.2
(define (correctedcourse Aλ Bλ alpha)
  (cond[(goingEast? Aλ Bλ) alpha]
       [else (- 360 alpha)]
       )
  )

; Hilfsfunktion für 2.2

;zur Berechnung des Winkels dG
; Parameter in Grad
(define (dG ab al bb bl)
  (my-acos
   (+ (* (sin (degrees->radians ab))
         (sin (degrees->radians bb))
         )
      (* (cos (degrees->radians ab))
         (cos (degrees->radians bb))
         (cos (degrees->radians
               (- bl al)))))))

;>> 4 Pkt.


;;; Aufgabe 2.3 Himmelsrichtungen
(define (grad->himmelsrichtung winkel)
  (cond [(=  winkel 0) "Nord"]
        [(and(> winkel 0) (< winkel 45)) "Nord Nordost"]
        [(= winkel 45) "Nordost"]
        [(and(> winkel 45) (< winkel 90)) "Ost Nordost"]
        [(= winkel 90) "Ost"]
        [(and(> winkel 90) (< winkel 135)) "Ost Südost"]
        [(= winkel 135) "Südost"]
        [(and(> winkel 135) (< winkel 180)) "Süd Südost"]
        [(= winkel 180) "Süd"]
        [(and(> winkel 180) (< winkel 225)) "Süd Südwest"]
        [(= winkel 225) "Südwest"]
        [(and(> winkel 225) (< winkel 270)) "West Südwest"]
        [(= winkel 270) "West"]
        [(and(> winkel 270) (< winkel 315)) "West Nordwest"]
        [(= winkel 315) "Nordwest"]
        [(and(> winkel 315) (< winkel 360)) "Nord Nordwest"]
        [(= winkel 360) "Nord"]
        [else "nur 0 bis 360 Grad"])

  )

(define (himmelsrichtung->grad kurs)
  (cond [(equal? kurs "N") "0 bzw. 360 Grad"]
        [(equal? kurs "NNO") "22.5 Grad"]
        [(equal? kurs "NO") "45 Grad"]
        [(equal? kurs "ONO") "67.5 Grad"]
        [(equal? kurs "O") "90 Grad"]
        [(equal? kurs "OSO") "112.5 Grad"]
        [(equal? kurs "SO") "135 Grad"]
        [(equal? kurs "SSO") "157.5 Grad"]
        [(equal? kurs "S") "180 Grad"]
        [(equal? kurs "SSW") "202.5 Grad"]
        [(equal? kurs "SW") "225 Grad"]
        [(equal? kurs "WSW") "247.5 Grad"]
        [(equal? kurs "W") "270 Grad"]
        [(equal? kurs "WNW") "292.5 Grad"]
        [(equal? kurs "NW") "315 Grad"]
        [(equal? kurs "NNW") "337.5 Grad"]
        [else "Keine gueltige Himmelsrichtung"]
        )
  )

;>> Optimaler Weise arbeitet ihr in Racket mit Symbolen, statt mit Stringvergleich
;>> 4 Pkt.

;>> Gesamt: 27 Pkt.

(require test-engine/racket-tests)

;1.1
(check-within (degrees->radians 180) 3.141592653589793 0.0)
(check-within (degrees->radians 360) 6.283185307179586 0.0)
(check-within (degrees->radians 90) 1.5707963267948966 0.0)

;(radians->degrees)
(check-within (radians->degrees pi) 180.0 0.0)
(check-within (radians->degrees (* pi 2)) 360.0 0.0)
(check-within (radians->degrees (/ pi 2)) 90.0 0.0)

;1.2
(check-within (my-acos (cos 1)) 1 0.001)
;(check-expect (my-acos 0) 0)

;1.3
(check-within (nm->km 1) 1.852 0.0)

;2.1
(check-within (distanzAB 59.93 10.75 22.20 114.10) 8589.412217586058 0.1) ;Oslo -> Hongkong
(check-within (distanzAB 37.75 122.45 21.32 157.83) 3844.6880504870555 0.1) ;San Francisco -> Honolulu
(check-within (distanzAB 27.10 109.40 12.10 77.05) 3757.622218810056 0.1) ;Osterinsel -> Lima
(check-within (distanzAB 41.1578 -8.6333 0.6722 -61.5333) 6944.307574004419 0.1) ;Porto -> Port of Spain

;2.2
(check-within (Anfangskurs 59.93 10.75 22.20 114.10) 67.43567317710463 0.1) ;Oslo -> Hongkong
(check-within (Anfangskurs 37.75 -122.45 21.32 -157.83) 251.78216746132105 0.1) ;San Francisco -> Honolulu
(check-within (Anfangskurs -27.10 -109.40 -12.10 -77.05) 70.07186231064308 0.1) ;Osterinsel -> Lima
(check-within (Anfangskurs 41.1578 -8.6333 0.6722 -61.5333) 244.04927957054304 0.1) ;Porto -> Port of Spain

;2.3
(check-expect (grad->himmelsrichtung 267) "W")
(check-expect (grad->himmelsrichtung 1) "N")

;Himmelsrichtung->Grad
(check-expect (himmelsrichtung->grad "N") 0)
(check-within (himmelsrichtung->grad "NNE") 22.5 0.0)

(test)
