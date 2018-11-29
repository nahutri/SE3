#lang racket
(require se3-bib/butterfly-module)

#|
  SE3 Funktionale Programmierung
  WiSe 2017/18, Uni Hamburg
  Aufgabenblatt 05
  Eingereicht von
    My Anna Nguyen
    Araf Sadeghi 6893272
    Daniel Djahang 6803168
  Beispielaufrufe befinden sich immer am Ende der Aufgabe
  
|#


; ##########################################################################################
; ## Aufgabe 1.1.1 + 1.1.2 #################################################################
; ##########################################################################################
; Datenstruktur fuer die Merkmale
; die Merkmale sind in einer Liste aufgeführt
; rezessive Merkmale zu einem dominanten Merkmal können dann einfach durch die Reihenfolge der Liste Dominant > rezessiv durch eine Hilfsfunktion selektiert werden
; bzw. analog kann eben durch die Listenposition auch das dominantere Merkmal bestimmt werden
; UPDATE: Bei der Umsetzung wurde mit einem Struct gearbeitet, das heißt auf die Merkmale wurden durch Selektoren zugegriffen
(define color  '(blue green yellow red))
(define pattern '(star dots stripes))
(define feeler '(curved curly straight))
(define wing  '(ellipse rhomb hexagon))

;>> 1.1.1
;>> 4 Pkt.

; Datenstruktur zur Repräsentation eines Schmetterlings
; d kennzeichnet dabei die dominanten sichtbaren Merkmale
; r kennzeichnet dabei die rezessiven unsichtbaren Merkmale
; dabei kann durch die Selektoren auf die verschiedenen Merkmale zugegriffen werden
(struct Butterfly (dColor dPattern dFeeler dWing rColor rPattern rFeeler rWing))

;>> 1.1.2
;>> 4 Pkt.

; ##########################################################################################
; ## Aufgabe 1.1.3 #########################################################################
; ##########################################################################################
#|
Vorschlag:
Das Programm kann so gegliedert werden, dass man zunächst Funktionen zur Verfügung hat, die elementar sind zur Erzeugung eines Kindes, undzwar
die beiden Hilfsfunktionen zur Prüfung der Dominanz (zur Bestimmung der sichtbaren Merkmale eines Kindes)
und der Rezession (für die Konstruktor Funktion, um rezessive Merkmale zu haben).
Dabei ist wichtig zu beachten, dass ein Merkmal über zwei oder mehrere Merkmale dominant sein kann und dadurch nur ein zufälliges ausgewählt werden muss,
dafür wird eine Hilfsfunktion benötigt, die von einer Liste mit mehreren Elementen ein zufälliges Element wählt.
Da es das Ziel ist Schmetterlinge zu erzeugen und es im wesentlichen nur um Schmetterlinge geht, bietet es sich an einen Struct Schmetterling zu definieren, um so
die Merkmale, welche jeweils 4 dominante und 4 rezessive umfassen, schonmal für einen Schmetterling als Basis festzulegen. Durch diesen Struct ist man auch in der Lage
durch einen bereitgestellten Konstruktor Schmetterlinge zu erzeugen. Außerdem hat man durch den Struct, Akzessoren zur Verfügung, durch die man bestimmte Merkmale
abrufen kann. Dabei ist unsere Idee dominante Merkmale von rezessiven Merkmalen zu trennen, und dadurch zwei Akzessoren zu haben, um klar die sichtbaren Merkmale
von den unsichtbaten Merkmalen zu trennen. Weiter eignet sich der Struct gut zur Anzeige von Schmetterlingen, da ihre dominanten Merkmale stets abrufbar sind.
Das Generieren von Kindern erfordert nochmal Hilfsfunktionen, die zum einen zufällige Merkmale der Elternteile aus der rezessiven und dominanten Liste auszuwählen und
durch das Dominanzverfahren, welche als einer der ersten Hilfsfunktionen definiert worden ist, das dominantere Merkmal zu selektieren. Um mehrere Kinder zu erzeugen,
gilt zu beachten, dass jedes mal neue zufällige Merkmale der Eltern gewählt werden sollen und es nicht immer das gleiche Schmetterlingskind sein soll.
|#

; Allgemeine Hilfsfunktion, um immer zufällig etwas auszuwählen
; Wir wählen aus einer Liste ein zufälliges Element 
; @param liste ist die Liste die wir uebergeben
; @return ein zufaelliges Element aus der Liste
(define (aimless liste)
    (let ((len (length liste)))         
      (list-ref liste (random len))
      )
  )

;>> 1.1.3
;>> 4 Pkt.

; ##########################################################################################
; ## Aufgabe 1.2.1 + 1.2.2 #################################################################
; ##########################################################################################
; Diese Funktion ist dazu da zu einem gegeben Merkmal, die dazu rezessiven Merkmale zurückzugeben
; @param attr das Merkmal von dem wir die rezessiven Merkmale wollen
; @return Liste von rezessiven Merkmalen
(define (recessive attr)
  (cond [(equal? attr 'star)  '(star dots stripes) ]  [(equal? attr 'dots)  '(dots stripes)]  [(equal? attr 'stripes) '(stripes)]
        [(equal? attr 'blue)  '(blue green yellow red) ]  [(equal? attr 'green) '(green yellow red) ] [(equal? attr 'yellow)  '(yellow red)]
        [(equal? attr 'red) '(red) ] [(equal? attr 'curved)  '(curved curly straight)] [(equal? attr 'curly)  '(curly straight)]
        [(equal? attr 'straight) '(straight)] [(equal? attr 'ellipse)  '(ellipse rhomb hexagon)] [(equal? attr 'rhomb)  '(rhomb hexagon)]
        [(equal? attr 'hexagon)  '(hexagon)] [else "Invalid Input"] ))

;>> Das ist etwas overkill. Wozu definiert ihr euch die Listen mit den Präzendenzen der Merkmale?
;>> 1 Pkt.

; Diese Funktion bestimmt bei zwei gegebenen Merkmalen das dominantere
; @param attribute ein Merkmal z.B. von einem Elternteil
; @param attribute2 das andere Merkmal vom anderen Elternteil
; @return das dominantere Merkmal
(define (dominant attribute attribute2)
  (cond [(or(equal? attribute 'star) (equal? attribute2 'star)) 'star] [(or(equal? attribute 'dots) (equal? attribute2 'dots)) 'dots] [(equal? attribute 'stripes)  'stripes]
        [(or(equal? attribute 'blue) (equal? attribute2 'blue)) 'blue] [(or(equal? attribute 'green) (equal? attribute2 'green)) 'green] [(or(equal? attribute 'yellow) (equal? attribute2 'yellow)) 'yellow] [(equal? attribute 'red)  'red]
        [(or(equal? attribute 'curved) (equal? attribute2 'curved)) 'curved] [(or(equal? attribute 'curly) (equal? attribute2 'curly)) 'curly] [(equal? attribute 'straight)  'straight]
        [(or(equal? attribute 'ellipse) (equal? attribute2 'ellipse)) 'ellipse] [(or(equal? attribute 'rhomb) (equal? attribute2 'rhomb)) 'rhomb] [(equal? attribute 'hexagon)  'hexagon]
        [else "Invalid Input"])) 

;>> Wie bei 1.2.1
;>> 1 Pkt.

; ##########################################################################################
; ## Aufgabe 1.2.3 - 1.2.5 #################################################################
; ##########################################################################################
; Diese Funktion dient als Konstruktor zum Erzeugen eines Schmetterlings
; @param attr attr2 attr3 attr4 sind die 4 verschiedenen sichtbaren Merkmale eines Schmetterlings
; @return zurueckgegeben wird ein Schmetterling mit den 4 sichtbaren Merkmalen + den zufaellig ausgewählten
; rezessiven Merkmalen
(define (create-Bfly attr attr2 attr3 attr4)
  (let ([myButterfly (Butterfly attr attr2 attr3 attr4 (aimless(recessive attr)) (aimless(recessive attr2)) (aimless(recessive attr3)) (aimless(recessive attr4)))])
   myButterfly
  )
  )

;>> 1.1.3
;>> Streng genommen wird zufällig von den Eltern ein Merkmal (dom/rez) ausgewählt
;>> und von beiden das dominantere kommt zur Ausprägung.
;>> Ist aber ok so.
;>> 6 Pkt.

; Diese Funktion dient als Akzessor, um die sichtbaren Merkmale eines Schmetterlings abzufragen
; @param bfly ein Schmetterling von dem wir die sichtbaren Merkmale wollen
; @return eine Liste von den sichtbaren Merkmalen des Schmetterlings
(define (domAttr bfly)
  (list (Butterfly-dColor bfly)
        (Butterfly-dPattern bfly)
        (Butterfly-dFeeler bfly)
        (Butterfly-dWing bfly)))

; Diese Funktion dient als Akzessor, um die rezessiven Merkmale eines Schmetterlings abzufragen
; @param bfly ein Schmetterling von dem wir die rezessiven Merkmale wollen
; @return eine Liste von den rezessiven Merkmalen des Schmetterlings      
(define (recAttr bfly)
  (list (Butterfly-rColor bfly)
        (Butterfly-rPattern bfly)
        (Butterfly-rFeeler bfly)
        (Butterfly-rWing bfly)))

;>> 1.1.4
;>> 2 Pkt.

; Diese Funktion dient der Anzeige von einem Schmetterling durch seine sichtbaren Merkmale
; @param bfly ein Schmetterling welches wir anzeigen lassen wollen
; @return (display Schmetterling)
(define (displayBfly bfly)   
  (show-butterfly (Butterfly-dColor bfly)
        (Butterfly-dPattern bfly)
        (Butterfly-dFeeler bfly)
        (Butterfly-dWing bfly)
        )
  )

;>> 1.1.5
;>> 2 Pkt.

; Beispielaufrufe:
 (define exampleFly (create-Bfly 'yellow 'star 'curly 'hexagon))
 (domAttr exampleFly)
 (recAttr exampleFly)
 (displayBfly exampleFly)

; ##########################################################################################
; ## Aufgabe 1.2.6 + 1.2.7 #################################################################
; ##########################################################################################
; Hilfsfunktion für randomBfly um zufällige merkmale aus dominant und rezessiv auszuwählen
; @param drlist die Liste mit Paaren von dominanten und rezessiven Merkmalen
; @return eine Liste mit zufällig ausgewählten Merkmalen
(define (randlist drlist)
  (if (empty? drlist)
      '()
      (cons (aimless (car drlist)) (randlist(cdr drlist)))))

; Hilfsfunktion um eine Liste mit zufaelligen Merkmalen zu bilden
; @param bfly ein Schmetterling von dem wir zufällige Merkmale aus dominant und rezessiv wollen
; @return eine Liste mit zufällig ausgewählten Merkmalen
(define (randomBfly bfly)
  (let* ([y
  (map cons (recAttr bfly) '(() () () ()))]
  [list1 (map cons (domAttr bfly) y)])
   (randlist list1)))

; Hilfsfunktion um dominante Merkmale aus zwei Schmetterlingseltern rauszuselektieren
; @param bfly Vater des neu zu erzeugenden Schmetterlings
; @param bfly2 Mutter des neu zu erzeugenden Schmetterlings
; @return Liste mit sichtbaren Merkmalen des Kindes
(define (mergeBfly bfly bfly2)
  (if (empty? bfly2)
  '()
  (cons (dominant (car bfly) (car bfly2)) (mergeBfly (cdr bfly) (cdr bfly2)))))

; Hilfsfunktion zur Erzeugung von Kindern
; @param father der Vater des Schmetterlings
; @param mother die Mutter des Schmetterlings
; @param num die Anzahl der Kinder
; @return Liste mit Kindern
(define (unlimitedKids father mother num)
  (let* (
  [childAttr (mergeBfly (randomBfly father) (randomBfly mother))]
  [childBfly (create-Bfly (car childAttr) (cadr childAttr) (caddr childAttr) (cadddr childAttr))])
  (if (= num 0)
  '()
    (cons(append (domAttr childBfly) (recAttr childBfly)) (cons (displayBfly childBfly) (unlimitedKids father mother (- num 1)))))))

; Diese Funktion erlaubt das Generieren einer Liste von möglichen Kindenr eines Schmetterlings
; Dabei ist in der Liste eine Liste mit den beiden Elternpaaren erhalten
; @param f1 f2 f3 f4 die sichtbaren Merkmale des Vaters
; @param m1 m2 m3 m4 die sichtbaren Merkmale der Mutter
; @param num die Anzahl der Kinder
; @return eine Liste mit der Liste der beiden Elternpaare + die ganzen Kinder
 (define (butterchild f1 f2 f3 f4 m1 m2 m3 m4 num)
 (let* ([father (create-Bfly f1 f2 f3 f4)]
        [mother (create-Bfly m1 m2 m3 m4)])        
        
      (cons (list (displayBfly father) (displayBfly mother)) (unlimitedKids father mother num))))
; Beispielaufrufe:

 (butterchild 'green 'stripes 'curved 'rhomb 'blue 'star 'curly 'ellipse 4)

;>> 1.1.6
;>> 8 Pkt.
;>> 1.1.7
;>> 3 Pkt.

; ##############################################################################
; ## Aufgabe 2 #################################################################
; ##############################################################################
; Diese Funktion ist dazu da zu testen, ob ein Schmetterling das Kind eines bestimmten Elternpaares sein kann
; Dabei ist zu beachten, dass bei den rezessiven Merkmale alle rezessiven Merkmale betrachtet werden muessen, anstatt nur eines zufälliges
; Daher werden die Eltern durch einen Sonderkonstruktor create-Bfly2 erstellt und die rezessiven Merkmale durch einen Sondercompare mit den
; dominanten Merkmalen des Kindes überprüft.
; @param f1 f2 f3 f4 sichtbare Merkmale des Vaters des vermeindlichen Kindes
; @param m1 m2 m3 m4 sichtbare Merkmale der Mutter des vermeintlichen Kindes
; @param c1 c2 c3 c4 sichtbare Merkmale des vermeindlichen Kindes
; @return liste mit booleschen Werten die Aufschluß darüber geben, inwiefern das Kind von den vermeindlichen Eltern abstammen könnte
; z.B. '(#t,#t,#f,#t) würde heißen, dass das Kind aufgrund von farbe, muster und flügel ein Kind sein könnte, es jedoch nicht ist, weil die
; fühler nicht von den vermeindlichen Eltern abstammen könnten
(define (parentTest f1 f2 f3 f4 m1 m2 m3 m4 c1 c2 c3 c4)
  
  (let* ([father (create-Bfly2 f1 f2 f3 f4)]
        [mother (create-Bfly2 m1 m2 m3 m4)]
        [child (create-Bfly c1 c2 c3 c4)]               
        )
    
    (compareAttr2(compareAttr2 (compareAttr (domAttr child) (domAttr father)) (compareAttr (domAttr child) (domAttr mother)))
                  (compareAttr2 (compareAttr3 (domAttr child) (recAttr father)) (compareAttr3 (domAttr child) (recAttr mother))))
    ))

; Hilfsfunktion um die Eltern gesondert zu erstellen, da mehrere rezessive Merkmale betrachtet werden müssen
; @param attr attr2 attr3 attr4 die dominanten Merkmale des Elternteils
; @return möglicher Vater bzw. mögliche Mutter
(define (create-Bfly2 attr attr2 attr3 attr4)
  (let ([myButterfly (Butterfly attr attr2 attr3 attr4 (recessive attr) (recessive attr2) (recessive attr3) (recessive attr4))])
   myButterfly
  ))
; Hilfsfunktion um die die dominanten Merkmale auf Gleichheit zu testn
; @param a Liste mit dominanten Merkmalen des Kindes
; @param b Liste mit dominanten Merkmalen des Elternteils
; @return Liste mit booleschen Werten
(define (compareAttr a b)
  (if (empty? a)
      '()
      (cons (equal? (car a) (car b)) (compareAttr (cdr a) (cdr b)))))

; Hilfsfunktion um die Listen aus compareAttr zu verodern
; @param a Liste mit booleschen Werten
; @param b Liste mit booleschen Werten
; @return Liste mit booleschen Werten
(define (compareAttr2 a b)
  (if (empty? a)
      '()
      (cons (or (car a) (car b)) (compareAttr2 (cdr a) (cdr b)))))   

; Hilfsfunktion um die die rezessiven Merkmale auf Gleichheit zu prüfen
; @param a Liste der dominanten Merkmale des Kindes
; @param b Liste der rezessiven Merkmale des Elternteils
; @return Liste mit booleschen Werten
(define (compareAttr3 a b)
  (cond [(empty? a) '()]
  [(= (length (car b)) 2) (cons(or (equal? (car a)(caar b)) (equal? (car a) (cadar b))) (compareAttr3 (cdr a) (cdr b))) ]
  [(= (length (car b)) 3) (cons(or (equal? (car a)(caar b)) (equal? (car a) (cadar b)) (equal? (car a) (caddar b))) (compareAttr3 (cdr a) (cdr b)))]
      [else (cons (equal? (car a) (caar b)) (compareAttr3 (cdr a) (cdr b)))]))

; Beispielaufrufe:
  (parentTest 'red 'stripes 'curved 'hexagon 'yellow 'star 'curly 'rhomb 'blue 'star 'curved 'hexagon)
  (parentTest 'red 'stripes 'curved 'hexagon 'yellow 'star 'curly 'rhomb 'green 'dots 'straight 'rhomb)
  (parentTest 'red 'stripes 'curved 'hexagon 'yellow 'star 'curly 'rhomb 'yellow 'stripes 'curly 'ellipse)
  (parentTest 'red 'stripes 'curved 'hexagon 'yellow 'star 'curly 'rhomb 'yellow 'stripes 'curly 'rhomb)

;>> 6 Pkt. 

;>> Gesamt: 41 Pkt.
