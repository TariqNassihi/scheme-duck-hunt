


; pixels 

(define pixels-horizontaal 900)

(define pixels-vertikaal 700)


(define cel-breedte-px 100)
(define cel-hoogte-px 100)

(define spel-breedte 9)
(define spel-hoogte 7)

(define y-min -500)
(define y-max 500)

(define venster-breedte-px (* cel-breedte-px spel-breedte))
(define venster-hoogte-px (* cel-hoogte-px spel-hoogte))

(define pixels-centrum-vogel 50)
(define pixels-centrum-crosshair 25)
(define pixels-centrum-water 50)
(define pixels-centrum-net-y 0)
(define pixels-centrum-net-x 70)

(define levens-tile-x 166)
(define levens-tile-y 600)

(define levens-text "Levens:    ")
(define levens-x 90)

(define text-hoogte 20)
(define text-x 0)
(define text-y 0)
(define text-kleur "white")
(define text-kleur2 "darkred")
(define text-kleur3 "green")

(define score-text "Score: ")
(define score-x 80)


(define herhaaltijd-tile-x 400)
(define herhaaltijd-tile-y 600)

(define kogel-tile-x 350)
(define kogel-tile-y 600)

(define pw-tile-x 500)
(define pw-tile-y 570)

(define score-tile-x 700)
(define score-tile-y 600)

(define level-tile-x 700)
(define level-tile-y 635)
(define level-text "Level: ")

(define ronde-tile-x 700)
(define ronde-tile-y 670)
(define ronde-text "Ronde: ")

(define luchtdruk-tile-x 10)
(define luchtdruk-tile-y 650)
(define luchtdruk-text "Luchtdrukgeweer")
(define luchtdruk-pixels 250)

(define turbo-tile-x 230)
(define turbo-tile-y 650)
(define turbo-text "Turbogeweer")

(define water-tile-x 400)
(define water-tile-y 650)
(define water-text "Waterpistool")

(define net-tile-x 570)
(define net-tile-y 650)
(define net-text "Net")


;speler en score 

(define speler-levens 5)

(define score/vogel 10)
(define score/buizerd 20)
(define score/kraai 30)
(define score/condor 40)
(define score/net 50)


(define tijd-tussen-rondes -4000)

(define level-tijd -10)

;vogels


(define vogel-max 4)

(define max-vogel-tijd 2500)
(define max-valse-tijd 4000)
(define max-dood-tijd 500)
(define verstopt-tijd 2000)



(define snelheid-vlucht 0.80)
(define rand-eend 4)
(define verstopt-tijd-eend 800)

(define levens-eend 1)
(define snelheid-eend 0.1)

(define levens-buizerd 2)
(define buizerd-snelheid 0.15)

(define levens-condor 3)
(define condor-snelheid 0.07)
(define condor-sneller 0.06)


(define levens-kraai 3)
(define kraai-snelheid 0.14)

(define nieuw-tile-max 300)




;posities

(define obstakel1 (list (maak-positie 7 4) (maak-positie 7 3.5) (maak-positie 0 2)
                     (maak-positie 1 2) (maak-positie 1 1)))

(define obstakel3 (list (maak-positie 7 4) (maak-positie 1 3) (maak-positie 1 4)(maak-positie 3 2) (maak-positie 4 2)
                        (maak-positie 4 1)))

(define obstakel2 (list (maak-positie 2 4) (maak-positie 6.7 2) (maak-positie 7 2) (maak-positie 8 2)
                        (maak-positie 6.7 3) (maak-positie 7 3) (maak-positie 8 3)
                        (maak-positie 0 2) (maak-positie 1 2)))
 
(define obstakel4 (list (maak-positie 1 4) (maak-positie 1 4) (maak-positie 4 2) (maak-positie 5 2)
                        (maak-positie 4 3 ) (maak-positie 5 3) (maak-positie 5 1) (maak-positie 6 4)
                        (maak-positie 6 5) (maak-positie 6 6)))

(define stap 0.06)



(define rand-links 0.5)
(define rand-rechts 8.5)
(define rand-boven 0.5)
(define rand-onder 4.5)


(define begin-x 1)
(define eind-x 7)
(define begin-y 4.4)
(define eind-y 1)
(define y-begin 4)


;wapen


(define herhaal-tijd-luchtdruk 1000)
(define herhaal-tijd-turbo 5000)
(define herhaal-tijd-net 10000)
(define luchtdruk-positie 2.3)
(define turbo-positie 4)
(define water-positie 5.4)
(define net-positie 6.3)

(define kogels-luchtdruk 1)
(define kogels-turbo 5)
(define kogels-water 20)
(define kogels-net 1)


(define wapen-begin-x 5)
(define wapen-begin-y 3)

(define bereik 0.3)
(define bereik-condor 0.5)
(define bereik-net 0.8)
(define bereik-water 0.5)

  


;Power-up


(define power-up-tijd-max 10000)
(define power-up-tijd-max2 11000)

(define power-up-snelheid 0)

