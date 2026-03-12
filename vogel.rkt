
;vogel ADT



(define (maak-vogel type soort)
  (let ((positie (if (not type) (maak-positie begin-x begin-y) (maak-positie (random begin-x eind-x) begin-y)))
        (levens (maak-levens soort))
        (verstopt? #f)
        (water? #f)
        (snelheid   (cond ((or (eq? soort 'vals) (eq? soort 'eend)) snelheid-eend)
                          ((eq? soort 'buizerd) buizerd-snelheid)
                          ((eq? soort 'condor) condor-snelheid)
                          ((eq? soort 'kraai) kraai-snelheid)))
        (steen? #f)
                   
        (rand-voor-vluchten rand-eend)
        (richting-x (if (not type) + (if (= (random 2) 0) + -)))
        (richting-y -)
        (verstopt-tijd verstopt-tijd-eend)) 



    (define (set-iets! symbol x)
      (cond
        ((eq? symbol 'verstopt-tijd!) (set! verstopt-tijd x))
        ((eq? symbol 'steen!) (set! steen? x))
        ((eq? symbol 'set-richting-x!) (set! richting-x x)) 
        ((eq? symbol 'set-richting-y!)  (set! richting-y x))
        ((eq? symbol 'water!) (set! water? x))
        ((eq? symbol 'vlucht!) (set! rand-voor-vluchten x)))) 
    


    (define (verstopt!)
      (if verstopt?
          (set! verstopt? #f)
          (set! verstopt? #t)))
     
    


    (define (rand-voor-vluchten!)
           (set! steen? #t)
      (let
          ((nieuw (if (not (symbol? rand-voor-vluchten))
                      (- rand-voor-vluchten 1)
                      rand-voor-vluchten )))

        (if (or (symbol? nieuw) (zero? nieuw))
            (begin
            (set! rand-voor-vluchten 'vlucht)
            (set! snelheid snelheid-vlucht))
            (set! rand-voor-vluchten nieuw))
        rand-voor-vluchten))


    
  (define (random-richting! as)
    (let
        ((richting (if (zero? (random 2)) + -)))

      (if (eq? as 'x)
          (set! richting-x richting)
          (set! richting-y richting))))
             
    
    (define (snelheid! tijd)
      (if (negative? tijd)
          0
          (set! snelheid tijd)))   

    



    (define (dispatch-vogel message)
      (cond
        ((eq? message 'type) type)
        ((eq? message 'soort) soort)
        ((eq? message 'water?) water?)
        ((eq? message 'levens) levens)
        ((eq? message 'snelheid) snelheid)
        ((eq? message 'snelheid!) snelheid!)
        ((eq? message 'verstopt-tijd) verstopt-tijd)
        ((eq? message 'verstopt?) verstopt?)
        ((eq? message 'verstopt!) verstopt!)
        ((eq? message 'richting-x) richting-x)
        ((eq? message 'richting-y) richting-y)
        ((eq? message 'random-richting!) random-richting!)
        ((eq? message 'set-iets!) set-iets!)
        ((eq? message 'steen?) steen?)
        ((eq? message 'rand-voor-vluchten!) rand-voor-vluchten!)
        ((eq? message 'rand-voor-vluchten) rand-voor-vluchten)
        ((eq? message 'positie) positie)
        (else (display "FOUTE MSG VOGEL"))))      
    dispatch-vogel))