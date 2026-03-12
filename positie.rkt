
; positie adt


(define (maak-positie x y)


  (define (x! x2)
    (set! x x2))
  
  (define (y! y2)
    (set! y y2))
  
                                      
    
  (define (gelijk? positie2 wapen vogel)
    (let
        ((bereik (cond ((eq? wapen 'net) bereik-net)
                       ((eq? wapen 'water) bereik-water)
                       ((eq? vogel 'condor) bereik-condor)
                       (else bereik))))
                       

     (and  (< (abs (- x (positie2 'x))) bereik)
         (< (abs (- y (positie2 'y))) bereik))))



  (define (achter-obstakel? level soort)
    (let loop 
      ((lst (cond ((eq? level 1) obstakel1)
                  ((eq? level 2) obstakel2)
                  ((eq? level 3) obstakel3)
                  (else obstakel4)))
                  
       (x (floor x))
       (y (floor y)))
      
      (cond ((null? lst) #f)

            ((and (= x ((car lst) 'x)) (= y ((car lst) 'y)))  #t)
            ((eq? soort 'kraai) #f)
            (else  (loop (cdr lst) x y)))))

  
                    

  (define (beweeg-naar-steen! doel vogel)

    (let* ((huidige-x x)
           (huidige-y y)
           (afstand-x (- (doel 'x) huidige-x))
           (afstand-y (- (doel 'y) huidige-y)))
      (if (and (= huidige-x (doel 'x)) (= huidige-y (doel 'y)))
         ((vogel 'set-iets!) 'steen! #f)
          
           
            (let (( nieuwe-x (+ huidige-x (if (> (abs afstand-x) stap) (if (> afstand-x 0) stap (- stap)) afstand-x)))
                  ( nieuwe-y (+ huidige-y (if (> (abs afstand-y) stap) (if (> afstand-y 0) stap (- stap)) afstand-y))))
    
            (x! nieuwe-x)
            (y! nieuwe-y)))))
  
      

  

  (define (dispatch-positie msg)
    (cond ((eq? msg 'x) x)
          ((eq? msg 'y) y)
          ((eq? msg 'x!) x!)
          ((eq? msg 'beweeg-naar-steen!)  beweeg-naar-steen!)     
          ((eq? msg 'y!) y!)
          ((eq? msg 'achter-obstakel?) achter-obstakel?)
          ((eq? msg 'pos) (cons x y))
          ((eq? msg 'gelijk?) gelijk?)
          (else "error: foute msg positie")))
  dispatch-positie)