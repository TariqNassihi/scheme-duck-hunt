(define (maak-power-up soort)
  (let ((power-up-tijd 1)
        (actief? 'niet-actief)
        (levens (maak-levens 1))
        (power-up-tijd-max (if (eq? soort 'chaos) power-up-tijd-max power-up-tijd-max2))
        (verstopt? #f)
        (geactiveerd? #f)
        (snelheid power-up-snelheid)
        (positie (maak-positie (random begin-x eind-x) (random eind-y  y-begin))))



    (define  (activeer-pw! op vogel-lijst)
      (for-each (lambda (cons) 
                  (if (not(eq? ((car cons) 'type) 'power-up))
                      (((car cons) 'snelheid!) (op ((car cons) 'snelheid) 0.05))))
                    
                vogel-lijst))
          

    
    
(define (set-iets! symbool x)
  (cond
    ((eq? symbool 'tijd!) (set! power-up-tijd x))
    ((eq? symbool 'actief!) (set! actief? x))
    ((eq? symbool 'geactiveerd!)  (set! geactiveerd? x))))
    

    (define (update! tijd)
      (let ((pw power-up-tijd))        
        (set! power-up-tijd (+ power-up-tijd tijd))
            (cond
              (geactiveerd? (if (> power-up-tijd (/ power-up-tijd-max 2)) (set! actief? 'was-actief)))
              ((and (negative? pw) (positive? power-up-tijd)) (set! actief? 'klaar))
              ((and (positive? pw) (> power-up-tijd power-up-tijd-max))
               (if (= 0 (random 2))
                   (begin
                     (set! actief? 'actief) (set! power-up-tijd (-(/ power-up-tijd-max 2))))
                   
                   (set! power-up-tijd 1))))))
         


         
           
       
 
    (define (powerup-dispatch message)
      (cond ((eq? message 'soort) soort)
            ((eq? message 'type) 'power-up)
            ((eq? message 'activeer-pw!) activeer-pw!)
            ((eq? message 'actief?) actief?)
            ((eq? message 'update!) update!)
            ((eq? message 'set-iets!) set-iets!)
            ((eq? message 'levens) levens)
            ((eq? message 'verstopt?) verstopt?)
            ((eq? message 'positie) positie)
            ((eq? message 'geactiveerd?) geactiveerd?)
            ((eq? message 'snelheid) snelheid)
            (else (display message))))
    powerup-dispatch))