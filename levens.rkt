
;levens ADT CHANGE SIGNATUUR




(define (maak-levens soort)
  (let
      ((levens 0))

    (cond ((not (symbol? soort)) (set! levens soort))
          ((or (eq? soort 'vals) (eq? soort 'eend) (eq? soort 'power-up)) (set! levens levens-eend))
          ((eq? soort 'buizerd) (set! levens levens-buizerd))
          ((eq? soort 'condor) (set! levens levens-condor))
          ((eq? soort 'kraai) (set! levens levens-kraai)))


  (define (verminder-levens! x) 
    (let
        ((nieuw-levens (if (number? levens)
                           (- levens x)
                           levens)))
      (if (or (symbol? levens) (>= 0 nieuw-levens))
          (set! levens 'dood)
          (set! levens nieuw-levens)))
    levens)
    

    (define (vermeerder-levens! x)
          (set! levens x))
 


  (define (dispatch-levens message)
    (cond
      ((eq? message 'levens) levens)
      ((eq? message 'verminder-levens!) verminder-levens!)
      ((eq? message 'vermeerder-levens!) vermeerder-levens!)
      (else (display "foute msg levens"))))
  dispatch-levens))