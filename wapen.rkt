
;wapen adt  


(define (maak-wapen soort)
  (let*
      ((herhaaltijd (cond ((eq? soort 'luchtdruk) herhaal-tijd-luchtdruk)
                          ((eq? soort 'turbo) herhaal-tijd-turbo)
                          ((eq? soort 'water) herhaal-tijd-turbo)
                          ((eq? soort 'net) herhaal-tijd-net)))
       
       (kogels (cond ((eq? soort 'luchtdruk) kogels-luchtdruk)
                     ((eq? soort 'turbo) kogels-turbo)
                     ((eq? soort 'water) kogels-water)
                     ((eq? soort 'net) kogels-net)))
                     
       (kogel kogels)
       
       (wapen-tijd (+ 1 herhaaltijd))
       
       (positie (maak-positie wapen-begin-x wapen-begin-y)))

    

    (define (wapen-tijd! tijd)
      (if (negative? wapen-tijd)
          (set! wapen-tijd (+ 1 herhaaltijd))         
          (set! wapen-tijd (+ wapen-tijd tijd)))
      (if (and (schieten?)(zero? kogel))
              (set! kogel kogels)))

  

    (define (schieten?)
      (and (>= kogel 0) (> wapen-tijd  herhaaltijd)))
       
    
     
  
 
    (define (reset-tijd!)
        (set! kogel (- kogel 1))
        (if (> 0 kogel)
            (begin
              (if (not (eq? soort 'water))
                  (set! kogel kogels))
              (set! wapen-tijd 0)) 
            (if (zero? kogel)
                (set! wapen-tijd 0)
                (set! wapen-tijd -1))))


    
       
    (define (reset-positie)
      (set! positie (maak-positie wapen-begin-x wapen-begin-y)))


    

     (define (dispatch-wapen message)
      (cond
        ((eq? message 'wapen-tijd) wapen-tijd)
        ((eq? message 'herhaaltijd) herhaaltijd)
        ((eq? message 'reset-positie) reset-positie)
        ((eq? message 'positie) positie)
        ((eq? message 'soort) soort)
        ((eq? message 'kogel) kogel)
        ((eq? message 'schieten?) schieten?)
        ((eq? message 'wapen-tijd!) wapen-tijd!)
        ((eq? message 'reset-tijd!) reset-tijd!)
        (else (display "FOUTE MSG WAPEN"))))
    dispatch-wapen))