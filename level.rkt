; level



(define (maak-level game-width game-height)
  
  (let ((vogel-tijd 0)
        (vogel-counter 0)
        (dood-tijd 0)
        (crosshair-tijd 0)
        (valse-tijd 0)
        (continue? #f)
        (max-vals max-valse-tijd)
        (vogel-max vogel-max) 
        (vogel-lijst '())
        (dood-lijst '())
        (vogel-maximum vogel-max)
        (max-tijd-vogel max-vogel-tijd) 
        (ronde 1)
        (ronde-tijd 1)
        (level 1))

    



    ;; nieuwe vogels aanmaken
    
    (define (vogel-lijst! teken pw)

      (if (not (> ronde 3))        
      (let*
          ((random (random 6))
           (soort (cond ((eq? random 0) 'buizerd)
                        ((eq? random 1) (if (> level 1) 'kraai 'buizerd))
                        ((eq? random 2) (cond ((> level 2) 'condor)
                                              ((= level 2) 'kraai)
                                              (else 'buizerd)))
                        (else 'eend))))
        
                        
      (cond ((negative? ronde-tijd) )
            
            ((> vogel-counter vogel-maximum)
             (set! vogel-tijd 0)
             (if (eq? ronde 3) (set! ronde-tijd level-tijd) (set! ronde-tijd tijd-tussen-rondes)))

            ((> vogel-tijd max-tijd-vogel)
             (set! vogel-counter (+ vogel-counter 1))
             (set! vogel-tijd 0)
             (nieuw-vogel! #t soort pw))
            
            ((> valse-tijd max-vals)
             (set! vogel-counter (+ vogel-counter 1))
             (set! valse-tijd 0)
             (nieuw-vogel! #f 'eend pw))))
      

      (if continue?
;Nieuwe level beginnen
        (begin
        (set! vogel-maximum (+ vogel-maximum 3))
        (set! max-tijd-vogel (if (negative? max-tijd-vogel)
                                 max-tijd-vogel
                                  (- max-tijd-vogel 300)))
        (set! max-vals (- max-vals 500))
        (set! ronde 1)
        (set! continue? #f)
        (set! level (+ level 1))
        (set! vogel-counter 0)
        (set! vogel-lijst '())
        (set! vogel-tijd 0)
        ((teken 'nieuw-map) level)
        'nieuw-level))))
           
        
    (define (continue!)
      (if  (eq? ronde 4)
           (set! continue? #t)))

    
       
    (define (dood-lijst! teken)
      (if (and (not (null? dood-lijst)) (> dood-tijd max-dood-tijd))
          (begin
            (set! dood-tijd 0)   
            ((teken 'verwijder-tile!) (car dood-lijst))
            (set! dood-lijst (cdr dood-lijst)))))


    

    (define (toevoeg-dood-lijst! el)
      (set! dood-lijst (append dood-lijst (list el))))


    
    
    (define (nieuw-vogel! bool soort pw)
      (let ((nieuw-vogel (if (symbol? soort)
                             (maak-vogel bool soort)
                             soort)))
        
        (if (and (eq? ((car pw) 'soort) 'bevriezing)((car pw) 'geactiveerd?) (symbol? soort))
            (begin
            ((nieuw-vogel 'snelheid!)(- (nieuw-vogel 'snelheid) 0.05))))
            
        (set! vogel-lijst (append (list (cons #t nieuw-vogel)) vogel-lijst))))
  
        
    

    (define (update! teken delta-tijd pw)
      (let ((res 'niks))
        (set! vogel-tijd (+ delta-tijd vogel-tijd))
        (set! valse-tijd (+ delta-tijd valse-tijd))
        (if (not (null? dood-lijst))
            (set! dood-tijd (+ delta-tijd dood-tijd)))      

        (cond ((and  (negative? ronde-tijd) (null? (teken 'vogel-tile-lijst)))
               (set! ronde-tijd (+ ronde-tijd delta-tijd))
               (if (positive? ronde-tijd) ;nieuwe ronde
                   (begin
                     (set! res 'nieuw-ronde)
                     (set! vogel-counter 0)
                     (set! vogel-lijst '())
                     (set! ronde (+ ronde 1)))))
              ((negative? ronde-tijd)  (set! ronde-tijd 0)))             
                                
        (dood-lijst! teken)
        (if (eq? (vogel-lijst! teken pw) 'nieuw-level)
              (set! res 'nieuw-level))
        res))



    ; controleer of een vogel is geraakt
    
    (define (geraakt? wapen teken)  
      (let loop
        ((lst (teken 'vogel-tile-lijst))
         (vorige '())
         (geraakt '()))
 
        (if (not (null? lst))
          
            (let* ((pos-wapen (wapen 'positie))
                   (object (caar lst))
                   (pos-vogel (object 'positie))
                   (tile (cdar lst))         
                   (type (object  'type))
                   (soort (object  'soort))
                   (soort-wapen (wapen 'soort))
                   (geraakt? #f)
                   (levens (object 'levens)))

              (cond (((teken 'gevlucht?) tile) ((teken 'nieuw-tile-lijst!) vorige (cdr lst)))

                    ((and (eq? type 'power-up) (eq? (object 'actief?) 'klaar))
                     ((teken 'verwijder-pw!) object) 
                     ((teken 'nieuw-tile-lijst!) vorige (cdr lst)))                                                                                                                                                                       

                    
                    ((and ((pos-wapen 'gelijk?) pos-vogel soort-wapen soort)  (not (object 'verstopt?)) 
                          (or (eq? (wapen 'wapen-tijd) 0) (eq? (wapen 'wapen-tijd) -1))
                          (set! geraakt? #t)(symbol? ((levens 'verminder-levens!) 1)) (not (eq? soort-wapen 'water)))

                     
                     (toevoeg-dood-lijst! ((teken 'vogel-tile->dood-tile!) tile object))
                     ((teken 'nieuw-tile-lijst!) vorige (cdr lst))
                     (cond ((eq? type 'power-up) (set! geraakt (cons object geraakt))
                                                 ((object 'set-iets!) 'geactiveerd! #t)
                                                 ((object 'set-iets!) 'tijd! 0))
                                                
                           (type (set! geraakt (cons soort geraakt)))
                           (else (set! geraakt (cons 'vals geraakt))))))
                           

              
                  (if geraakt?
                      (cond ((eq? soort-wapen 'water)
                             (if (not (object 'water?))
                                 ((object 'snelheid!) (- (object 'snelheid) 0.04)))
                             ((object 'set-iets!) 'water! #t)
                             ((levens 'vermeerder-levens!) (if (symbol? (levens 'levens)) 1 (+ 1 (levens 'levens)))))
                            
                            ((eq? soort 'condor) ((object 'snelheid!) (+ (object 'snelheid) condor-sneller)))))
              
              (loop (cdr lst) (append (list (car lst)) vorige) geraakt))
              
            geraakt)))


 
    
    (define (volgende-positie-parabool object x y delta-t)
      (if (> x y-begin)
           ((object 'set-iets!) 'set-richting-y! +))      
      (let*
          ((op-x (object 'richting-x))
           (op-y (object 'richting-y))    
           (x-volgende (op-x x (/ (* snelheid-eend delta-t) cel-breedte-px)))
           (y-volgende (op-y y (/ (* snelheid-eend delta-t) cel-hoogte-px))))      
        (cons x-volgende y-volgende)))



    
   ; posities updaten 

    
  (define (nieuwe-posities! teken ms level)
  
      (define (loop vogel-lijst vorige)      
        (if (not (null? vogel-lijst))     
            (let*
                ((object (caar vogel-lijst))
                 (tile (cdar vogel-lijst))
                 (snelheid (object 'snelheid))
                 (doel (cond ((eq? level 2) (car obstakel2))
                             ((eq? level 3) (car obstakel3))
                             (else (car obstakel4))))
                             
                 (type (object 'type))
                 (pos (object 'positie))
                 (x (pos 'x))
                 (y (pos 'y))              
                 (vlucht? (if (eq? type 'power-up) #f (symbol? (object 'rand-voor-vluchten)))))

         
              (if (not type)
                  (let 
                      ((positie (volgende-positie-parabool object x y  ms)))
                    (if (> y begin-y )
                        ((teken 'verwijder-tile!) tile))
                        (begin
                          ((pos 'x!) (car positie))
                          ((pos 'y!) (cdr positie))))
                 
                  (begin
                  ; Controleer of een vogel een rand heeft bereikt
                  (cond
                    ((eq? type 'power-up) '())
                    (vlucht?   ((object 'set-iets!) 'set-richting-y! -))
              
                    ((= x rand-rechts)
                     ((object 'set-iets!) 'set-richting-x! -)
                     ((object 'rand-voor-vluchten!))
                     ((object 'random-richting!) 'y))
              
                    ((= y rand-onder)
                     ((object 'set-iets!) 'set-richting-y! -)
                     ((object 'rand-voor-vluchten!))
                     ((object 'random-richting!) 'x))
              
                    ((= x rand-boven)
                     ((object 'set-iets!) 'set-richting-x! +)
                     ((object 'rand-voor-vluchten!))
                     ((object 'random-richting!) 'y))
              
                    ((= y rand-links)
                     ((object 'set-iets!) 'set-richting-y! +)
                     ((object 'rand-voor-vluchten!))
                     ((object 'random-richting!) 'x)))
              
                 
                  (cond ((or (eq? type 'power-up) (object 'verstopt?)) '())

                        ((and (eq? (object 'soort) 'kraai) (object 'steen?)
                              (not(symbol? (object 'rand-voor-vluchten))) (< (object 'rand-voor-vluchten) rand-eend)) 
                         ((pos 'beweeg-naar-steen!) doel object))
                        
                      (else 
                   
                          (let* ((op-x (object 'richting-x))
                                 (op-y (object 'richting-y))
        
                                 (nieuw-pos-x (bereik 'x (op-x x (/(* snelheid ms) cel-breedte-px)) vlucht?))
                                 (nieuw-pos-y (bereik 'y (op-y y (/(* snelheid ms) cel-hoogte-px)) vlucht?)))

                            (if vlucht?
                          
                                ((pos 'y!) nieuw-pos-y)
                            
                                (begin
                                  ((pos 'x!) nieuw-pos-x)
                                  ((pos 'y!) nieuw-pos-y))))))
                      

                  (verstopt! object teken vorige ms tile vogel-lijst)))
         
              (loop (cdr vogel-lijst) (append vorige (list (car vogel-lijst)))))))

      (loop (teken 'vogel-tile-lijst) '()))

  


    
    (define (bereik richting getal vlucht?)
      (cond (vlucht? getal)
            ((and (eq? richting 'x) (<= getal rand-links))rand-links)
            ((and (eq? richting 'x) (>= getal rand-rechts))rand-rechts)
            ((and (eq? richting 'y) (<= getal rand-boven)) rand-boven)
            ((and (eq? richting 'y) (>= getal rand-onder))rand-onder)
            (else getal)))
            
      

            


    (define (verstopt! object teken vorige ms tile vogel-lijst)
          
      (if (and (not (or (eq? (object 'type) 'power-up) (eq? (object 'soort) 'buizerd))) (object 'type))

          (let ((rand  (eq? 0 (random 2)))
                (soort (object 'soort))
                (verstopt? (object 'verstopt?))
                (tijd (object 'verstopt-tijd)))
      
            (cond   ((and (((object 'positie) 'achter-obstakel?) level soort)
                          (not verstopt?) (<= tijd (- verstopt-tijd))
                          (if (eq? soort 'kraai) rand #t))
               
                     ((object 'verstopt!))
                     ((teken 'verwijder-tile!) tile))
                    
                    ((and (not verstopt?) (> tijd (- verstopt-tijd)))
                     ((object 'set-iets!) 'verstopt-tijd! (- tijd ms)))

                    ((and verstopt? (< tijd verstopt-tijd))
                     ((object 'set-iets!) 'verstopt-tijd! (+ tijd ms)))

                    ((and verstopt? (>= tijd verstopt-tijd))
                     ((teken 'nieuw-tile-lijst!) vorige (cdr vogel-lijst))
                     ((teken 'teken-terug) object))))))
       



    ;;chaos
    

    (define (vogel-max! op)
      (set! vogel-maximum (op vogel-maximum 2))
      (let* ((op2 (if (eq? op *) - +))
            (nieuw (op2 max-tijd-vogel 500))) 
        (if (not (negative? nieuw))  
            (set! max-tijd-vogel nieuw))))




     
    (define (vlucht)
      (define (loop lst cntr)
        (if (not (null? lst))
            
            (let* ((object (cdar lst))
                   (vlucht? (if (eq? (object 'type) 'power-up)
                                #f
                                (eq? (object 'rand-voor-vluchten) 'vlucht))))
              
              (if vlucht?
                  (begin
                  ((object 'set-iets!) 'vlucht! 'vlucht+levens-1)
                  (loop (cdr lst) (+ 1 cntr)))
              (loop (cdr lst) cntr)))

            cntr))
      (loop vogel-lijst 0))


    
 
    (define (reset)
      (set! vogel-tijd 0)
      (set! vogel-counter 0)
      (set! dood-tijd 0)
      (set! crosshair-tijd 0)
      (set! valse-tijd 0)
      (set! continue? #f)
      (set! vogel-max vogel-max) 
      (set! vogel-lijst '())
      (set! dood-lijst '())
      (set! vogel-maximum vogel-max)
      (set! max-tijd-vogel max-vogel-tijd) 
      (set! ronde 1)
      (set! ronde-tijd 1)
      (set! level 1))



       (define (dispatch-level message)
      (cond
        ((eq? message 'update!) update!)
        ((eq? message 'vogel-max!) vogel-max!)
        ((eq? message 'level) level)
        ((eq? message 'continue!) continue!)
        ((eq? message 'ronde) ronde)
        ((eq? message 'geraakt?) geraakt?)
        ((eq? message 'reset) reset)
        ((eq? message 'vlucht) vlucht)
        ((eq? message 'nieuwe-posities!) nieuwe-posities!)
        ((eq? message 'vogel-lijst) vogel-lijst)
        ((eq? message 'nieuw-vogel!) nieuw-vogel!)
        ((eq? message 'dood-lijst) dood-lijst)
        (else (display "FOUTE MSG LEVEL"))))
    dispatch-level))