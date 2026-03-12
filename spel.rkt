
;SPEL

(define (maak-spel)
  (let* ((teken-ADT (maak-teken pixels-horizontaal pixels-vertikaal))
        (level-ADT (maak-level venster-breedte-px venster-hoogte-px))
        (wapens (list (maak-wapen 'luchtdruk)(maak-wapen 'turbo)(maak-wapen 'water)(maak-wapen 'net)))
        (wapen-ADT (car wapens))
        (levens (maak-levens speler-levens))
        (power-ups (list (maak-power-up 'chaos) (maak-power-up 'bevriezing)))
        (score 0))



    (define (start!)
      ((teken-ADT 'teken-crosshair!) wapen-ADT)
      ((teken-ADT 'teken-wapen-text))
      ((teken-ADT 'teken-herhaaltijd))
      ((teken-ADT 'teken-pw))
      ((teken-ADT 'set-game-loop!) game-loop!)
      ((teken-ADT 'set-teken-loop!) teken-loop!)
      ((teken-ADT 'set-mouse-click-loop!) mouse-loop!)
      ((teken-ADT 'set-key-callback!) keyboard-loop!))


    
    (define (game-loop! delta-tijd)

      (if (not (symbol? (levens 'levens)))
          
          (begin
            (for-each (lambda (power-up) 
                        (if (not (eq? (power-up 'actief?) 'klaar))
                            ((power-up 'update!) delta-tijd)))
                      power-ups)
                
             (nieuw-power-up?)
            ((levens 'verminder-levens!) ((level-ADT 'vlucht)))
            ((teken-ADT 'nieuw-current!) delta-tijd)
            
            (let ((res ((level-ADT 'update!) teken-ADT delta-tijd power-ups)))
            (cond ((eq? res 'nieuw-ronde) (nieuw-power-up))
                  ((eq? res 'nieuw-level) (set-car! (cddr wapens) (maak-wapen 'water)))))
            
            ((level-ADT 'nieuwe-posities!) teken-ADT delta-tijd (level-ADT 'level))
            
            (let loop
                ((geraakt? ((level-ADT 'geraakt?) wapen-ADT teken-ADT)))
                
              (cond ((null? geraakt?) '())
                    ((eq? (car geraakt?) 'vals) ((levens 'verminder-levens!)1) (loop (cdr geraakt?)))
                    ((eq? (wapen-ADT 'soort) 'net) (set! score (+ score score/net)) (loop (cdr geraakt?)))
                    ((eq? (car geraakt?) 'eend) (set! score (+ score score/vogel)) (loop (cdr geraakt?)))
                    ((eq? (car geraakt?) 'buizerd) (set! score (+ score score/buizerd)) (loop (cdr geraakt?)))
                    ((eq? (car geraakt?) 'kraai) (set! score (+ score score/kraai)) (loop (cdr geraakt?)))
                    ((eq? (car geraakt?) 'condor) (set! score (+ score score/condor)) (loop (cdr geraakt?)))
                    
                    ((eq? ((car geraakt?) 'soort) 'bevriezing)
                     (((car geraakt?) 'activeer-pw!) - (teken-ADT 'vogel-tile-lijst)) (loop (cdr geraakt?)))
                    
                    ((eq? ((car geraakt?) 'soort) 'chaos)
                     ((level-ADT 'vogel-max!)*) (loop (cdr geraakt?)))))
                    
            (for-each (lambda (wapen) ((wapen 'wapen-tijd!) delta-tijd)) wapens))))

  
        

    (define (teken-loop!)   
      (if (not (symbol? (levens 'levens)))
          (begin
            ((teken-ADT 'vogel-tile-lijst!)(level-ADT 'vogel-lijst))
            ((teken-ADT 'update-menu!) (if (symbol? (levens 'levens))
                                           (levens 'levens)
                                           (number->string (levens 'levens)))
                                       (number->string score)((wapen-ADT 'schieten?))
                                       (number->string (level-ADT 'level))
                                       (if (eq? 4 (level-ADT 'ronde)) (number->string 3)(number->string (level-ADT 'ronde)))
                                       ((car power-ups) 'geactiveerd?) ((cadr power-ups) 'geactiveerd?)   (number->string (wapen-ADT 'kogel)))
                                
            ((teken-ADT 'update-teken!))

            (if ((teken-ADT 'teken!)(teken-ADT 'vogel-tile-lijst) (level-ADT 'vogel-lijst)) 
                (set-car! (level-ADT 'vogel-lijst) (cons #f (cdar (level-ADT 'vogel-lijst))))))
         

          (begin
            ((teken-ADT 'verwijder-vogels))
            ((teken-ADT 'verwijder-dood) (level-ADT 'dood-lijst))
            ((teken-ADT 'verwijder-teken))
            ((teken-ADT 'nieuw-map) -1))))
      
      

      (define (keyboard-loop! type toets)  
        (cond
              ((and (eq? type 'pressed) (eq? toets #\return))
                ((level-ADT 'continue!)))
              ((and (eq? type 'pressed) (eq? toets #\r) (not (symbol? (levens 'levens))))
               (let ((nieuwe-wapens (list (maak-wapen 'luchtdruk)(maak-wapen 'turbo)(maak-wapen 'water)(maak-wapen 'net))))
                     
                 ((level-ADT 'reset))
                 (set! score 0)
                 (set! wapens nieuwe-wapens)
                 (set! wapen-ADT (car wapens))
                 ((levens 'vermeerder-levens!) speler-levens)
                 ((teken-ADT 'teken-crosshair!) wapen-ADT)
                 ((teken-ADT 'teken-wapen-text))
                 ((teken-ADT 'teken-herhaaltijd))
                 (set! power-ups (list (maak-power-up 'chaos) (maak-power-up 'bevriezing)))
                 ((teken-ADT 'nieuw-map) 0)))))
               
       
      
    
    (define (mouse-loop! knop status x y)
      (let ((nieuwe-x (exact->inexact (/ x cel-breedte-px)))
            (nieuwe-y  (exact->inexact(/ y cel-hoogte-px))))
      (if (and (eq? knop 'left)
               (eq? status 'pressed)
               (or (> nieuwe-y 6)((wapen-ADT 'schieten?))))
          (begin
            ((wapen-ADT 'reset-tijd!))
            ((teken-ADT 'update-crosshair!) x y (wapen-ADT 'soort))
            (((wapen-ADT 'positie) 'x!) nieuwe-x)
            (((wapen-ADT 'positie) 'y!) nieuwe-y)
  
           (if (> ((wapen-ADT 'positie) 'y) 6)
               (cond ((<((wapen-ADT 'positie) 'x) luchtdruk-positie)(verander-wapen 'luchtdruk))
                     ((<((wapen-ADT 'positie) 'x) turbo-positie)(verander-wapen 'turbo))
                     ((<((wapen-ADT 'positie) 'x) water-positie)(verander-wapen 'water))
                     ((<((wapen-ADT 'positie) 'x) net-positie)  (verander-wapen 'net))))))))
    
                      

 
          (define (verander-wapen soort)
            (if (not (eq? soort (wapen-ADT 'soort)))
                (begin
                (cond ((eq? soort 'luchtdruk) (set! wapen-ADT (car wapens)))
                      ((eq? soort 'turbo) (set! wapen-ADT (cadr wapens)))
                      ((eq? soort 'water) (set! wapen-ADT (caddr wapens)))
                      ((eq? soort 'net) (set! wapen-ADT (cadddr wapens))))
                ((wapen-ADT 'reset-positie))
  
                ((teken-ADT 'verander-crosshair!) soort wapen-ADT))))

    
                

    (define (nieuw-power-up)
      (for-each (lambda (power-up)
                  (if (eq? (power-up 'actief?) 'klaar)
                  
                      (let ((pw (maak-power-up (power-up 'soort))))

                        (if (eq? (power-up 'soort) ((car power-ups) 'soort))
                            (set-car! power-ups pw)
                            (set-car! (cdr power-ups) pw)))))
                power-ups))


    (define (nieuw-power-up?)
   
      (for-each (lambda (power-up)
                  (cond ((null? (teken-ADT 'vogel-tile-lijst)) )

                    ((eq? (power-up 'actief?) 'actief)
                         ((level-ADT 'nieuw-vogel!) #t power-up power-ups) ((power-up 'set-iets!) 'actief! 'getekend))
            
                        ((and (eq? (power-up 'actief?) 'was-actief) (eq? (power-up 'soort) 'chaos))
                    
                         ((level-ADT 'vogel-max!) /)
                         ((power-up 'set-iets!) 'geactiveerd! #f)
                         ((power-up 'set-iets!) 'tijd! 0)
                         ((power-up 'set-iets!) 'actief! 'klaar))
                  
                        ((and (eq? (power-up 'actief?) 'was-actief) (eq? (power-up 'soort) 'bevriezing))
                         ((power-up 'activeer-pw!) + (teken-ADT 'vogel-tile-lijst))
                         ((power-up 'set-iets!) 'geactiveerd! #f)
                         ((power-up 'set-iets!) 'tijd! 0)
                         ((power-up 'set-iets!) 'actief! 'klaar))))
                 
                power-ups))


    

     (define (dispatch-spel message)
      (cond
        ((eq? message 'teken-ADT) teken-ADT)
        ((eq? message 'level-ADT) level-ADT)
        ((eq? message 'wapen-ADT) wapen-ADT)
        ((eq? message 'start!) start!)
        (else (display "FOUTE MSG SPEL"))))      
    dispatch-spel))