(#%require "Graphics.rkt")


(define (maak-teken pixels-horizontaal pixels-verticaal)
  (let ((venster (make-window pixels-horizontaal pixels-verticaal "Tower-Defence"))
        (vogel-tile-lijst '())
        (pw-list '())
        (valse-curr 0)
        (nieuw-tile-tijd 0))
    
    

   ; achtergrond + lagen instellen
    
    
    (define achtergrond-laag ((venster 'new-layer!)))

    (define achtergrond-tile
      (make-bitmap-tile  "images/map.png"))

    ((achtergrond-laag 'add-drawable!) achtergrond-tile)

   
    (define vogel-laag ((venster 'new-layer!)))

 
    (define wapen-laag ((venster 'new-layer!)))
    
    (define menu-laag ((venster 'new-layer!)))


    

    (define (nieuw-map level)
      ((achtergrond-laag 'remove-drawable!) achtergrond-tile)
      (cond
        ((eq? level -1) 
         (set! achtergrond-tile (make-bitmap-tile "images/game-over.png")))
    
        ((eq? level 0) (set! achtergrond-tile (make-bitmap-tile "images/map.png")))
    
        ((eq? level 2) (set! achtergrond-tile (make-bitmap-tile "images/map2.png")))
        ((eq? level 3) (set! achtergrond-tile (make-bitmap-tile "images/map3.png")))
        (else (set! achtergrond-tile (make-bitmap-tile "images/map4.png"))))
      ((achtergrond-laag 'add-drawable!) achtergrond-tile))


  
 ;objecten tekenen


  (define (teken! lijst-tile lijst-vogels)
      (define verandert? #f)    
      (if (and (not (null? lijst-tile)) (not (null? lijst-vogels)) (caar lijst-vogels))
          (let*
              ((object (caar lijst-tile))
               (type (object 'type))
               (tile (cdar lijst-tile)))
            ((vogel-laag 'add-drawable!) tile)
            (teken-object! object tile)
            (set! verandert? #t)))
            verandert?)

    
    (define (teken-object! obj tile)        
      (let* ((obj-x ((obj 'positie) 'x)) 
             (obj-y ((obj 'positie) 'y))
          
            (screen-x (* cel-breedte-px obj-x))
             (screen-y (* cel-hoogte-px obj-y)))
        ((tile 'set-x!) (- screen-x pixels-centrum-vogel ))
        ((tile 'set-y!) (- screen-y pixels-centrum-vogel))))


    ;crosshair tekenen

    
     (define crosshair-tile (make-bitmap-tile "images/crosshair.png" "images/crosshair-mask.png"))
       
      
     (define (teken-crosshair! wapen)
         (teken-object! wapen crosshair-tile)
        ((wapen-laag 'add-drawable!) crosshair-tile))



    (define (verander-crosshair! soort wapen)
      ((wapen-laag 'remove-drawable!) crosshair-tile)

      (let
          ((nieuw-tile (cond ((eq? soort 'luchtdruk) (make-bitmap-tile "images/crosshair.png" "images/crosshair-mask.png"))
                             ((eq? soort 'turbo) (make-bitmap-tile "images/turbo.png" "images/turbo-mask.png"))
                             ((eq? soort 'water) (make-bitmap-tile "images/water.png" "images/water-mask.png"))
                             ((eq? soort 'net) (make-bitmap-tile "images/net.png" "images/net-mask.png")))))
        (set! crosshair-tile nieuw-tile)
        (teken-crosshair! wapen)))
        
                             

    ; tiles aanmaken + verwijderen

      
    (define (verwijder-tile! tile)
      ((vogel-laag 'remove-drawable!) tile))

(define (gevlucht? tile)
 (or (< ((tile 'get-y)) y-min) (> ((tile 'get-y)) y-max)))

    
    (define (vogel-tile-lijst! vogel-list)
      (cond
        ((null? vogel-list) '())
        ((caar vogel-list) 
          (let*
            ((object (cdar vogel-list))
            (positie (object 'positie))
            (type (object 'type))
            (soort (object 'soort))
            (vogel-tile (if type
                            (cond ((eq? soort 'eend)
                                   (make-tile-sequence (list (make-bitmap-tile "images/eend.png" "images/eend-mask.png")
                                                             (make-bitmap-tile "images/eend2.png" "images/eend2-mask.png")
                                                             (make-bitmap-tile "images/eend3.png" "images/eend3-mask.png")
                                                             (make-bitmap-tile "images/eend-b.png" "images/eend-mask.png")
                                                             (make-bitmap-tile "images/eend2-b.png" "images/eend2-mask.png")
                                                             (make-bitmap-tile "images/eend3-b.png" "images/eend3-mask.png"))))

                                  ((eq? soort 'bevriezing)
                                   (make-tile-sequence (list (make-bitmap-tile "images/bevriezing.png" "images/bevriezing-mask.png"))))
                                        

                                  ((eq? soort 'chaos)
                                    (make-tile-sequence (list (make-bitmap-tile "images/chaos.png" "images/chaos-mask.png"))))
                                         
                                  
                                  ((eq? soort 'buizerd)
                                   (make-tile-sequence (list (make-bitmap-tile "images/buizerd.png" "images/buizerd-mask.png")
                                                             (make-bitmap-tile "images/buizerd2.png" "images/buizerd2-mask.png")
                                                             (make-bitmap-tile "images/buizerd1.png" "images/buizerd1-mask.png")
                                                             (make-bitmap-tile "images/buizerd-b.png" "images/buizerd-mask.png")
                                                             (make-bitmap-tile "images/buizerd2-b.png" "images/buizerd2-mask.png")
                                                             (make-bitmap-tile "images/buizerd1-b.png" "images/buizerd1-mask.png"))))
                                  ((eq? soort 'condor)
                                   (make-tile-sequence (list (make-bitmap-tile "images/condor.png" "images/condor-mask.png")
                                                             (make-bitmap-tile "images/condor2.png" "images/condor2-mask.png")
                                                             (make-bitmap-tile "images/condor.png" "images/condor-mask.png")
                                                             (make-bitmap-tile "images/condor-b.png" "images/condor-mask.png")
                                                             (make-bitmap-tile "images/condor2-b.png" "images/condor2-mask.png")
                                                             (make-bitmap-tile "images/condor-b.png" "images/condor-mask.png"))))
                                  ((eq? soort 'kraai)
                                   (make-tile-sequence (list (make-bitmap-tile "images/kraai.png" "images/kraai-mask.png")
                                                             (make-bitmap-tile "images/kraai1.png" "images/kraai1-mask.png")
                                                             (make-bitmap-tile "images/kraai2.png" "images/kraai2-mask.png")
                                                             (make-bitmap-tile "images/kraai-b.png" "images/kraai-mask.png")
                                                             (make-bitmap-tile "images/kraai1-b.png" "images/kraai1-mask.png")
                                                             (make-bitmap-tile "images/kraai2-b.png" "images/kraai2-mask.png")))))
                                  
                                  
                             (make-tile-sequence(list (make-bitmap-tile "images/bom.png" "images/bom-mask.png")
                                                      (make-bitmap-tile "images/rubberen-eendje.png" "images/rubberen-eendje-mask.png")
                                                      (make-bitmap-tile "images/kleiduif.png" "images/kleiduif-mask.png"))))))
                                                     

              (cond ((eq? type 'power-up) (set! pw-list (append (list (cons object vogel-tile)) pw-list)))
                    ((not type) ((vogel-tile 'set-current!) valse-curr) (set! valse-curr (modulo (+ 1 valse-curr) 3))))
          
            (set! vogel-tile-lijst (append (list (cons object vogel-tile)) vogel-tile-lijst))))))
                          
            

    
  (define (nieuw-tile-lijst! stuk1 stuk2)
    (set! vogel-tile-lijst (append stuk1 stuk2)))


    (define (verwijder-vogels)
      (let loop
        ((lst vogel-tile-lijst))

        (if (not (null? lst))
            (begin
            (verwijder-tile! (cdar lst))
            (loop (cdr lst)))))
      (set! vogel-tile-lijst '()))


      
      

    
 ; posities van tiles veranderen


    (define (update-crosshair! x y soort)
      (let* ((centrum (cond ((eq? soort 'water) pixels-centrum-water)
                           ((eq? soort 'net) pixels-centrum-net-x)
                           (else pixels-centrum-crosshair)))
            (centrum2 (if (eq? soort 'net) pixels-centrum-net-y centrum))) 
      ((crosshair-tile 'set-x!) (- x centrum))
      ((crosshair-tile 'set-y!) (- y centrum2))))
           

   

    (define (update-tile! tile object vlucht? snelheid ms)
      (let* 
          ((op-x (object 'richting-x))
           (op-y (object 'richting-y))
           (huidige-x ((tile 'get-x)))
           (huidige-y ((tile 'get-y)))
           (nieuwe-y (op-y huidige-y  (* snelheid ms)))     
          (nieuwe-x (op-x huidige-x  (* snelheid ms))))
           
        (if vlucht?
            ((tile 'set-y!)  nieuwe-y)         
            (begin
              ((tile 'set-x!) nieuwe-x)
              ((tile 'set-y!)  nieuwe-y)))))
        
        


    (define (update-teken!)

      (define (loop vogel-lijst)      

        (if (not (null? vogel-lijst))     
            (let*
                ((object (caar vogel-lijst))
                 (tile (cdar vogel-lijst)))
              
              (teken-object!  object tile)

              (loop (cdr vogel-lijst)))))
      (loop vogel-tile-lijst))

    

              
        

    (define (teken-terug obj)
     
      (let ((nieuw-tile (if (obj 'type)
                            (cond ((eq? (obj 'soort) 'eend)
                                   (make-tile-sequence (list (make-bitmap-tile "images/eend.png" "images/eend-mask.png")
                                                             (make-bitmap-tile "images/eend2.png" "images/eend2-mask.png")
                                                             (make-bitmap-tile "images/eend3.png" "images/eend3-mask.png")
                                                             (make-bitmap-tile "images/eend-b.png" "images/eend-mask.png")
                                                             (make-bitmap-tile "images/eend2-b.png" "images/eend2-mask.png")
                                                             (make-bitmap-tile "images/eend3-b.png" "images/eend3-mask.png"))))
                                  ((eq? (obj 'soort) 'condor)
                                   (make-tile-sequence (list (make-bitmap-tile "images/condor.png" "images/condor-mask.png")
                                                             (make-bitmap-tile "images/condor.png" "images/condor-mask.png")
                                                             (make-bitmap-tile "images/condor.png" "images/condor-mask.png")
                                                             (make-bitmap-tile "images/condor-b.png" "images/condor-mask.png")
                                                             (make-bitmap-tile "images/condor2-b.png" "images/condor2-mask.png")
                                                             (make-bitmap-tile "images/condor-b.png" "images/condor-mask.png"))))
                                                            

                                  ((eq? (obj 'soort) 'kraai)
                                   (make-tile-sequence (list (make-bitmap-tile "images/kraai.png" "images/kraai-mask.png")
                                                             (make-bitmap-tile "images/kraai1.png" "images/kraai1-mask.png")
                                                             (make-bitmap-tile "images/kraai2.png" "images/kraai2-mask.png")
                                                             (make-bitmap-tile "images/kraai-b.png" "images/kraai-mask.png")
                                                             (make-bitmap-tile "images/kraai1-b.png" "images/kraai1-mask.png")
                                                             (make-bitmap-tile "images/kraai2-b.png" "images/kraai2-mask.png"))))))))
                                                            

                                  
                                                      
        (teken-object! obj nieuw-tile)
        ((vogel-laag 'add-drawable!) nieuw-tile)
        ((obj 'verstopt!))
        ((obj 'set-iets!) 'verstopt-tijd! 0)
        (set! vogel-tile-lijst (append (list (cons obj nieuw-tile)) vogel-tile-lijst))))





        

    ; vogel geraakt
    
                 
  
    (define (vogel-tile->dood-tile! tile object)
      (let* ((type (object 'type))
             (x ((tile 'get-x)))
             (y ((tile 'get-y)))
             (water? (if (eq? type 'power-up) #f (object 'water?)))
             (soort (object 'soort))
             (dood-tile (if type
                            (cond ((and (eq? soort 'eend) water?) 
                                   (make-bitmap-tile "images/eend-b-dood.png" "images/dood-mask.png"))
                                  ((eq? soort 'eend)
                                   (make-bitmap-tile "images/dood.png" "images/dood-mask.png"))                                  
                                   ((and (eq? soort 'buizerd) water?)
                                     (make-bitmap-tile "images/buizerd-b-dood.png" "images/buizerd-dood-mask.png"))                                  
                                   ((eq? soort 'buizerd)
                                     (make-bitmap-tile "images/buizerd-dood.png" "images/buizerd-dood-mask.png"))
                                    ((and (eq? soort 'condor) water?)
                                     (make-bitmap-tile "images/condor-b-dood.png" "images/condor-dood-mask.png"))                                   
                                   ((eq? soort 'condor)
                                     (make-bitmap-tile "images/condor-dood.png" "images/condor-dood-mask.png"))
                                   ((and (eq? soort 'kraai) water?)
                                     (make-bitmap-tile "images/kraai-b-dood.png" "images/kraai-dood-mask.png"))                                   
                                   ((eq? soort 'kraai)
                                     (make-bitmap-tile "images/kraai-dood.png" "images/kraai-dood-mask.png"))
                                   (else (make-tile 1 1)))
                                                             
                                                              
                        (make-bitmap-tile "images/explosie.png" "images/explosie-mask.png"))))
                       
        ((vogel-laag 'remove-drawable!) tile)
        ((vogel-laag 'add-drawable!) dood-tile)
        ((dood-tile 'set-x!) x)
        ((dood-tile 'set-y!) y)
        dood-tile))
    



    
    (define (verwijder-dood lst)

      (if (not (null? lst))
          (begin
            (verwijder-tile! (car lst))
            (verwijder-dood (cdr lst)))))
    


    
    ; call-back's  instellen     

 (define (set-game-loop! functie)
      ((venster 'set-update-callback!) functie))


  (define (set-mouse-click-loop! functie)
    ((venster 'set-mouse-click-callback!) functie))


 
       
(define (set-key-callback! functie)
  ((venster 'set-key-callback!) functie))
 
(define (set-teken-loop! functie)
  ((venster 'set-draw-callback!) functie))





    

    ; volgende in de tile sequence

    
    
    (define (nieuw-current! ms)
      (define (loop lst)
        (if (not (null? lst))
            (let*
                ((tile (cdar lst))
                 (object (caar lst))
                 (type (object 'type))
                 (curr (tile 'get-current))
                 (water? (if (eq? type 'power-up) #f (object 'water?)))
                 (nieuw-curr (if water?
                                 (+ 3 (modulo (+ curr 1) 3))
                                 (modulo (+ curr 1) 3))))

              (if (and type (> nieuw-tile-tijd nieuw-tile-max) (not (eq? type 'power-up)))

                  (begin
                  ((tile 'set-current!) nieuw-curr)
                   (set! nieuw-tile-tijd 0))

                  (set! nieuw-tile-tijd (+ nieuw-tile-tijd ms)))
              
              
              (loop (cdr lst)))))
      (loop vogel-tile-lijst))





    ;power-up

   

    (define (verwijder-pw! pw)
      (for-each (lambda (cons) (if (eq? ((car  cons) 'soort) (pw 'soort))
                                   (verwijder-tile! (cdr cons)))) pw-list))
   




    

    ; Menu

    (define levens-tile (make-tile (+ pixels-centrum-vogel cel-breedte-px) (+ pixels-centrum-vogel cel-hoogte-px)))
    ((levens-tile 'set-x!) levens-tile-x )
    ((levens-tile 'set-y!) levens-tile-y)
    ((menu-laag 'add-drawable!)levens-tile)


    (define herhaaltijd-tile  (make-tile-sequence (list (make-bitmap-tile "images/herhaaltijd.png" "images/herhaaltijd.png")
                                                  (make-bitmap-tile "images/herhaaltijd2.png" "images/herhaaltijd2-mask.png"))))
    ((herhaaltijd-tile  'set-x!) herhaaltijd-tile-x)
    ((herhaaltijd-tile  'set-y!)  herhaaltijd-tile-y)

    (define (teken-herhaaltijd)
      ((menu-laag 'add-drawable!)herhaaltijd-tile))
    

    (define pw-tile  (make-tile-sequence (list (make-tile 1 1)
                                               (make-bitmap-tile "images/bevriezing2.png" "images/bevriezing2-mask.png"))))

    ((pw-tile  'set-x!) pw-tile-x)
    ((pw-tile  'set-y!) pw-tile-y)

     (define pw-tile2  (make-tile-sequence (list (make-tile 1 1)
                                               (make-bitmap-tile "images/chaos2.png" "images/chaos2-mask.png"))))

    ((pw-tile2  'set-x!) (+ cel-breedte-px pw-tile-x))
    ((pw-tile2  'set-y!) pw-tile-y)


     (define (teken-pw)
       ((menu-laag 'add-drawable!)pw-tile2)
      ((menu-laag 'add-drawable!)pw-tile))

    (define score-tile (make-tile (+ pixels-centrum-vogel cel-breedte-px) (+ pixels-centrum-vogel cel-hoogte-px)))
    ((score-tile 'set-x!) score-tile-x)
    ((score-tile 'set-y!) score-tile-Y)
    ((menu-laag 'add-drawable!)score-tile)

    
    (define level-tile (make-tile (+ pixels-centrum-vogel cel-breedte-px) (+ pixels-centrum-vogel cel-hoogte-px)))
    ((level-tile 'set-x!) level-tile-x)
    ((level-tile 'set-y!) level-tile-Y)
    ((menu-laag 'add-drawable!) level-tile)

     (define ronde-tile (make-tile (+ pixels-centrum-vogel cel-breedte-px) (+ pixels-centrum-vogel cel-hoogte-px)))
    ((ronde-tile 'set-x!) ronde-tile-x)
    ((ronde-tile 'set-y!) ronde-tile-Y)
    ((menu-laag 'add-drawable!) ronde-tile)

    (define kogels-tile (make-tile cel-breedte-px cel-hoogte-px))
    ((kogels-tile 'set-x!) kogel-tile-x)
    ((kogels-tile 'set-y!) kogel-tile-Y)
    ((menu-laag 'add-drawable!) kogels-tile)



    
   (define luchtdruk-tile (make-tile luchtdruk-pixels (+ pixels-centrum-vogel cel-hoogte-px)))
    ((luchtdruk-tile 'set-x!) luchtdruk-tile-x)
    ((luchtdruk-tile 'set-y!) luchtdruk-tile-Y)
    ((menu-laag 'add-drawable!)luchtdruk-tile)

    (define turbo-tile (make-tile (+ pixels-centrum-vogel cel-breedte-px pixels-centrum-vogel) (+ pixels-centrum-vogel cel-hoogte-px)))
    ((turbo-tile 'set-x!) turbo-tile-x)
    ((turbo-tile 'set-y!) turbo-tile-Y)
    ((menu-laag 'add-drawable!) turbo-tile)

    (define water-tile (make-tile (+ pixels-centrum-vogel cel-breedte-px) (+ pixels-centrum-vogel cel-hoogte-px)))
    ((water-tile 'set-x!) water-tile-x)
    ((water-tile 'set-y!) water-tile-Y)
    ((menu-laag 'add-drawable!) water-tile)

   (define net-tile (make-tile (+ pixels-centrum-vogel cel-breedte-px) (+ pixels-centrum-vogel cel-hoogte-px)))
    ((net-tile 'set-x!) net-tile-x)
    ((net-tile 'set-y!) net-tile-Y)
    ((menu-laag 'add-drawable!) net-tile)


(define (teken-wapen-text)
    ((luchtdruk-tile 'draw-text!) luchtdruk-text text-hoogte text-x text-y text-kleur)
     ((turbo-tile 'draw-text!) turbo-text text-hoogte text-x text-y text-kleur)
     ((water-tile 'draw-text!) water-text text-hoogte text-x text-y text-kleur)
     ((net-tile 'draw-text!) net-text text-hoogte text-x text-y text-kleur))




    
    (define (update-menu! levens score bool level ronde activeerd? activeerd2? kogels)

  
      ((levens-tile 'clear!))
      ((levens-tile 'draw-text!) levens-text text-hoogte text-x text-y text-kleur3)
      ((levens-tile 'draw-text!) levens text-hoogte levens-x text-y text-kleur3)

      ((score-tile 'clear!))
      ((score-tile 'draw-text!) score-text text-hoogte text-x text-y text-kleur2)
      ((score-tile 'draw-text!) score text-hoogte score-x  text-y text-kleur2)

      ((level-tile 'clear!))
      ((level-tile 'draw-text!) level-text text-hoogte text-x text-y text-kleur2)
      ((level-tile 'draw-text!) level text-hoogte score-x text-y text-kleur2)

      ((ronde-tile 'clear!))
      ((ronde-tile 'draw-text!) ronde-text text-hoogte text-x text-y text-kleur2)
      ((ronde-tile 'draw-text!) ronde text-hoogte levens-x text-y text-kleur2)

      ((kogels-tile 'clear!))
      ((kogels-tile 'draw-text!) kogels text-hoogte text-x text-y text-kleur)

      (if activeerd?
          ((pw-tile2'set-current!) 1)
          ((pw-tile2 'set-current!) 0))
      
      (if activeerd2?
          ((pw-tile 'set-current!) 1)
          ((pw-tile 'set-current!) 0))
    
      (if (not bool)
          ((herhaaltijd-tile 'set-current!) 1)
          ((herhaaltijd-tile 'set-current!) 0)))


    (define (verwijder-teken)
      ((ronde-tile 'clear!))
      ((level-tile 'clear!))
      ((levens-tile 'clear!))
      ((net-tile 'clear!))
      ((water-tile 'clear!))
      ((turbo-tile 'clear!))
      ((kogels-tile 'clear!))
      ((luchtdruk-tile 'clear!))
      ((menu-laag 'remove-drawable!) herhaaltijd-tile)
      ((menu-laag 'remove-drawable!) pw-tile)
      ((wapen-laag 'remove-drawable!) crosshair-tile))
     
    

    (define (dispatch-teken message)
      (cond
        ((eq? message 'update-crosshair!) update-crosshair!)
        ((eq? message 'teken-crosshair!) teken-crosshair!)
        ((eq? message 'vogel-tile-lijst) vogel-tile-lijst)
        ((eq? message 'verander-crosshair!) verander-crosshair!)
        ((eq? message 'teken!) teken!)
        ((eq? message 'verwijder-pw!) verwijder-pw!)
        ((eq? message 'nieuw-map) nieuw-map)
        ((eq? message 'gevlucht?) gevlucht?)
        ((eq? message 'teken-terug) teken-terug)
        ((eq? message 'update-teken!) update-teken!)
        ((eq? message 'update-menu!) update-menu!)
        ((eq? message 'nieuw-current!) nieuw-current!)
        ((eq? message 'verwijder-tile!) verwijder-tile!)
        ((eq? message 'verwijder-teken) verwijder-teken)
        ((eq? message 'verwijder-dood) verwijder-dood)
        ((eq? message 'teken-herhaaltijd) teken-herhaaltijd)
        ((eq? message 'teken-pw) teken-pw)
        ((eq? message 'teken-wapen-text) teken-wapen-text)
        ((eq? message 'verwijder-vogels) verwijder-vogels)
        ((eq? message 'set-game-loop!) set-game-loop!)
        ((eq? message 'set-teken-loop!) set-teken-loop!)
        ((eq? message 'set-mouse-click-loop!) set-mouse-click-loop!)
        ((eq? message 'vogel-tile-lijst!) vogel-tile-lijst!)
        ((eq? message 'nieuw-tile-lijst!) nieuw-tile-lijst!)
        ((eq? message 'set-key-callback!) set-key-callback!)
        ((eq? message 'vogel-tile->dood-tile!) vogel-tile->dood-tile!)
        (else (display "FOUTE MSG TEKEN"))))
    dispatch-teken))
   