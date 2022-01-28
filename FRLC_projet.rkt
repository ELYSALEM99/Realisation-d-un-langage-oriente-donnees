(require (lib "trace.ss"))

(require (lib "compat.ss"))

(require (lib "list.ss"))

(define *frames* '())
(define *frame '())
(define *slot '())
(define *facet '())
(define *value '())
(define *demons* '())
(define *variables* '())
(define *frames-list* '())
(define *frames_inst* '())
(define *frames_generic* '())

(define (fget frame slot facet)
  (map car 
       (mycdr (myassoc  facet 
                        (mycdr (myassoc slot 
                                        (mycdr (mygetprop frame 'frame))))))))

(define (mycdr l)
  (cond ((null? l) '())
        (#t (cdr l))))

(define (mygetprop s p) 
  (cond ((equal? #f (getprop s p)) '())
        (#t (getprop s p))))

(define (fassoc-slot frame cle aliste)
  (cond ((assoc  cle (cdr aliste)))
        (#t    (putprop frame 'frame (ajoute-slot cle aliste (getprop frame 'frame))) (myassoc cle (cdr (getprop frame 'frame)))
               )))


(define (fassoc-facet frame slot facet cle aliste)
  
  
  
  (cond ((null? aliste) '())
        ((assoc  cle (cdr aliste)))
        ;((
        (#t    (putprop frame 'frame (ajoute-facet  facet slot cle aliste (getprop frame 'frame))) (myassoc cle (cdr (myassoc slot (cdr (getprop frame 'frame)))))
               )))

(define (fassoc-value  frame slot facet  value cle aliste)
  
  (cond ((null? aliste) '())
        ((assoc  cle (cdr aliste)))
        (#t (putprop frame 'frame (ajoute-value  value facet  slot 
                                                 cle aliste (getprop frame 'frame)))
            ))) 

(define (rplacd l l1)
  (cons (car l) l1))

(define (rplacd2 lambda l l1)
  (set-rest! l l1) (cons (car l) l1))


(define (ajoute-slot cle aliste  l)
  (cond ((null? (cdr l)) (list (car l) (cons cle (cdr l))))
        (#t (cons (car l) (cons (cons cle (cdr aliste)) (cdr l))))))

(define (removemy s l)
  (cond ((null? l) '())
        ((equal? s (car l))(removemy s (cdr l)))
        (else (cons (car l) (removemy s (cdr l)))))) 


(define (ajoute-facet  facet slot cle aliste  l) 
  
  (cons (car l) (cons (cons  slot  (cons (list facet) (cdr aliste))) (remove (myassoc slot (cdr l)) (cdr l)))))

(define (ajoute-value value facet  slot cle aliste  l)
  ;(cons (car l)(cons (cons slot (cons (cons facet (cons (list value) (cdr aliste))) (myassoc facet (cdr aliste)))) (remove (myassoc slot (cdr l)) (cdr l))))) 
  (cons (car l)(cons (append (list slot) (cons (cons facet (cons (list value) (cdr aliste))) (myassoc facet (cdr aliste))) (liste-facets (car l)  facet slot l)  ) (remove (myassoc slot (cdr l)) (cdr l)))))  

(define (liste-facets frame facet slot l)
  
  (cond ((not (null? (mycdr (mycdr (myassoc slot (cdr l)))))) (cond ((not (member (caar (mycdr (mycdr (myassoc slot (cdr l)))))  (list-slots l)))   (mycdr (cdr (assoc slot (cdr l)))))
                                                                    (#t '()))) 
        
        (#t '())))


(define (list-slots l)
  (map (lambda ( ls)(car ls)  )  (cdr  (fgetframe (car l))))) 

(define (myassoc cle aliste)
  (cond ((equal? #f (assoc cle aliste)) '())
        ((assoc cle  aliste))))

(define (fput frame slot facet valeur)
  (cond ((equal? valeur (mycar (fget frame slot facet))) #f)
        (#t (set! *frames-list* (append *frames-list* (list frame)))  (fassoc-value frame slot facet valeur valeur
                                                                                (fassoc-facet frame slot  facet facet
                                                                                              (fassoc-slot frame slot
                                                                                                           (fgetframe frame)))) valeur)  ))  

(define (mycar l)(cond ((null? l) '())
                       (#t (car l))))

(define fgetframe (lambda (frame) 
                    (cond ((getprop frame 'frame))
                          (#t (set! *frames* (append *frames* (list frame))) 
                              (putprop frame  'frame (list frame))))
                    (getprop frame 'frame) )) 

(define last (lambda (l)
               (cond ( (not (null? l)) (list (car (reverse l))))
                     (#t '()))))





(define (fcreate frame name)
  (set! *frames_generic* (append *frames_generic* (list frame)))
  (fput name 'ako 'valeur frame)
  (fput name 'classification 'valeur 'instance)
  (fput name 'age 'ifneeded 'ask)
  (fput name 'travail 'ifneeded 'ask)
  (fput name 'marié 'ifneeded 'ask)
  (fput name 'vie 'ifneeded 'ask)
  
  )

(define (finst frame name)
  (set! *frames_generic* (append *frames_generic* (list frame)))
  (fcreate frame name) 
  (set! *frame name)
  (map(lambda(e)
        (map (lambda (slot)
        (cond((not (null?(fget e slot 'valeur))))
             (#t (cond((not (null? (fget e slot 'defaut))))
                      (#t (cond((not (null? (fget e slot 'ifadded))))
                               (#t (printf"(~a)~a: " *frame slot) (set! *slot slot)(apply(eval (mycar (fget e slot 'ifneeded))) '()))))))))
               (fslot e))
        )
      (fgetclasses frame)))

(define (fput+ frame slot facet value)
  (fput frame slot facet value)
  (set! *frame frame)
  (set! *slot slot)
  (set! *facet facet)
  (set! *value value)
  (map(lambda(e)
       (cond((not (null? (fget e slot 'ifadded)))
             (apply (eval (mycar (fget e slot 'ifadded))) '()))))
      (fgetclasses frame)))


(define (fgetclasses frame)
  (cond((null? frame) '())
       ((equal? frame 'objet) '(objet))
      (#t (cons frame (fgetclasses(mycar(fget frame 'ako 'valeur)))))))  

(define(fslot frame)
  (map car(mycdr (mygetprop frame 'frame))))

(define (ffacet frame slot)
(map car (mycdr (myassoc slot (mycdr (mygetprop frame 'frame))))))


(define (fremove frame slot facet valeur)
  (cond((null? (fget frame slot facet)) '())
       ((not(equal? valeur  (mycar (fget frame slot facet)))) '())
       ( (fput frame slot facet ()) #t)))

(define (ask)
  (cond((not(null?(fget *frame *slot '=))))
       (#t (fput *frame *slot '=(read)))))

(define (ajouter)
  (finst *frame *value)
  )


(define (fget-i frame slot)
  (define *valeur-i '())
  (map (lambda (e)
               (cond((not (null?(fget e slot 'valeur))) (set! *valeur-i (append *valeur-i (list (fget e slot 'valeur)))) #t)
                    ('())))
               (fgetclasses frame))
  (cond((null? *valeur-i ) '())
       (*valeur-i))
  )
(define (fget-n frame slot)
  (define *valeur-n '())
  (map(lambda(facet)
        (map (lambda (e)
               (cond((not (null?(fget e slot facet))) (set! *valeur-n (append *valeur-n (list (fget e slot facet)))) #t)
                    ('())))
               (fgetclasses frame))
        )
      '(valeur defaut ifneeded))
  
  (cond((null? *valeur-n ) '())
       (*valeur-n))  
  )

(define (fget-z frame slot)
  (define *valeur-z '())
  (map(lambda(e)
        (map (lambda (facet)
               (cond((not (null?(fget e slot facet))) (set! *valeur-z (append *valeur-z (list (fget e slot facet)))) #t)
                    ('())))
               '(valeur defaut ifneeded))
        )
      (fgetclasses frame))
  (cond((null? *valeur-z ) '())
       (*valeur-z))
  )

(define (fgename frame)
  (define frames-nb 0)
  (cond ((equal? #f (member frame *frames*)) '())
        (#t (for-each(lambda(e)
                       (cond((equal? e frame) (set! frames-nb(+ frames-nb 1)))
                            (#t))
                       )*frames-list*) (printf"~a_~a"frame frames-nb))
        )
  )

(define(fchildren frame slot)
  (cond((null? (ffacet frame slot)) '())
       ((< 1 (length (fgetclasses frame ))) (car (cdr (fgetclasses frame))))
       (#t '())))

(define(Frame frame)
  (cond((null? (Frames? frame)) '())
       (#t   (fgetframe frame)))
  )

(define(Frames? frame)
  (cond((equal? #f (member frame *frames*)) '())
        ( #t))
       )

(define (fname frame)
  (cond((equal? #f (member frame *frames*)) '())
        ( frame))
       )

(define (fnames? frame)
  (cond((null? (fname frame) ) '())
       (#t))
  )

(define (fako? frame1 frame2)
  (cond ((< (length (fgetclasses frame2)) (length (fgetclasses frame1))) (cond((equal? (list frame1 frame2) (list (list-ref (fgetclasses frame1)0) (list-ref (fgetclasses frame1)1))) #t) 
                                                                              ('()))  )
        ((< (length (fgetclasses frame1)) (length (fgetclasses frame2))) (cond((equal? (list frame2 frame1) (list (list-ref (fgetclasses frame2)0) (list-ref (fgetclasses frame2)1))) #t) 
                                                                              ('()))  )
        ('()))
  
  )

(define (finstance? frame)
  (cond((= 1 (length (fgetclasses frame))) '())
        (#t))
  )

(define (fgeneric? frame)
  (define frames-generic 0)
  (for-each(lambda(e)
             (cond((equal? (fget e 'ako 'valeur) '()) '())
                  ((equal? (car(fget e 'ako 'valeur)) frame) (set! frames-generic(+ frames-generic 1))))
             )*frames*)
  (cond((equal? 0 frames-generic ) '())
       (#t))
  )

(define (naissance enfant mere )
  (cond((equal? mere (fchildren enfant 'ako)) (printf"Impossible! L’enfant qui naît existe déjà") )
       ((equal? #f (member mere *frames*)) (printf"Impossible! La mére n'existe pas") )
       ((equal? '(non) (fget mere 'marié (car (ffacet mere 'marié))) )  (printf"Impossible! la mére elle n'est pas mariée"))
       ((equal? mere (fchildren (fchildren enfant 'ako) 'ako)) (printf"Impossible! la mère est sa grand-mère"))
       ((< 10 (nombreEnfant mere)) (printf"Impossible! le nombre d’enfants est supérieur à 10"))
       (#t (finst mere enfant)))
  )

(define (nombreEnfant mere)
  (define enfant-nb 0)
  (for-each(lambda(e)
             (cond((equal? #t (fako_children? mere e) ) (set! enfant-nb(+ enfant-nb 1)))
                  (#t))
             )*frames*)
enfant-nb)

(define (fako_children? frame1 frame2)
  (cond ((< (length (fgetclasses frame1)) (length (fgetclasses frame2))) (cond((equal? (list frame2 frame1) (list (list-ref (fgetclasses frame2)0) (list-ref (fgetclasses frame2)1))) #t) 
                                                                              ('()))  )
        ('()))
  
  )

(define (mariage m_homme m_femme)
   (cond((equal? '(mort) (fget m_homme 'vie (car (ffacet m_homme 'vie)))) (printf"Impossible! L’homme est mort" ))
        ((equal? '(mort) (fget m_femme 'vie (car (ffacet m_femme 'vie)))) (printf"Impossible! La femme est morte"))
        ((equal? '(oui) (fget m_homme 'marié (car (ffacet m_homme 'marié)))) (printf"Impossible! l'homme est déjà marié"))
        ((equal? '(oui) (fget m_femme 'marié (car (ffacet m_femme 'marié)))) (printf"Impossible! la femme est déjà mariée"))
        ((fput m_homme 'marié (car (ffacet m_homme 'marié)) 'oui) (fput m_femme 'marié (car (ffacet m_femme 'marié)) 'oui) #t)))

(define (gestionTravail frame)
  (cond( (member 'homme (fgetclasses frame)) (fget frame 'travail (car(ffacet frame 'travail))))
       (#t (cond((<= 1 (nombreEnfant frame)) (printf"oui"))
                (#t (printf"aucun")))))
  )

;(fput 'homme 'vie 'defaut 'mort)
(fput 'homme 'ako 'valeur 'objet)
(fput 'homme 'mere 'defaut 'inconnue)
(fput 'homme 'age 'ifneeded 'ask)
(fput 'homme 'marié 'ifneeded 'ask)
(fput 'homme 'travail 'ifneeded 'ask)
(fput 'homme 'enfant 'ifadded 'ajouter)
(fput 'homme 'vie 'ifneeded 'ask)


;(finst 'med 'titi)
;(fput+ 'med 'enfant 'valeur 'toto)
   
;(fput 'femme 'vie 'defaut 'vivant)
(fput 'femme 'ako 'valeur 'objet)
(fput 'femme 'mere 'defaut 'inconnue)
(fput 'femme 'travail 'defaut 'aucun)
(fput 'femme 'age 'ifneeded 'ask)
(fput 'femme 'marié 'ifneeded 'ask)
(fput 'femme 'enfant 'ifadded 'ajouter)
(fput 'femme 'vie 'ifneeded 'ask)
(finst 'homme 'henry)
(finst 'femme 'mari)