(defun canvia (n l x)
        (cond ((null l) nil)
        ((= n 1) (cons x (cdr l)))
        (t (cons (car l) (canvia (- n 1) (cdr l) x)))
        )
)

(defun quadrat (l)
    (drawrel l 0)
    (drawrel 0 l)
    (drawrel (- l) 0)
    (drawrel 0 (- l))
)

(defun quadrat-ple (l)
  "Dibuixa un quadrat omplert de mida 'l'."
  (omple-linies l l)
  (moverel 0 (- l)))

(defun omple-linies (amplada files-restants)
  "Funció recursiva que dibuixa línies horitzontals per omplir el quadrat."
  (cond ((< files-restants 0) nil) ; Cas base: ja no queden línies per dibuixar
        (t
         (drawrel amplada 0)       ; Dibuixam la línia horitzontal cap a la dreta
         (moverel (- amplada) 1)   ; Tornam el cursor a l'esquerra i baixam 1 píxel
         (omple-linies amplada (- files-restants 1))))) ; Crida recursiva per a la següent línia

(defun RED ()
        (color 255 0 0)
)

(defun CYAN ()
        (color 0 255 255)
)

(defun GREEN ()
        (color 0 255 0)
)

(defun BLUE ()
        (color 0 0 255)
)

(defun BLACK()
        (color 0 0 0)
)

(defun PINK () 
    (color 255 0 255)
)

(defun quadrats (n l d)
    (cond ((= 0 n))
          (t 
             (moverel l d)
             (quadrat m)
             (quadrats (- n 1) l d) 
             )
    )
)

(defun dreta(p)
        (cond ((= (mod (+ p 1) 5) 0) 0)
              (t (+ p 1))
        )
)

(defun esquerra (p)
  (mod (+ p 4) 5)
  
)

(defun pinta (l p)
  (cls)
  (move xi yi) 
  (pinta_recursiva l p)
  (BLACK)
)



(defun pinta_recursiva (l p)

        (cond ((null l) nil)
              ((= 1 (car l)) 
               (RED)
               (quadrat (- m 1))
               (cond ((= p 0)
                      (CYAN)
                      (moverel g g)
                      (quadrat (- m (* g 2)))
                      (moverel (- g) (- g))
                      )
               )
               (moverel m 0)
               (pinta_recursiva (cdr l) (- p 1)))
               ((= 2 (car l)) 
               (GREEN)
               (quadrat (- m 1))
               (cond ((= p 0)
                      (CYAN)
                      (moverel g g)
                      (quadrat (- m (* g 2)))
                      (moverel (- g) (- g))
                      )
               )
               (moverel m 0)
               (pinta_recursiva (cdr l) (- p 1)))
               ((= 3 (car l)) 
               (BLUE)
               (quadrat (- m 1))
               (cond ((= p 0)
                      (CYAN)
                      (moverel g g)
                      (quadrat (- m (* g 2)))
                      (moverel (- g) (- g))
                      )
               )
               (moverel m 0)
               (pinta_recursiva (cdr l) (- p 1)))
               (t 
               (BLACK)
               (quadrat (- m 1))
               (cond ((= p 0)
                      (CYAN)
                      (moverel g g)
                      (quadrat (- m (* g 2)))
                      (moverel (- g) (- g))
                      )
               )
               (moverel m 0)
               (pinta_recursiva (cdr l) (- p 1)))
        )
)

(defun passa(l p)
        
        
        (cond   ((every 'plusp l) '"FIN")
                (t 
                 (let ((codigo (get-key)))
                   (cond 
                     ((= codigo 97) 
                      (pinta l (esquerra p))
                      (passa l (esquerra p)))
                     ((= codigo 100)
                      (pinta l (dreta p))
                      (passa l (dreta p)))
                     ((= codigo 49)
                      (pinta (canvia (+ 1 p) l 1) p)
                      (passa (canvia (+ 1 p) l 1) p))
                     ((= codigo 50)
                      (pinta (canvia (+ 1 p) l 2) p)
                      (passa (canvia (+ 1 p) l 2) p))
                     ((= codigo 51)
                      (pinta (canvia (+ 1 p) l 3) p)
                      (passa (canvia (+ 1 p) l 3) p))
                     ((= codigo 27) '"FIN")
                     (t (passa l p))))))
        
)

(defun aplanar (l) 
    (cond ((null l) l)
          ((atom (car l)) (cons (car l) (aplanar (cdr l))))  
                                                              
          (t (append (aplanar (car l)) (aplanar (cdr l))))         

    )
)


(defun esborra (x l)
    (cond ((null l) l)
          ((equal x (car l)) (esborra x (cdr l)))
          (t (cons (car l) (esborra x (cdr l))))
    )
)


;EXERCICI 3

(defun esborranprimers (n x l)
    (cond ((< n 1) l)
          ((equal x (car l)) (esborranprimers (- n 1) x (cdr l)))
          (t (cons (car l) (esborranprimers n x (cdr l))))
    )


)


(defun deixanprimers (n x l)
    (cond ((and (< n 1) (equal x (car l))) (esborra x (cdr l)))
          ((equal x (car l)) (cons (car l) (deixanprimers (- n 1) x (cdr l))))
          (t (cons (car l) (deixanprimers n x (cdr l))))  
    )


)

(defun rdc (l)
    (cond ((null (cdr l)) nil)
          (t (cons (car l) (rcd (cdr l))))
    )


)


(defun snoc (a l)
    (cond ((null l) (list a))
          (t (cons (car l) (snoc a (cdr l))))
    )

)

(defun escala (x l)
    (cond ((null l) nil)
         (t (cons (* x (car l)) (escala x (cdr l))))
    )

)

(defun maxim (l)
    (cond ((null (cdr l)) (car l)) 
          ((> (car l) (car (cdr l))) (maxim (cons (car l) (cdr (cdr l))))) 
          (t (maxim (cdr l))) 
    )
)

(defun minim (l)
    (cond ((null (cdr l)) (car l)) 
          ((< (car l) (car (cdr l))) (minim (cons (car l) (cdr (cdr l))))) 
          (t (minim (cdr l))) 
    )
)


(defun inverteix (l)
    (cond ((null (cdr l)) l)
          (t (snoc (car l) (inverteix (cdr l))))
    )

)

(defun esborrap (x l)
    (cond ((= x 1) (cdr l))
          (t (cons (car l) (esborrap (- x 1) (cdr l))))

    )


)

(defun vegades (x l)
    (cond ((null (cdr l)) 0)
          ((equal (car l) x) (+ 1 (vegades x (cdr l))))
          (t (vegades x (cdr l)))
    
    
    )
)

(defun atoms (l)
    (cond ((null l) 0)
          ((listp (car l)) (+ (atoms (car l)) (atoms (cdr l))))
          (t (+ 1 (atoms (cdr l))))

    )

)


(defun insereix-esquerra (i x l)
    (cond ((null l) nil)
          ((equal i (car l)) (cons x l))
          (t (cons (car l) (insereix-esquerra i x (cdr l))))
    
    )
)

(defun insereix-dreta (i x l)
    (cond ((null l) nil)
          ((equal i (car l)) (append (list i x) (cdr l)))
          (t (cons (car l) (insereix-dreta i x (cdr l))))
    
    )
)

(defun quadrat (l)
    (drawrel l 0)
    (drawrel 0 l)
    (drawrel (- l) 0)
    (drawrel 0 (- l))
)

(defun quadrats (n l d)
    (cond ((= 0 n))
          (t 
             (moverel l d)
             (quadrat 40)
             (quadrats (- n 1) l d) 
             )
    )
)

(defun esborra-si (c l)
    (cond ((null l) nil)
          ((funcall c (car l)) (esborra-si c (cdr l)))
          (t (cons (car l) (esborra-si c (cdr l))))
    
    
    )
)

(defun suma1 (n)
    (+ n 1)
)

(defun meu-mapcar (f l)
    (cond ((null l) nil)
          (t (cons (funcall f (car l)) (meu-mapcar f (cdr l)))) 
    
    )
)

(setq fila '( (a 1) (b 2) (c 3) ))

(defun indexa-fila (f p)

    (cond ((null f) nil)
          ((= p 0) (car f))
          (t  (indexa-fila (cdr f) (- p 1)))
        
    )

)

(setq matriu '(((a 1) (b 2) (c 3))
                ((d 4) (e 5) (f 6))
                ((g 7) (h 8) (i 9))))

(defun indexa-matriu (m f c)
    (cond ((null m) nil)
          ((= f 0) (indexa-fila (car m) c))
          (t (indexa-matriu (cdr m) (- f 1) c))
    )

)

(defun canvia (n l x)
        (cond ((null l) nil)
        ((= n 1) (cons x (cdr l)))
        (t (cons (car l) (canvia (- n 1) (cdr l) x)))
        )
)

(defun posa-dins-fila (f c x)
    (cond ((null f) nil)
          ((= c 0) (cons x (cdr f)))
          (t (cons (car f) (posa-dins-fila (cdr f) (- c 1) x)))
        
    )

)

(defun posa-dins-matriu (m f c x)
    (cond ((null m) nil)
          ((= f 0) (cons (posa-dins-fila (car m) c x) (cdr m)))
          (t (cons (car m) (posa-dins-matriu (cdr m) (- f 1) c x)))
    )
)

(defun troba-dins-fila (f v &optional (x 0))

    (cond ((null f) nil)
          ((member v (car f)) x)
          (t (troba-dins-fila (cdr f) valor (+ x 1)))
    )
)


(defun pescalar (l1 l2)
    (cond ((null l1) 0)
          (t (+ (* (car l1) (car l2)) (pescalar (cdr l1) (cdr l2))))
    )
)

;; ======================================================================
;; FUNCIONS DE CERCLE (Adaptades a posició relativa)
;; ======================================================================

(defun radians (graus)
  (/ (* graus (* 2 pi)) 360.0))

(defun cercle (l segments)
  "Dibuixa un cercle de diàmetre 'l' a la posició actual i retorna el cursor al lloc."
  (let ((radi (/ l 2.0)))
    ;; 1. Movem el llapis a la dreta del tot per començar a dibuixar (angle 0)
    (moverel (round (* 2 radi)) (round radi))
    ;; 2. Cridem a la teva funció recursiva passant-li el (x,y) relatiu actual
    (cercle2 radi (/ 360.0 segments) 0 (* 2 radi) radi)
    ;; 3. Tornam el cursor a l'origen per no rompre la quadrícula del mapa
    (moverel (- (round (* 2 radi))) (- (round radi)))))

(defun cercle2 (radi pas angle x-actual y-actual)
  "Funció recursiva que dibuixa els segments del cercle."
  (cond ((< angle 360)
         (let* ((nou-x (+ radi (* radi (cos (radians (+ angle pas))))))
                (nou-y (+ radi (* radi (sin (radians (+ angle pas))))))
                ;; Calculem la diferència (quant ens hem de moure des del punt anterior)
                (dx (- nou-x x-actual))
                (dy (- nou-y y-actual)))
           ;; Dibuixem només el desplaçament
           (drawrel (round dx) (round dy))
           ;; Crida recursiva amb el nou angle i la nova posició
           (cercle2 radi pas (+ angle pas) nou-x nou-y)))
        (t t)))

(defun triangle (base altura)
  "Dibuixa un triangle relatiu cap amunt i torna a l'origen."
  (let ((meitat (round (/ base 2.0))))
    ;; Baixem a la cantonada inferior esquerra del triangle
    (moverel 0 altura)
    ;; Pugem dibuixant fins a la punta superior central
    (drawrel meitat (- altura))
    ;; Baixem dibuixant fins a la cantonada inferior dreta
    (drawrel (- base meitat) altura)
    ;; Dibuixem la línia horitzontal de sota per tancar-lo
    (drawrel (- base) 0)
    ;; Desfem el primer moviment per tornar a l'origen de la casella
    (moverel 0 (- altura))))

(defun sleep (seconds)
    "Espera la quantitat indicada de segons"
    ; Això és un bucle iteratiu. NO PODEU FER-LO SERVIR ENLLOC MÉS
    (do ((endtime (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
        ((> (get-internal-real-time) endtime))))
    