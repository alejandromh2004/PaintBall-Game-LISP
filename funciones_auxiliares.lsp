;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: Alejandro Martinez Hermosa, Javier Vivo Samaniego.
;; Professor: Miquel Cabot.
;; Lliurament: primera convocatòria.
;; Funcions auxiliars del programa.

;; GESTIÓ DE MATRIUS (Estat del joc)

;; Retorna l'element a la posició p d'una fila f
(defun indexa-fila (f p)
  (cond ((null f) nil)
        ((= p 0) (car f))
        (t (indexa-fila (cdr f) (- p 1)))))

;; Retorna la casella a la fila f i columna c de la matriu m
(defun indexa-matriu (m f c)
  (cond ((null m) nil)
        ((= f 0) (indexa-fila (car m) c))
        (t (indexa-matriu (cdr m) (- f 1) c))))

;; Retorna una nova fila amb l'element x a la posició c
(defun posa-dins-fila (f c x)
  (cond ((null f) nil)
        ((= c 0) (cons x (cdr f)))
        (t (cons (car f) (posa-dins-fila (cdr f) (- c 1) x)))))

;; Retorna una nova matriu amb l'element x a la fila f i columna c
(defun posa-dins-matriu (m f c x)
  (cond ((null m) nil)
        ((= f 0) (cons (posa-dins-fila (car m) c x) (cdr m)))
        (t (cons (car m) (posa-dins-matriu (cdr m) (- f 1) c x)))))


;; ======================================================================
;; FUNCIONS GEOMÈTRIQUES I DE COLOR
;; ======================================================================

;; Dibuixa el contorn d'un quadrat de costat l
(defun quadrat (l)
  (drawrel l 0)
  (drawrel 0 l)
  (drawrel (- l) 0)
  (drawrel 0 (- l)))

;; Auxiliar per a quadrat-ple: dibuixa línies horitzontals
(defun omple-linies (amplada files-restants)
  (cond ((< files-restants 0) nil)
        (t
         (drawrel amplada 0)
         (moverel (- amplada) 1)
         (omple-linies amplada (- files-restants 1)))))

;; Dibuixa un quadrat omplert de costat l
(defun quadrat-ple (l)
  (omple-linies l l)
  (moverel 0 (- l)))

;; Dibuixa un triangle de base i altura indicades, tornant a l'origen
(defun triangle (base altura)
  (let ((meitat (round (/ base 2.0))))
    (moverel 0 altura)
    (drawrel meitat (- altura))
    (drawrel (- base meitat) altura)
    (drawrel (- base) 0)
    (moverel 0 (- altura))))

;; Dibuixa un triangle omplert de base i altura indicades
(defun triangle-ple (base altura)
  (let ((meitat (round (/ base 2.0))))
    (omple-tri base altura 0)
    (moverel 0 (- altura))))

;; Auxiliar per a triangle-ple: omple el triangle per files
(defun omple-tri (b h pas)
  (cond ((>= pas h) nil)
        (t (let ((w (round (* b (/ (- h pas) (float h))))))
             (moverel (round (/ (- b w) 2.0)) 0)
             (drawrel w 0)
             (moverel (- (+ (round (/ (- b w) 2.0)) w)) 1)
             (omple-tri b h (+ pas 1))))))

;; Converteix graus a radians
(defun radians (graus)
  (/ (* graus (* 2 pi)) 360.0))

;; Funcions de paleta de colors
(defun RED () (color 255 0 0))
(defun CYAN () (color 0 255 255))
(defun GREEN () (color 0 255 0))
(defun BLUE () (color 0 0 255))
(defun BLACK () (color 0 0 0))
(defun PINK () (color 255 0 255))

;; Calcula la distància euclidiana al quadrat entre dos punts (ax, ay) i (bx, by)
(defun distancia-quadrada (ax ay bx by)
  (+ (* (- ax bx) (- ax bx))
     (* (- ay by) (- ay by))))

