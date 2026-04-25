;; ======================================================================
;; PRÀCTICA FINAL LLENGUATGES DE PROGRAMACIÓ - LISP - PAINTBALL
;; ======================================================================
;; Estudiants: ABC, XYZ
;; Data: 25/04/2026
;; Assignatura: Llenguatges de Programació (LP)
;; Grup: <Indicar Grup>
;; Professors: <Indicar Professors>
;; Convocatòria: Primera Convocatòria (Ordinària)
;;
;; ----------------------------------------------------------------------
;; FITXER: funciones_auxiliares.lsp
;; DESCRIPCIÓ: Conté funcions geomètriques, de gestió de matrius i 
;; utilitats de sistema necessàries per al funcionament del projecte.
;; ----------------------------------------------------------------------

;; ======================================================================
;; GESTIÓ DE MATRIUS (Estat del joc)
;; ======================================================================

(defun indexa-fila (f p)
  "Retorna l'element a la posició p d'una fila f."
  (cond ((null f) nil)
        ((= p 0) (car f))
        (t (indexa-fila (cdr f) (- p 1)))))

(defun indexa-matriu (m f c)
  "Retorna la casella a la fila f i columna c de la matriu m."
  (cond ((null m) nil)
        ((= f 0) (indexa-fila (car m) c))
        (t (indexa-matriu (cdr m) (- f 1) c))))

(defun posa-dins-fila (f c x)
  "Retorna una nova fila amb l'element x a la posició c."
  (cond ((null f) nil)
        ((= c 0) (cons x (cdr f)))
        (t (cons (car f) (posa-dins-fila (cdr f) (- c 1) x)))))

(defun posa-dins-matriu (m f c x)
  "Retorna una nova matriu amb l'element x a la fila f i columna c."
  (cond ((null m) nil)
        ((= f 0) (cons (posa-dins-fila (car m) c x) (cdr m)))
        (t (cons (car m) (posa-dins-matriu (cdr m) (- f 1) c x)))))


;; ======================================================================
;; FUNCIONS GEOMÈTRIQUES I DE COLOR
;; ======================================================================

(defun quadrat (l)
  "Dibuixa el contorn d'un quadrat de costat l."
  (drawrel l 0)
  (drawrel 0 l)
  (drawrel (- l) 0)
  (drawrel 0 (- l)))

(defun omple-linies (amplada files-restants)
  "Auxiliar per a quadrat-ple: dibuixa línies horitzontals."
  (cond ((< files-restants 0) nil)
        (t
         (drawrel amplada 0)
         (moverel (- amplada) 1)
         (omple-linies amplada (- files-restants 1)))))

(defun quadrat-ple (l)
  "Dibuixa un quadrat omplert de costat l."
  (omple-linies l l)
  (moverel 0 (- l)))

(defun triangle (base altura)
  "Dibuixa un triangle de base i altura indicades, tornant a l'origen."
  (let ((meitat (round (/ base 2.0))))
    (moverel 0 altura)
    (drawrel meitat (- altura))
    (drawrel (- base meitat) altura)
    (drawrel (- base) 0)
    (moverel 0 (- altura))))

(defun radians (graus)
  "Converteix graus a radians."
  (/ (* graus (* 2 pi)) 360.0))

;; Funcions de paleta de colors
(defun RED () (color 255 0 0))
(defun CYAN () (color 0 255 255))
(defun GREEN () (color 0 255 0))
(defun BLUE () (color 0 0 255))
(defun BLACK () (color 0 0 0))
(defun PINK () (color 255 0 255))


;; ======================================================================
;; UTILITATS DE SISTEMA
;; ======================================================================
