;;;; Liste - Plist - Database - Record - User Interaction - Default value - Cicli (loop) 

; Definisco variabile globale
(defvar *db* nil)

; Creazione lista semplice
(defun simpleList () 
  (list 3 2 4))

; Creazione Plist
(defun simplePlist ()
  (list :a 3 :b 2 :c 4 :ff 35))

; Get value from plist
(defun getPlist ()
  (getf (list :a 3 :b 2 :c 4 :ff 33) :c))

; Get value from plist already exist
(defun getPlist_1 ()
  (getf (simplePlist) :ff))

; Creo una lista con i parametri passati
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

; Inserisco record nel database
(defun add-record (cd)
  (push cd *db*))

;;;================================================================
;;;; Print Format ;;;
;;;================================================================

; Permette di visualizzare il contenuto del db in modo molto più leggibile
; mettendo tutti gli attributi uno sotto l'altro. (tipo TABELLA)
(defun dump-cd ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd))) ;Format t sta per vista in forma tabellare.

; ???? ____ ????
; Dovrebbe stampare la lista passata in forma tabellare
; come il metodo precedente
(defun dump-list (list)
  (format t "~{~a:~10t~a~%~}~%" list))

; Viene stampato come si scrive a parole il numero passato come parametro
(defun word-number (number)
  (format nil "~r" number))


;;;================================================================
;;;; User Interacion ;;;
;;;================================================================

; Legge il valore inserito dal promt
;; Chiede un valore
;; Lo mostra e ne aspetta un altro
;; Stampa il valore inserito.
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt) ;"prompt: " - query-io è una varibile globale
  (force-output *query-io*) ;aspetta che inserisca qualcosa
  (read-line *query-io*)) ;legge quello che si è inserito

; Crea una lista cd con i valori che l'utente passerà dal promt 
; in realtime.
(defun prompt-for-cd ()
  (make-cd 
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0) ;Se il valore passato non è un intero inserisce per default 0.
   (or (y-or-n-p (prompt-read "Ripped [y/n]") :just-allowed t) "n"))) ;Accetta solo valori y, Y, n, N, come default 0.

; Aggiunge n tracce al cd
; Il numero di tracce è determinato dall'utente in realtime
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]:"))
            (return))))

;;;================================================================
;;;; Saving and Loading the Database ;;;
;;;================================================================

; Path Database
(defun path-db () 
 (print "/Users/MarcoVelluto/Documents/Lisp/my-cds.db"))

; Scrive il db sul path passato
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
   
    (with-standard-io-syntax
      (print *db* out))))
      ;(dump-cd) out)))
 
; Carica il db dal path passato
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax 
      (setf *db* (read in)))))


;;;================================================================
;;;; Querying the Database ;;;
;;;================================================================

; Visualizza il record dall'ARTISTA
(defun select-by-artist (artist)
  (remove-if-not 
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

; ???____???
(defun select-by-artist-2 (artist)
  (dump-list 
   (select-by-artist artist)))

; Visualizza il recorda dal TITOLO
(defun select-by-title (title)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :title) title))
   *db*))











