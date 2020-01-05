;;;; Cristian_Ferrari_810932  Progetto LISP "Da regex a NFA"


;;; ---------------------------------------------------------------------

;;; is-regexp/1
;;; True quando 're' è un'espressione regolare
;;; Una Sexp è considerata un'espressione regolare nel caso in cui il suo 
;;; primo elemento sia diverso da un operatore riservato

(defun is-regexp (re)
  (cond ((null re) nil) 
        ((atom re) T)                   
        ((is-operator-mult (first re))   ;controllo se operatore= seq o or
         (if (>= (length re) 2)          ;controllo n° argomenti or/seq 
             (is-regexp (rest re))       ;accettato caso degenere
           nil)
         )                               
        ((is-operator-sing (first re))
         (if (eql (length re) 2)
           (is-regexp (rest re))
           nil)
         )
        (T  (if (not (null (rest re)))
                (is-regexp (rest re))
              T))))

;;; ---------------------------------------------------------------------------
;;; nfa-regexp-comp/1
;;; Ritorna una lista contenente l'automa generato dall'espressione regolare 're'
;;; Ritorna falso se 're' non è una regex valida
;;; Resetta gensym-counter e crea stato iniziale e finale

(defun nfa-regexp-comp (re)
  (let* ((*gensym-counter* 2))       ;resetta *gensym-counter* (numero crescente stati)
    (if (is-regexp re)
        (append '((START 0))
                (nfa-comp re 0 1)
                '((FINAL 1))
                )                   
      nil)
    )
  )

;;; nfa-comp/3
;;; smista i vari casi ai rispettivi 'handler' in base al loro operatore
;;; gestisce caso base (atom re) e (null re)

(defun nfa-comp (re start final)    
  (cond ((atom re)
         (list (list 'DELTA start re final)))
        ((null re)
         (list (list 'DELTA start 'epsilon final)))
        (T 
         (cond ((is-operator (first re))
                (cond ((eql (first re) 'seq)
                       (seq-handler (rest re) start final))
                      ((eql (first re) 'or)
                       (or-handler (rest re) start final))
                      ((eql (first re) 'star)
                       (star-handler (rest re) start final))
                      ((eql (first re) 'plus)
                       (plus-handler (rest re) start final))
                      )
                )
               (T (list (list 'DELTA start re final)))
               )
         )
        )
  )

;;;seq-handler/3
;;;Gestisce la creazione in caso di operatore sequence
          
(defun seq-handler (re start final)
  (cond ((atom (first re))           ;se first è un simbolo
         (cond ((null (rest re))     ;se ho solo 1 elemento rimanente
                (list (list 'DELTA start (first re) final)))
               (T                    ;N elementi
                (append (seq-handler (list (first re)) start (gensympp))
                        (list (list 'DELTA *gensym-counter* 'epsilon (gensympp)))
                        (seq-handler  (rest re) *gensym-counter* final))
                )
               )
         )
        ((listp (first re))                                 ;se first è una sotto-lista
         (cond ((is-operator (first (first re)))            ;CASO Operatore
                (if (null (rest re))                        ;singolo
                     (nfa-comp (first re) start final)
                  (let ((temp (+ 1 *gensym-counter*)))
                    (append (nfa-comp  (first re) *gensym-counter* (gensympp)) ;piu elementi
                            (list (list 'DELTA temp 'epsilon (gensympp)))
                            (seq-handler (rest re) *gensym-counter* final))
                    )
                  )
                )

               (T (if (null (rest re))                      ;CASO Sexp
                      (list (list 'DELTA start (first re) final))
                    (append (seq-handler (list (first re)) start (gensympp))
                            (list (list 'DELTA *gensym-counter* 'epsilon (gensympp)))
                            (seq-handler  (rest re) *gensym-counter* final)))
                  )
               )
         )
        )
)

;;;or-handler/3
;;;Gestisce la creazione dell'automa in caso di operatore 'or'

(defun or-handler (re start final)
    (cond ((atom (first re))
           (cond ((null (rest re))                   ;;Elemento Singolo
                  (append (list (list 'DELTA start 'epsilon (gensympp)))
                          (list (list 'DELTA *gensym-counter* (first re) (gensympp)))
                          (list (list 'DELTA *gensym-counter* 'epsilon final)))
                  )
                 (T                                  ;; N elementi
                  (append (or-handler (list (first re)) start final)
                          (or-handler (rest re) start final))
                  )
                 )
           )
           ((listp (first re))
            (cond ((is-operator (first (first re)))    ;;CASO OPERATORE
                   (if (null (rest re))                 ;;Ultimo elemento restante
                       (let ((temp (+ 2 *gensym-counter*)))
                         (append (list (list 'DELTA start 'epsilon (gensympp)))
                                 (nfa-comp (first re) *gensym-counter* (gensympp))
                                 (list (list 'DELTA temp 'epsilon final)))) 
                       (append (or-handler (list (first re)) start final)
                               (or-handler (rest re) start final)))
                   )
                  (T (if (null (rest re))                      ;;CASO SEXP
                         (append (list (list 'DELTA start 'epsilon (gensympp)))
                                 (list (list 'DELTA *gensym-counter* (first re) (gensympp)))
                                 (list (list 'DELTA *gensym-counter* 'epsilon final)))
                       (append (or-handler (list (first re)) start final)
                               (or-handler (rest re) start final)))
                     )
                  )
            )
           )
          )

;;; star-handler/3
;;; gestisce la creazione dell'automa in caso di operatore 'star'

(defun star-handler (re start final)
  (cond ((atom (first re))                    ;;CASO BASE
         (let ((temp (+ 1 *gensym-counter*)))         
           (append (list (list 'DELTA start 'epsilon (gensympp)))
                   (list (list 'DELTA start 'epsilon final))
                   (list (list 'DELTA *gensym-counter* (first re) (gensympp)))
                   (list (list 'DELTA *gensym-counter* 'epsilon temp))
                   (list (list 'DELTA *gensym-counter* 'epsilon final)))
           )
         )
        ((listp (first re))
         (if (is-operator (first (first re)))
             (let ((temp (+ 1 *gensym-counter*))
                   (temp2 (+ 2 *gensym-counter*)));;;; might wanna combine the 2 lets before if
               (append (list (list 'DELTA start 'epsilon (gensympp)))
                       (list (list 'DELTA start 'epsilon final))
                       (nfa-comp (first re) *gensym-counter* (gensympp))
                       (list (list 'DELTA temp2 'epsilon temp))
                       (list (list 'DELTA temp2 'epsilon final)))
                       
               )
           (let ((temp (+ 1 *gensym-counter*)))
             (append (list (list 'DELTA start 'epsilon (gensympp)))
                     (list (list 'DELTA start 'epsilon final))
                     (list (list 'DELTA *gensym-counter* (first re) (gensympp)))
                     (list (list 'DELTA *gensym-counter* 'epsilon temp))
                     (list (list 'DELTA *gensym-counter* 'epsilon final))))
           )
         )
        )
  )

;;; plus-handler/3
;;; gestisce la creazione dell'automa in caso di operatore 'plus

(defun plus-handler (re start final)
  (cond ((atom (first re))                                            ;; CASO BASE
         (append (list (list 'DELTA start (first re) (gensympp)))
                 (star-handler (list (first re)) *gensym-counter* final))
         )
        ((listp (first re))                                           ;; CASO LISTA
         (if (is-operator (first (first re)))
             (let ((temp (+ 1 *gensym-counter*)))                               ;;Caso operatore
               (append (nfa-comp (first re) start (gensympp))
                       (star-handler (list (first re)) temp final)))
           (append (list (list 'DELTA start (first re) (gensympp)))     ;caso  SEXP
                   (star-handler (list (first re)) *gensym-counter* final)))
         )
        )
  )

;;; -----------------------------------------------------------------------------

;;;nfa-test/2
;;;ritorna T nel caso in cui l'input sia accettato dall'automa 'fa'
;;;altrimenti ritorna false

;;;Il predicato da errore nel caso in cui vengano inseriti dati che non siano nel
;;;formato corretto

(defun nfa-test (fa input)
  (let ((f1 (list 'FINAL 1))
        (s0 (list 'START 0)))
    (if (atom fa)
        (error "~a is not a Finite State Automata" fa)
      (if (and (equal (first fa) s0)                  ;;controllo nfa valido
               (equal (first (last fa)) f1))
          (let ((f (rest fa)))
            (nfa-test-state (butlast f) input '(0))     ;;0 è sempre lo stato iniziale
            )
        (error "~a is not a Finite State Automata" fa))  ;;;; Find a way to get parameter name    
      )
    )
  )

;;;nfa-test-state/3
;;;controlla l'effettiva accettazione dell'input da parte dell'NFA 'fa'

(defun nfa-test-state (fa input states)
  (cond ((null states)
         nil)
        ((null input)
         (if (eql 1 (find 1 (e-closure fa states)))
             T
           nil)
         )
        (T (let* ((e-close (e-closure fa states))
                  (newstates (nfa-delta fa (first input) e-close)))
             (nfa-test-state fa (rest input) newstates))
                  )   
        )
  )

;;;nfa-delta/3
;;;gestisce gli stati correnti 'states' e richiama nfa-transition per eseguire
;;;le transizioni necessarie

(defun nfa-delta (fa symbol states)
  (cond ((atom states)
         (nfa-transition fa symbol states))
        (T (append (nfa-delta fa symbol (first states))
                   (nfa-delta fa symbol (rest states)))
           )
        )
  )

;;;nfa-transition/3
;;;Predicato responsabile di eseguire la transizione sull'NFA dato un simbolo e uno stato

(defun nfa-transition (fa symbol state)
  (cond ((null (rest fa))
         (if (and (eql (second (car fa)) state)
                  (equalp (third (car fa)) symbol))
             (last (car fa))
           nil
           )
         )
        (T (append (nfa-transition (list (car fa)) symbol state)
                   (nfa-transition (cdr fa) symbol state))
           )
        )
  )

;;;e-closure/2
;;;Predicato che esegue la e-closure dell'insieme di stati 'states'

(defun e-closure (fa states)
    (cond ((atom states)
           (let ((new-state (nfa-transition fa 'epsilon states)))
             (if (null new-state)
                 (list states)
               (e-closure fa new-state)
               )
             )
           )

          ((listp states)
           (if (null (rest states))
               (e-closure fa (first states))
             (append (e-closure fa (first states))
                     (e-closure fa (rest states)))
             )
           )

          )   
  )
   
;;; -------------------------------------------------------------------------

;;;Utility

;;;gensympp/0
;;;incrementa gensym e ritorna solo il numero da *gensym-counter*

(defun gensympp ()
  (let ((x (gensym)))
    *gensym-counter*))

;;;Funzioni di controllo operatori (per comodità)

(defun is-operator (op)
  (if (or (eql op 'seq)
          (eql op 'or)
          (eql op 'star)
          (eql op 'plus))
      T
    nil))

(defun is-operator-mult (op)
  (if (or (eql op 'seq) (eql op 'or))
      T
    nil))

(defun is-operator-sing (op)
	(if (or (eql op 'star) (eql op 'plus))
		T
		nil))

;;;; ------------------------- EOF End of File ----------------------------- ;;;;