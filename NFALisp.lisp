(defparameter seq 'seq)
(defparameter star 'star)
(defparameter plus 'plus)
(defparameter alt  'or)    ;;might wanna remove these
(defvar *count* 1)

(defun is-regexp (re)
  (cond ((null re) nil) 
        ((atom re) T)                    ;CONTROLLA OR CON 1 ARGOMENT
        ((is-operator-mult (first re))   ;controllo se operatore= seq o or
         (if (>= (length re) 2)        ;controllo n° argomenti or/seq ;accettato caso degenere
             (is-regexp (rest re))         
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


(defun nfa-regexp-comp (re)
  (let ((z (setq *count* 1)))       ;resetta *count* (numero crescente stati)
    (if (is-regexp re)
        (append '((START 0))
                (nfa-comp re 0 1)
                '((FINAL 1))
                )                   
      nil)
    )
  )


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

          
(defun seq-handler (re start final)
  (cond ((atom (first re))           ;se first è un simbolo
         (cond ((null (rest re))     ;se ho solo 1 elemento rimanente
                (list (list 'DELTA start (first re) final)))
               (T                    ;N elementi
                (append (seq-handler (list (first re)) start (countpp))
                        (list (list 'DELTA *count* 'epsilon (countpp)))
                        (seq-handler  (rest re) *count* final))
                )
               )
         )
        ((listp (first re))                                 ;se first è una sotto-lista
         (cond ((is-operator (first (first re)))            ;CASO Operatore
                (if (null (rest re))                        ;singolo
                     (nfa-comp (first re) start final)
                  (let ((temp (+ 1 *count*)))
                    (append (nfa-comp  (first re) *count* (countpp)) ;piu elementi
                            (list (list 'DELTA temp 'epsilon (countpp)))
                            (seq-handler (rest re) *count* final))
                    )
                  )
                )

               (T (if (null (rest re))                      ;CASO Sexp
                      (list (list 'DELTA start (first re) final))
                    (append (seq-handler (list (first re)) start (countpp))
                            (list (list 'DELTA *count* 'epsilon (countpp)))
                            (seq-handler  (rest re) *count* final)))
                  )
               )
         )
        )
)


(defun or-handler (re start final)
    (cond ((atom (first re))
           (cond ((null (rest re))                   ;;Elemento Singolo
                  (append (list (list 'DELTA start 'epsilon (countpp)))
                          (list (list 'DELTA *count* (first re) (countpp)))
                          (list (list 'DELTA *count* 'epsilon final)))
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
                       (let ((temp (+ 2 *count*)))
                         (append (list (list 'DELTA start 'epsilon (countpp)))
                                 (nfa-comp (first re) *count* (countpp))
                                 (list (list 'DELTA temp 'epsilon final)))) 
                       (append (or-handler (list (first re)) start final)
                               (or-handler (rest re) start final)))
                   )
                  (T (if (null (rest re))                      ;;CASO SEXP
                         (append (list (list 'DELTA start 'epsilon (countpp)))
                                 (list (list 'DELTA *count* (first re) (countpp)))
                                 (list (list 'DELTA *count* 'epsilon final)))
                       (append (or-handler (list (first re)) start final)
                               (or-handler (rest re) start final)))
                     )
                  )
            )
           )
          )

(defun star-handler (re start final)
  (cond ((atom (first re))                    ;;CASO BASE
         (let ((temp (+ 1 *count*)))         
           (append (list (list 'DELTA start 'epsilon (countpp)))
                   (list (list 'DELTA start 'epsilon final))
                   (list (list 'DELTA *count* (first re) (countpp)))
                   (list (list 'DELTA *count* 'epsilon temp))
                   (list (list 'DELTA *count* 'epsilon final)))
           )
         )
        ((listp (first re))
         (if (is-operator (first (first re)))
             (let ((temp (+ 1 *count*))
                   (temp2 (+ 2 *count*)));;;; might wanna combine the 2 lets before if
               (append (list (list 'DELTA start 'epsilon (countpp)))
                       (list (list 'DELTA start 'epsilon final))
                       (nfa-comp (first re) *count* (countpp))
                       (list (list 'DELTA temp2 'epsilon temp))
                       (list (list 'DELTA temp2 'epsilon final)))
                       
               )
           (let ((temp (+ 1 *count*)))
             (append (list (list 'DELTA start 'epsilon (countpp)))
                     (list (list 'DELTA start 'epsilon (final)))
                     (list (list 'DELTA *count* (first re) (countpp)))
                     (list (list 'DELTA *count* 'epsilon temp))
                     (list (list 'DELTA *count* 'epsilon final))))
           )
         )
        )
  )

(defun plus-handler (re start final)
  (cond ((atom (first re))                                            ;; CASO BASE
         (append (list (list 'DELTA start (first re) (countpp)))
                 (star-handler (list (first re)) *count* final))
         )
        ((listp (first re))                                           ;; CASO LISTA
         (if (is-operator (first (first re)))
             (let ((temp (+ 1 *count*)))                               ;;Caso operatore
               (append (nfa-comp (first re) start (countpp))
                       (star-handler (list (first re)) temp final)))
           (append (list (list 'DELTA start (first re) (countpp)))     ;caso  SEXP
                   (star-handler (list (first re)) *count* final)))
         )
        )
  )


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


(defun nfa-test-state (fa input states)
  (cond ((null states)
         nil)
        ((null input)
         (if (eql 1 (find 1 (e-closure fa states)))
             T
           nil)
         )
        (T (nfa-test-state fa (rest input) (nfa-delta fa (first input) (e-closure fa states)))
           )   ;; TRY TO CLEAN THIS UP A BIT WITH LET BUT IDK IT DIDNT WORK THE FIRST TIME
        )
  )

(defun nfa-delta (fa symbol states)
  (cond ((atom states)
         (nfa-transition fa symbol states))
        (T (append (nfa-delta fa symbol (first states))
                   (nfa-delta fa symbol (rest states)))
           )
        )
  )

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
   



(defun countpp ()
  (setq *count* (+ 1 *count*)))

(defun is-operator (op)
  (if (or (eql op seq)
          (eql op 'or)
          (eql op star)
          (eql op plus))
      T
    nil))

(defun is-operator-mult (op)
  (if (or (eql op seq) (eql op 'or))
      T
    nil))

(defun is-operator-sing (op)
	(if (or (eql op star) (eql op plus))
		T
		nil))