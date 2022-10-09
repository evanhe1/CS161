;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (dfs delta 1 NIL n)
	)

(defun validate-clause (clause)
  (if (atom clause)
    (or (numberp clause) (equal T clause))
    (or (validate-clause (car clause)) (validate-clause (cdr clause)))))

;(print (validate-clause 1))
;(print (validate-clause '(NIL 1 NIL NIL)))

(defun update-clause (clause index)
  (if (atom clause)
    (if (and (numberp clause) (equal index (* clause -1)))
      NIL
      (list clause))
    (if (equal (length (cdr clause)) 0)
      (update-clause (car clause) index)
      (append (update-clause (car clause) index) (update-clause (cdr clause) index)))))

(print (update-clause '(-1 2 3 4) 1))
        
(defun contains-literal (clause literal)
  (if (null clause)
    NIL
    (if (equal (car clause) literal)
      T
      (contains-literal (cdr clause) literal))))

(print (contains-literal '(100000 100) 100))

;(print (update-clause '(NIL) 1 NIL))

; takes list of lists (CNF) as input
(defun validate-CNF (CNF)
  (if (null CNF) T ; empty cnf is valid
    (let ((car-valid (validate-clause (car CNF))))
      (if (null car-valid)
        NIL
        (and (validate-clause (car CNF)) (validate-CNF (cdr CNF)))))))

;(print (validate-CNF '((T NIL))))

(defun update-CNF (CNF index)
  (if (null CNF) NIL
    (if (contains-literal (car CNF) index)
      (update-CNF (cdr CNF) index)
      (if (equal (length (cdr CNF)) 0)
        (list (update-clause (car CNF) index))
          (append (list (update-clause (car CNF) index)) (update-CNF (cdr CNF) index))))))

;(print (update-clause '(1 2 3 NIL nIL NIL NIL) 1 NIL))
;(print (update-clause '(1) 1 NIL))
;(print (update-CNF '((1 1 1 2 3 4 5) (1 2 3 4 5 6 1 1 1 1 1) (1 2 3 NIL nIL NIL NIL) (1)) 1 NIL))
;(print (update-CNF '((1) (1) (1) (1) (10)) 1 NIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

(defun update-and-validate (CNF index assignments n)
  ;(print assignments)
  (let ((update (update-CNF CNF index)))
    (if (validate-CNF update)
      (dfs update (+ (abs index) 1) (append assignments (list index)) n)
      NIL)))
#|
(defun dfs (CNF index assignments n)
  ;(if (and (equal -1 (car assignments)) (< 23 (length assignments))) (print assignments))
  (if (equal index (+ n 1))
    (print assignments)
    (let* ((update-true (update-CNF CNF index T)) (update-false (update-CNF CNF index NIL)))
      (if (validate-CNF update-true)
        (dfs update-true (+ (abs index) 1) (append assignments (list index)) n))
      (if (validate-CNF update-false)
        (dfs update-false (+ (abs index) 1) (append assignments (list (* index -1))) n)
          NIL))))
|#
(defun dfs (CNF index assignments n)
  ;(if (and (equal -1 (car assignments)) (< 23 (length assignments))) (print assignments))
  (if (equal index (+ n 1))
    assignments
    (or (update-and-validate CNF index assignments n) (update-and-validate CNF (* index -1) assignments n))))

(setq cnf1 (parse-cnf "cnfs/f1/sat_f1.cnf"))
;(print (validate-cnf cnf1;))
;(print (cadr cnf1))
;(FRESH-LINE)
;(print (update-cnf (cadr cnf1) 1 NIL))
;(print (list (update-clause '(-1 10 23) 1 T)))
;(print (sat? (car cnf1) (cadr cnf1)))
(print (solve-cnf "cnfs/f1/sat_f1.cnf"))
(print (solve-cnf "cnfs/f2/sat_f2.cnf"))
(print (solve-cnf "cnfs/f3/sat_f3.cnf"))
(time (solve-cnf "cnfs/f1/sat_f1.cnf"))
(time (solve-cnf "cnfs/f2/sat_f2.cnf"))
(time (solve-cnf "cnfs/f3/sat_f3.cnf"))
(print (solve-cnf "cnfs/uf50-01.cnf"))
(time (solve-cnf "cnfs/uf50-01.cnf"))
;(print (solve-cnf "cnfs/uf100-01.cnf"))