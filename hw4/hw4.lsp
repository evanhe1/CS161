;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;

; sources used: https://www.cs.cmu.edu/~15414/f17/lectures/10-dpll.pdf, http://kti.ms.mff.cuni.cz/~bartak/constraints/ordering.html

; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
    (sort-list-abs NIL (dfs delta (make-variable-order delta n) NIL n))
	)

(defun validate-clause (clause)
  (if (atom clause)
    (or (numberp clause) (equal T clause))
    (or (validate-clause (car clause)) (validate-clause (cdr clause)))))

(defun update-clause (clause index)
  (if (atom clause)
    (if (and (numberp clause) (equal index (* clause -1)))
      NIL
      (list clause))
    (if (equal (length (cdr clause)) 0)
      (update-clause (car clause) index)
      (append (update-clause (car clause) index) (update-clause (cdr clause) index)))))

(defun get-variable-count-clause (clause var)
  (if (null clause)
    0
    (if (or (equal (car clause) var) (equal (car clause) (* -1 var)))
      (+ 1 (get-variable-count-clause (cdr clause) var))
      (get-variable-count-clause (cdr clause) var))))

(defun get-variable-count-CNF (CNF var)
  (if (null CNF)
    0
    (+ (get-variable-count-clause (car CNF) var) (get-variable-count-CNF (cdr CNF) var))))

(defun make-unsorted-variable-counts-list (CNF n)
  (if (= 0 n)
    NIL
    (append (make-unsorted-variable-counts-list CNF (- n 1)) (list (list n (get-variable-count-CNF CNF n))))))

(defun insert-pair (container pair)
  (if (null container)
    (list pair)
    (let ((container-first-count (cadr (car container))) (pair-count (cadr pair)))
      (if (> pair-count container-first-count)
        (append (list pair) container)
        (append (list (car container)) (insert-pair (cdr container) pair))))))

(defun sort-list-pairs (sorted unsorted)
  (if (null unsorted)
    (append sorted unsorted)
    (sort-list-pairs (insert-pair sorted (car unsorted)) (cdr unsorted))))

(defun insert (container num)
  (if (null container)
    (list num)
    (let ((container-first (car container)))
      (if (< (abs num) (abs container-first))
        (append (list num) container)
        (append (list container-first) (insert (cdr container) num))))))

(defun sort-list-abs (sorted unsorted)
  (if (null unsorted)
    (append sorted unsorted)
    (sort-list-abs (insert sorted (car unsorted)) (cdr unsorted))))

(defun remove-counts (container)
  (if (null container)
    NIL
    (append (list (car (car container))) (remove-counts (cdr container)))))

(defun make-variable-order (CNF n)
  (remove-counts (sort-list-pairs NIL (make-unsorted-variable-counts-list CNF n))))

(defun contains-literal (clause literal)
  (if (null clause)
    NIL
    (if (equal (car clause) literal)
      T
      (contains-literal (cdr clause) literal))))

(defun validate-CNF (CNF)
  (if (null CNF) T
    (let ((car-valid (validate-clause (car CNF))))
      (if (null car-valid)
        NIL
        (and (validate-clause (car CNF)) (validate-CNF (cdr CNF)))))))

(defun update-CNF (CNF index)
  (if (null CNF) NIL
    (if (contains-literal (car CNF) index)
      (update-CNF (cdr CNF) index)
      (if (equal (length (cdr CNF)) 0)
        (list (update-clause (car CNF) index))
          (append (list (update-clause (car CNF) index)) (update-CNF (cdr CNF) index))))))

(defun update-and-validate (CNF variable variable-order assignments n)
  (let* ((update (update-CNF CNF variable)))
    (if (validate-CNF update)
      (dfs update (cdr variable-order) (append assignments (list variable)) n)
      NIL)))
      
(defun dfs (CNF variable-order assignments n)
  (if (null variable-order)
    assignments
    (let* ((variable (car variable-order)))
      (or (update-and-validate CNF variable variable-order assignments n) (update-and-validate CNF (* variable -1) variable-order assignments n)))))

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
      if (equal 'p (first line)) collect (third line) 
      if (integerp (first line)) collect (butlast line))))

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

;(setq cnf1 (parse-cnf "cnfs/f1/sat_f1.cnf"))
;(print cnf1)
;(setq vars-cnf1 (make-unsorted-variable-counts-list (cadr cnf1) (car cnf1)))
;(print vars-cnf1)
;(print (sort-list-pairs NIL vars-cnf1))
;(print (validate-cnf cnf1;))
;(print (cadr cnf1))
;(print (update-cnf (cadr cnf1) 1 NIL))
;(print (sat? (car cnf1) (cadr cnf1)))
;(print (solve-cnf "cnfs/f1/sat_f1.cnf"))
;(print (solve-cnf "cnfs/f2/sat_f2.cnf"))
;(print (solve-cnf "cnfs/f3/sat_f3.cnf"))
;(time (solve-cnf "cnfs/f1/sat_f1.cnf"))
;(time (solve-cnf "cnfs/f2/sat_f2.cnf"))
;(time (solve-cnf "cnfs/f3/sat_f3.cnf"))
;(print (solve-cnf "cnfs/uf50-01.cnf"))
;(time (solve-cnf "cnfs/uf50-01.cnf"))
;(print (solve-cnf "cnfs/uf50-02.cnf"))
;(time (solve-cnf "cnfs/uf50-02.cnf"))
;(print (solve-cnf "cnfs/uf100-01.cnf"))