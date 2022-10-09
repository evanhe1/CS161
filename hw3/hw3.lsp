;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 1000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (left) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond ((null s) T)
		((numberp s)
			(not (or (= s 2) (= s 3))))
		(T (and (goal-test (car s)) (goal-test (cdr s)))))
  );end defun

; gets value in state S at row r, column c, nthcdr recursively strips away values
; until arriving at row r, column c
(defun get-square (S r c)
	(if (or (< r 0) (< c 0))
		NIL
		(car (nthcdr c (car (nthcdr r S))))))

; updates index c in row R to value v
(defun update-row (R c v)
	(if (null R)
		NIL
		(if (= c 0)
			(append (list v) (cdr R))
			(append (list (car R)) (update-row (cdr R) (- c 1) v)))))

; returns a new state that differs from state S by one element: row r, column c now continas value v
(defun set-square (S r c v)
	(if (null S)
		NIL
		(if (= r 0)
			(append (list (update-row (car S) c v)) (cdr S))
			(append (list (car S)) (set-square (cdr S) (- r 1) c v)))))

; tries move in state S in direction D at row r, column c, using the check-case helper function
; dr and dc represent direction of movement with respect to row and column coordinates
(defun try-move (S D r c)
	(cond 
		((equal D "UP")
			(let ((dr -1) (dc 0))
				(check-cases S r c dr dc)))
		((equal D "DOWN")
			(let ((dr 1) (dc 0))
				(check-cases S r c dr dc)))
		((equal D "LEFT")
			(let ((dr 0) (dc -1))
				(check-cases S r c dr dc)))
		((equal D "RIGHT")
			(let ((dr 0) (dc 1))
				(check-cases S r c dr dc)))))
		
; detects invalid moves at state S depending on r, c, dr, and dc inputs,
; and output new states based on valid current states as indicated in inline comments
(defun check-cases (S r c dr dc)
	(let* ((pos (get-square S r c)) (r1 (+ r dr)) (c1 (+ c dc)) (r2 (+ r (* 2 dr))) (c2 (+ c (* 2 dc)))
	(one-away (get-square S r1 c1)) (two-away (get-square S r2 c2)))
		(cond ((null one-away) NIL) ; out of bounds
			((equal one-away 1) NIL) ; block in front is wall
			((and (equal one-away 2)
					(or (or (equal two-away 1) (equal two-away 2)) (equal two-away 5))) NIL)
					; push box into wall, box, or box + goal
			((and (and (equal pos 3) (equal one-away 0))) ; keeper, blank
				(set-square (set-square S r c 0) r1 c1 3)) ; blank, keeper
			((and (and (equal pos 3) (equal one-away 4))) ; keeper, goal
				(set-square (set-square S r c 0) r1 c1 6)) ; blank, keeper + goal
			((and (and (equal pos 6) (equal one-away 0))) ; keeper + goal, blank
				(set-square (set-square S r c 4) r1 c1 3)) ; goal, keeper
			((and (and (equal pos 6) (equal one-away 4))) ; keeper + goal, goal
				(set-square (set-square S r c 4) r1 c1 6)) ; goal, keeper + goal
			;;;;;;;;;;
			((and (and (equal pos 3) (equal one-away 2)) (equal two-away 0)) ; keeper, box, blank
				(set-square (set-square (set-square S r c 0) r1 c1 3) r2 c2 2)) ; blank, keeper, box
			((and (and (equal pos 3) (equal one-away 2)) (equal two-away 4)) ; keeper, box, goal
				(set-square (set-square (set-square S r c 0) r1 c1 3) r2 c2 5)) ; blank, keeper, box + goal
			((and (and (equal pos 3) (equal one-away 5)) (equal two-away 0)) ; keeper, box + goal, blank
				(set-square (set-square (set-square S r c 0) r1 c1 6) r2 c2 2)) ; blank, keeper + goal, box
			((and (and (equal pos 3) (equal one-away 5)) (equal two-away 4)) ; keeper, box + goal, goal
				(set-square (set-square (set-square S r c 0) r1 c1 6) r2 c2 5)) ; blank, keeper + goal, box + goal
			;;;;;;;;;;
			((and (and (equal pos 6) (equal one-away 2)) (equal two-away 0)) ; keeper + goal, box, blank
				(set-square (set-square (set-square S r c 4) r1 c1 3) r2 c2 2)) ; goal, keeper, box
			((and (and (equal pos 6) (equal one-away 2)) (equal two-away 4)) ; keeper + goal, box, goal
				(set-square (set-square (set-square S r c 4) r1 c1 3) r2 c2 5)) ; goal, keeper, box + goal
			((and (and (equal pos 6) (equal one-away 5)) (equal two-away 0)) ; keeper + goal, box + goal, blank
				(set-square (set-square (set-square S r c 4) r1 c1 6) r2 c2 2)) ; goal keeper + goal, box
			((and (and (equal pos 6) (equal one-away 5)) (equal two-away 4)) ; keeper + goal, box + goal, goal
				(set-square (set-square (set-square S r c 4) r1 c1 6) r2 c2 5))))) ; goal, keeper + goal, box + goal
	
; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0)) ; this function gives column before row, I didn't make it
	 (c (car pos))
	 (r (cadr pos))
	 ;r and c are now the coordinate of the keeper in s.
	 (result (list (try-move s "UP" r c) (try-move s "RIGHT" r c) (try-move s "DOWN" r c) (try-move s "LEFT" r c)))
	 )
    (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;

; This is an admissible heuristic because in each step, the keeper can push at most one
; box. Therefore, even in the best case scenario, when all boxes are only one move away
; from a unique goal, the heuristic will not overestimate the number of moves. 
; Additionally, the number of incorrectly positioned boxes is always nonnegative. Thus, 
; both criteria for an admissible heuristic are satisfied. 
(defun h1 (s)
	(cond ((null s) 0)
		((numberp s)
			(if (= s 2) 1 0))
		(T (+ (h1 (car s)) (h1 (cdr s)))))
  )

; EXERCISE: Modify this h2 function to compute an
; admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
; heuristic is total distance from keeper to each box + distance from box to nearest goal
; top-level function initializes list of goals and boxes for helper functions to use,
; saving computation later on
(defun h2 (s)
	(let ((boxes (list-boxes s 0 0)) (goals (list-goals s 0 0))
	(pos (getKeeperPosition s 0)))
		(+ (sum-distances s boxes goals pos) (find-nearest-goal-dist pos goals 1000))
  ))

; helper function for h2
; determines if a box is pushed into a corner in state S and the corner is not a goal state, since this state
; cannot possibly transform into a goal state; corners determined by at least one horizontal edge (left or right) and at least
; one vertical edge (top or bottom) being NIL
(defun check-corner-deadlock (S box)
	(let ((up (get-square S (- (cadr box) 1) (car box))) (down (get-square S (+ (cadr box) 1) (car box))) 
	(left (get-square S (cadr box) (- (car box) 1))) (right (get-square S (cadr box) (+ (car box) 1))))
		(and (or (or (null up) (equal up 1)) (or (null down) (equal down 1))) 
		(or (or (null left) (equal left 1)) (or (null right) (equal right 1))))))

; helper function for h2
; sums distances from each box to nearest goal in S with distance from keeper to nearest goal, while giving a large weight
; to paths that end to a corner to terminate search along said path
(defun sum-distances (s boxes goals pos)
	(if (null boxes) 0
		(if (null goals) 0
			(let* ((box (car boxes)) (keeper-dist (abs (+ (- (car pos) (car box)) (- (cadr pos) (cadr box)))))) ; flipping keeper position
				(if (check-corner-deadlock s box)
					1000
					(+ (+ keeper-dist (find-nearest-goal-dist box goals 1000)) (sum-distances s (cdr boxes) goals pos)))))
))

; helper function for h2
; finds nearest goal in list of goals to object obj (either the keeper or a box), min is used as the current minimum distance
; for all goals exmined so far
(defun find-nearest-goal-dist (obj goals min)
	(if (null goals) min
		(let* ((r (cadr obj)) (c (car obj)) (goal_r (cadr (car goals))) (goal_c (car (car goals))) (dist (abs (+ (- goal_r r) (- goal_c c)))))
			(if (= dist 0)
				0
				(if (< dist min)
					(find-nearest-goal-dist obj (cdr goals) dist)
					(find-nearest-goal-dist obj (cdr goals) min))))))

; helper function for h2
; top level recursive function for listing all goals
(defun list-goals (s r c)
	(if (null s) NIL
		(append (list-goals-row (car s) r 0) (list-goals (cdr s) (+ r 1) 0))))

; helper function for list-goals
; extracts all goals in row s, which will be one row in the s argument passed to the parent list-goals function
(defun list-goals-row (s r c)
	(if (null s) NIL
		(if (or (or (= (car s) 4) (= (car s) 5)) (= (car s) 6))
			(append (list (list c r)) (list-goals-row (cdr s) r (+ c 1))) ; make box coordinates consistent with player
			(list-goals-row (cdr s) r (+ c 1)))))

; helper function for h2
; top level recursive function for listing all boxes
(defun list-boxes (s r c)
	(if (null s) NIL
		(append (list-boxes-row (car s) r 0) (list-boxes (cdr s) (+ r 1) 0))))

; helper function for list-boxes
; extracts all boxes not on a goal in row s, which will be one row in the s argument passed to the parent list-boxes function
(defun list-boxes-row (s r c)
	(if (null s) NIL
		(if (or (= (car s) 2))
			(append (list (list c r)) (list-boxes-row (cdr s) r (+ c 1)))
			(list-boxes-row (cdr s) r (+ c 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)

(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)

(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun

(setq s1 '((1 1 1 1 1)
	   (1 4 0 0 1)
	   (1 0 2 0 1)
	   (1 0 3 0 1)
	   (1 0 0 0 1)
	   (1 1 1 1 1)))

(setq s2 '((1 1 1 1 1)
	   (1 0 0 4 1)
	   (1 0 2 3 1)
	   (1 0 0 0 1)
	   (1 0 0 4 1)
	   (1 1 1 1 1)))

(setq s3 '((1 1 1 1 1)
	   (1 0 0 6 1)
	   (1 0 2 0 1)
	   (1 0 0 0 1)
	   (1 4 0 4 1)
	   (1 1 1 1 1)))

(setq s4 '((1 1 1 1 1)
	   (1 0 2 4 1)
	   (1 0 0 0 1)
	   (1 0 0 0 1)
	   (1 0 5 3 1)
	   (1 1 1 1 1)))

(setq p1 '((0 0 1 1 1 1 0 0 0)
	(1 1 1 0 0 1 1 1 1)
	(1 0 0 0 0 0 2 0 1)
	(1 0 1 0 4 1 2 0 1)
	(1 0 4 0 4 1 3 0 1)
	(1 1 1 1 1 1 1 1 1)))
(load-a-star)

;(print (next-states p7))
;(a* p11 #'goal-test #'next-states #'h0)
;(a* p11 #'goal-test #'next-states #'h1)
;(sokoban p1 #'h2)
;(sokoban p2 #'h2)
;(sokoban p3 #'h2)
;(sokoban p4 #'h2)
;(sokoban p5 #'h2)
;(sokoban p6 #'h2)
;(sokoban p7 #'h2)
;(sokoban p8 #'h2)
;(sokoban p9 #'h2)
;(sokoban p10 #'h2)
;(sokoban p11 #'h2)
;(sokoban p12 #'h2)
;(sokoban p14 #'h2)
;(FRESH-LINE)
;(sokoban s1 #'h2)
;(sokoban s2 #'h2)
;(sokoban s3 #'h2)
;(sokoban s4 #'h2)