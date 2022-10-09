; 1
; arguments: TREE (tree to perform BFS traversal on)
; return value: list containing nodes of TREE traversed in breadth-first order
(defun BFS (TREE)
  (cond ((null TREE) NIL)
    ((atom (car TREE)) (append (list (car TREE)) (BFS (cdr TREE))))
    (T (BFS (append (cdr TREE) (car TREE))))))

; This function uses the TREE argument as a de facto queue for performing BFS.
; Non-leaf nodes are appended after the yet-unexamined nodes in the recursive BFS
; call, mimicking the queue's behavior of examining nodes from the opposite end
; that they are inserted in.

; 2
; arguments: TREE (tree to perform DFS traversal on)
; return value: list containing nodes of TREE traversed in depth-first order
(defun DFS (TREE)
  (cond ((null TREE) NIL)
    ((atom (car TREE)) (append (list (car TREE)) (DFS (cdr TREE))))
    (T (DFS (append (car TREE) (cdr TREE))))))    

; This function uses the TREE argument as a de facto stack for performing DFS.
; Non-leaf nodes are appended before the still-unexamined nodes in the recursive
; DFS call, mimicking the stack's behavior of examining nodes from the same end
; that they are inserted in.

; 3
; arguments: TREE (tree to build subtree of), DEPTH (max depth to build up to)
; return value: list representing nodes of TREE of depth DEPTH or less, traversed
; from right to left
(defun BUILD-TREE (TREE DEPTH)
  (if (atom TREE) TREE
    (if (> DEPTH 0)
      (let ((CAR-TREE (BUILD-TREE (car TREE) (- DEPTH 1))) (CDR-TREE (BUILD-TREE (cdr TREE) DEPTH)))
        (cond ((null CAR-TREE) CDR-TREE)
          ((null CDR-TREE) (list CAR-TREE))
          (T (append CDR-TREE (list CAR-TREE))))))))

; This function recursively builds a subtree of TREE containing all nodes of depth
; DEPTH or lower, traversed from right to left. When the function recursively
; calls itself, it attempts to build a subtree of max depth DEPTH - 1 with the
; current node under examination as the root node. This recursive call will only
; be made when DEPTH is greater than 0, meaning that all nodes of depth greater
; than 0 in the original tree will not be processed and thus never make it into
; the list returned by the function. In the default case of the bottom-most cond
; statement, the examined node is appended after the yet-unexamined nodes to
; create the desired right-to-left traversal behavior.

; arguments: TREE (tree to perform DFID on), DEPTH (max depth to perform DFID up to)
; return value: 
(defun DFID (TREE DEPTH)
  (if (< DEPTH 0)
    NIL
      (append (DFID TREE (- DEPTH 1)) (DFS (BUILD-TREE TREE DEPTH)))))
    
; This function uses the BUILD-TREE function to build subtrees ranging from depth DEPTH
; to depth 0, and performs DFS on each of the generated subtrees. DFS traversals of 
; lower-depth trees are appended before those on higher-depth trees to achieve the
; expected behavior.    

; These functions implement a depth-first solver for the River-Boat
; problem. In this problem, three members from Group-X, denoted XXX,
; and three members from Group-O, denoted OOO, are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more O's on one side of the river than X's.

; In this implementation, a state is represented by a single list
; (#X #O side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. #X and #O represent the number of X's and
; O's on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three X's, three O's, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (if (equal s (list 3 3 NIL)) T
    NIL))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; X's to move (m), and a number of O's to move (c). It returns a
; list containing the state that results from moving that number of X's
; and O's from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more O's than X's on either side of the river, or because
; it would move more X's or O's than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (let* ((x (+ (- 3 (car s)) m)) (o (+ (- 3 (cadr s)) c)) (e (not (caddr s)))
  (next (list x o e)))
    (cond ((or (> m (car s)) (> c (cadr s))) NIL) ; move more people of one type than are on this side
      ((and (<= m 0) (<= c 0)) NIL) ; empty boat
      ((or (< m 0) (< c 0)) NIL) ; negative input
      ((> (+ m c) 2) NIL) ; move > 2 people at once
      ((and (> x 0) (> o x)) NIL) ; after move, at least 1 x on destination side, more o's than x's
      ((and (> (- 3 x) 0) (> (- 3 o) (- 3 x))) NIL) ; after move, at least 1 x on source side, more o's than x'successor
      (T (list next)))))

; This function generates the next state by creating a list in which the first two
; elements are initialized by subtracting the number of x's and o's in the current state 
; from 3 and then adding m and c respectively (representing the number of x's and o's on 
; the other side of the river after m x's and c o's have moved), and the third element is
; the negation of the third element in the current state (representing the boat moving
; to the other side of the river). If this list does not meet any criteria for
; invalidity (detailed in the comments above), the list is returned as the output of the
; function; otherwise, NIL is returned to represent an invalid state. 

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
  (append (next-state s 0 1) (next-state s 1 0) (next-state s 1 1) (next-state s 2 0) (next-state s 0 2)))

; This function simply appends the outputs of the next-state function in the for state s
; with (m, c) inputs (0, 1), (1, 0), (1, 1), (2, 0), (0, 2), which are the only possible
; valid combinations for placing people on the boat.

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.

(defun on-path (s states)
  (if (null states) NIL
    (if (equal s (car states)) T
      (on-path s (cdr states)))))

; This function returns true if the first element of states is a match for state s,
; and otherwise recursively calls itself with the remainder of the elements in states. 
; The function returns NIL when all states have been exhausted.

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)

(defun mult-dfs (states path)
  (if (null states) NIL ; run out of states
    (let ((dfs-result (mc-dfs (car states) path)))
      (if (final-state (car dfs-result))
        dfs-result
        (mult-dfs (cdr states) path)))))

; This function calls mc-dfs for each state in the states argument, for which the output
; will be a path from that state to the goal state. This path is returned as the output
; of the function if it exists. Otherwise, the function recursively calls itself on the
; yet un-examined states. If all states have been examined without yielding a match,
; the function simply returns NIL.

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (if (final-state s)
    (append (list s) path)
    (if (on-path s path)
      NIL
      (if (null s)
        NIL
        (let ((succs (succ-fn s)))
          (mult-dfs succs (append (list s) path)))))))

; This function first checks if the s input is the goal state using the final-state
; function, and returns the complete path to the goal state if s is the goal state.
; Otherwise, the function will check if s is not an already visited node using the
; on-path function and immediately return NIL if this is the case. The last termination
; condition is if s is NIL, which will be the case when there are no states left to
; examine, upon which the function will return NIL as well. If none of the above 
; conditions are met, the successor states to s are generated with the succ-fn
; function, and mult-dfs is called with the list of successor states and the previous
; path with s prepended.

; Function execution examples

; Applying this operator would result in an invalid state, with more O's
; than X's on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one O and zero X on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))

; 1
#|
(print (BFS NIL))
(print (BFS '(NIL)))
(print (BFS '((w x) (y z))))
(print (BFS '((A (B)) C (D))))
(FRESH-LINE)

; 2
(print (DFS NIL))
(print (DFS '(NIL)))
(print (DFS '((w x) (y z))))
(print (DFS '((A (B)) C (D))))
(FRESH-LINE)

; 3
(print (BUILD-TREE 'A 0))
(print (BUILD-TREE '((w x) (y z)) 2))
(print (BUILD-TREE '((a (b)) c (d)) 0))
(print (BUILD-TREE '((a (b)) c (d)) 1))
(print (BUILD-TREE '((a (b)) c (d)) 2))
(print (BUILD-TREE '((a (b)) c (d)) 4))
(print (BUILD-TREE '(A (B C) (D) (E (F G))) 0))
(print (BUILD-TREE '(A (B C) (D) (E (F G))) 1))
(print (BUILD-TREE '(A (B C) (D) (E (F G))) 2))
(print (BUILD-TREE '(A (B C) (D) (E (F G))) 10))
(FRESH-LINE)

(print (DFID NIL 3))
(print (DFID '(NIL) 3))
(print (DFID '((A (B)) C (D)) 3))
(print (DFID '(A (B C) (D) (E (F G))) 3))
(print (DFID '(((((A))))) 4))
(print (DFID '(((((A))))) 5))
(print (DFID '(((((A))))) 6))
(FRESH-LINE)

; 4
(print (final-state '(3 3 NIL)))
(print (final-state '(0 0 NIL)))
(FRESH-LINE)

(print (next-state '(3 3 T) 1 0))
(print (next-state '(3 3 T) 0 1))
(print (next-state '(2 0 T) 3 0))
(FRESH-LINE)

(print (succ-fn '(0 0 T)))
(print (succ-fn '(3 3 T)))
(print (succ-fn '(1 1 T)))
(print (succ-fn '(1 1 NIL)))
(print (succ-fn '(0 1 NIL)))
(print (succ-fn '(3 2 NIL)))
(print (succ-fn '(0 2 T)))
(print (succ-fn '(3 2 T)))
(print (succ-fn '(0 2 NIL)))
(FRESH-LINE)

(print (on-path '(0 1 NIL) '((0 1 NIL) '(3 3 T))))
(print (on-path '(1 1 NIL) '((0 1 NIL) '(3 3 T))))
(FRESH-LINE)

(print (mc-dfs '(3 3 T) NIL))
(print (mc-dfs '(3 2 T) '((0 2 NIL) (3 3 T)))) ; should be same output as previous case
(print (mc-dfs '(0 0 NIL) NIL))
(print (mc-dfs '(2 1 T) NIL))
(FRESH-LINE)
|#

