;;; 1.
;;; arguments: N (target number in tree), TREE (list representing ordered tree that is 
;;; searched for N)
;;; return value: T if N appears in TREE, NIL otherwise
(defun TREE-CONTAINS (N TREE)
  (if (null TREE) NIL
    (if (atom TREE) (equal N TREE) 
    (or (TREE-CONTAINS N (car TREE)) (TREE-CONTAINS N (cdr TREE))))))

;;; The function will recursively examine every number in TREE, include numbers inside nested elements, 
;;; and compare if the number is equal to the target number N. On the condition that TREE is NIL, which will occur when
;;; either the initial TREE input is NIL or when every number in TREE has already been examined, the function will return NIL
;;; to reflect the absence of N. 


;;; 2.
;;; arguments: TREE (list representing ordered tree to find the maximum value that 
;;; appears in it)
;;; return value: maximum number in TREE
(defun TREE-MAX (TREE)
  (if (atom TREE) TREE
    (if (= (length TREE) 1)
    (TREE-MAX (car TREE))
    (TREE-MAX (cdr TREE)))))

;;; Given that the input TREE is guaranteed to be an ordered tree, the greatest number appearing in TREE will simply be the
;;; the number farthest to the right in the tree's representation. Recursively calling the function on cdr TREE will ensure that 
;;; we always travel down the right branch of the tree until reaching the rightmost leaf of the tree. In this case, car TREE is 
;;; used to return the leaf itself as a number. 


;;; 3.
;;; arguments: TREE (list representing ordered tree to perform a post-order traversal of)
;;; return value: list containing post-order traversal of TREE
(defun TREE-ORDER (TREE)
  (if (null TREE) TREE
  (if (numberp TREE) (list TREE)
    (append (TREE-ORDER (car TREE)) (TREE-ORDER (caddr TREE)) (list (cadr TREE))))))

;;; Post order traversal involves first traversing the left branch, then traversing the right branch, and finally examining the
;;; parent node. The above recursion accomplishes this by extracting the left branch with car TREE, the right branch with caddr
;;; TREE, and the parent node with cadr TREE, recursively calling the function on each of the above components, and appending the
;;; return values of each of the recursive calls. In the base case of TREE being NIL, TREE is directly returned. In the base case 
;;; of TREE being a number, TREE is wrapped in a list because the append function requires all inputs to be lists to correctly 
;;; return a proper list.


;;; 4.
;;; arguments: L (parent list), START (non-negative start index of sub-list), LEN (non-negative length of sub-list)
;;; return value: list containing sub-list of L begining at index START of length LEN
(defun SUB-LIST (L START LEN)
  (if (= START 0) 
    (if (or (= (length L) 0) (= LEN 0)) NIL
      (if (= (length L) 1) (list (car L))
        (append (list (car L)) (SUB-LIST (cdr L) 0 (- LEN 1)))))
        (SUB-LIST (cdr L) (- START 1) LEN)))

;;; The function will first arrive at the starting index by recursively calling itself with cdr L and length decremented by 1 
;;; until START is 0. Next, the LEN variable will be used to construct the sub-list by appending additional elements to the 
;;; sub-list until either LEN is 0 or L's length is 0 (representing that we have run out of elements in L and must terminate early). 
;;; This prevents the padding of extra NIL elements to the final sub-list.
  

;;; 5.
;;; arguments: L (parent list to split)
;;; return value: list (L1 L2) where L is the result of appending L1 and L2 and L1 and L2 differ in length by 0 or 1
(defun SPLIT-LIST (L)
  (if (oddp (length L))
    (list (SUB-LIST L 0 (/ (+ 1 (length L)) 2)) 
    (SUB-LIST L (/ (+ 1 (length L)) 2) (- (/ (+ 1 (length L)) 2) 1)))
    (list (SUB-LIST L 0 (/ (length L) 2))
    (SUB-LIST L (/ (length L) 2) (/ (length L) 2)))))

;;; The cases of an even-length and odd-length input are handled separately due to the fact that integer division in Lisp does
;;; not result in truncated outputs. The split lists are generated using the SPLIT-LIST function. In the case of an odd-length
;;; list, the left sub-list will contain one more element than the right sub-list.


;;; 6.
;;; arguments: TREE (binary tree to find height of)
;;; return value: number representing height of tree (longest root-to-leaf path)
(defun BTREE-HEIGHT (TREE)
  (if (atom TREE) 0
    (let ((LEFT-HEIGHT (BTREE-HEIGHT (car TREE))) 
    (RIGHT-HEIGHT (BTREE-HEIGHT (cadr TREE))))
      (if (> LEFT-HEIGHT RIGHT-HEIGHT)
        (+ LEFT-HEIGHT 1)
        (+ RIGHT-HEIGHT 1)))))

;;; The function recursively calculates the heights of the left and right sub-trees and adds 1 to the greater of the two values
;;; (since the parent node represents an additional level not contained in its child sub-trees). The base case of the recursion is
;;; to simply return 0 when TREE is an atom. This return value is 0 instead of 1 because the height incrementation occurs in the
;;; recursive call one level above the one in which a node is actually explored.


;;; 7.
;;; arguments: LEAVES (non-empty list representing leaves of binary tree to be contructed)
;;; return value: list representing binary tree contructed from LEAVES (leaves are elements of LEAVES, number of leaves on a left 
;;; branch differ from the number of leaves on a corresponding right branch by 0 or 1)
(defun LIST2BTREE (LEAVES)
  (if (null LEAVES) NIL
    (if (= (length LEAVES) 1) (car LEAVES)
      (if (= (length LEAVES) 2) LEAVES
        (let ((SPLIT (SPLIT-LIST LEAVES)))
        (list (LIST2BTREE (car SPLIT)) (LIST2BTREE (cadr SPLIT))))))))

;;; The function simply calls SPLIT-LIST recursively until each nested list is of length 2, satifying the definition of a binary
;;; tree. SPLIT-LIST will allow us to handle both odd and even-length lists. The base cases of LEAVES being a list of length 1 and 
;;; a list of length 2 must be handled separately because the list wrapper must be stripped away from lists of length 1 in the
;;; final output using car LEAVES.


;;; 8.
;;; arguments: TREE (list representing binary tree)
;;; return value: list of atoms representing the flattened TREE input
(defun BTREE2LIST (TREE)
  (if (null TREE) NIL
    (if (atom TREE) (list TREE)
      (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))))))

;;; The function takes advantage of the append function to recursively flatten the input TREE one level at a time. In the base
;;; case of TREE being an atom, TREE will be wrapped in a list because the append function requires all inputs to be lists in 
;;; order to correctly return a proper list.


;;; 9.
;;; arguments: E1 (first expression to compare, atoms are all numbers), E2 (second expression to compare, atoms are all numbers)
;;; return value: T if E1 and E2 represent the same expression, NIL otherwise
(defun IS-SAME (E1 E2) ; breaks on case NIL '(NIL)
  (if (and (numberp E1) (numberp E2)) (= E1 E2)
    (if (and (null E1) (null E2)) T
      (if (not (and (listp E1) (listp E2))) NIL
        (and (= (length E1) (length E2)) (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))))))

;;; The function compares both expressions by directly comparing E1 and E2 if they are either both numbers, returning true 
;;; if both E1 and E2 are NIL (this case can only occur within a recursive call), or returning false if either E1 or E2 is not 
;;; a list. If both elements are lists, the function recursively calls itself on car E1 and car E2, and cdr E1 and cdr E2. The
;;; length comparison check handles the edge case of {NIL, '(NIL)}, since previous conditions are not sufficient to differentiate 
;;; the two inputs. This check is not explicitly required for this problem.


;;; 10.
;;; arguments: E1 (first expression to append, not an atom itself, all atoms are numbers), E2 (second expression that is flattened
;;; before it is appended to E1, all atoms are numbers)
;;; return value: list containing unmodified elements of E1 followed by flattened elements of E2
(defun FLATTEN-APPEND (E1 E2)
  (if (null E2) (append E1 E2)
    (if (numberp E2)
      (append E1 (list E2))
      (append E1 (FLATTEN-APPEND NIL (car E2)) (FLATTEN-APPEND NIL (cdr E2))))))

;;; Similar to the BTREE2LIST function, the function will use the append function to recursively flatten E2 before appending
;;; the flattened E2 to E1 once in the outermost recursive call. NIL is passed for E1 in all other recursive calls so that E1
;;; only appears once in the final output. In the base case of E2 being NIL, E2 is directly appended to E1. In the base case of E2 
;;; being a number, E2 is wrapped in a list before being appended to E1 so that the output is a proper list.