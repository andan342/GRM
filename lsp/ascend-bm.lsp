;;GRM parser-generator file, adapted for ALisp
;;(C) 2003-2014, Andrej Andrejev

(defvar _accept_empty_input_ t) ;; parser will return NIL if -! or DELIMS is encoutered at initial state (^)

(defstruct grammar ts nts rules start delims ; primary props
  nullable-nts sht nt-cnt t-cnt symbol-cnt begins-with is-end-of is-followed-by init1-flag rule-status-cnt t-status-cnt nt-status-cnt ; symbol-based props
  active-symbols active-nt-cnt active-rules occurence-map o-bw o-bw-map keys) ; occurence-based props

(defstruct sht-rec 
  id ; 0 .. (nt-cnt - 1) for NTs, nt-cnt .. nt-count + t-cnt for Ts
  prec ; explicit precedence (<rank> . <assoc>), where <assoc> is either :left, :right, :nonassoc
  active-id ; 0 .. (nt-cnt - 1), TODO: should be updated on status change!
  first ; first-set of the symbol (not used?) 
  follow ; follow-set of the symbol (not used?)
  status ; status of the symbol
  types ; type tags of the symbol
  keys) ; assoc-list of '((occ1 occ2 .. occN) . key-id)

(defstruct rule lhs rhs action prec status id)

;; STATUS: NIL = ON, 1 = OFF 2 = DEAD, 3 = UNREACHABLE

(defparameter status-width 5)

(defparameter nttypes-width 4)

(defstruct occurence ruleno pos symbol-id)

(defstruct key-rec key pt-row ct-groups)

;;====================== ALisp compatibility features

(defun member-test (item list test)
  (isome list (f/l (x tl) (funcall test x item))))

(defmacro pushnew (item place test)
  `(unless (member-test ,item ,place ,test)
    (push ,item ,place)))

(defun assoc-test (item alist test)
  (car (isome alist (f/l (x) (funcall test (car x) item)))))

(defun copy-list (list)
  "Copy the top structure of LIST"
  (if (atom list) list
    (cons (car list) (copy-list (cdr list)))))

(defun acons (key datum alist)
  (cons (cons key datum) alist))

;;========================== convert grammars from Johnson's format

(defun grammar-from-johnsons (j)
  "Make gramamr from list of (<nt> -> s1 ... sN) rules"
  (let (nts symbols rules rhs action start delims prec res prec-rank srec prec-found)	   
    (dolist (r j)
      (selectq (car r)
	       (:start (setq start (second r)))
	       (:delims (setq delims (cdr r)))
	       (:prec (setq prec (cdr r)))
	       (progn
		 (pushnew-equal (car r) nts)
		 (setq rhs (butlast (nthcdr (if (eq (second r) :prec) 4 2) r)))
		 (dolist (s rhs)
		   (pushnew-equal s symbols))
		 (setq action (car (last r)))
		 (push (make-rule :lhs (car r) :rhs rhs :action (if (numberp action) action (cadr action)) 
				  :prec (when (eq (second r) :prec) (third r))) rules))))
    (setq res (make-grammar :ts (append (set-difference-equal (nreverse symbols) nts) delims) :nts (nreverse nts) :rules (nreverse rules) 
			    :start (if start start (caar j)) :delims delims))
    (grammar-init-sht res)
    (setq prec-rank 0)
    (dolist (p prec)
      (incf prec-rank)
      (dolist (s (cdr p))
	(setq srec (gethash s (grammar-sht res)))
	(when srec ; precedence manifest might include pseudo-symbols
	  (setf (sht-rec-prec srec) (cons prec-rank (car p))))))
    (when prec ; adjust precedence      
      (dolist (rule (grammar-rules res)) 
	(setf (rule-prec rule)
	      (if (rule-prec rule) ; search in precedence mainfest directly (might be a pseudo-symbol)
		  (progn 
		    (setq prec-rank 0)
		    (dolist (p prec)
		      (incf prec-rank)
		      (when (member (rule-prec rule) (cdr p))
			(return (cons prec-rank (car p))))))
		(dolist (s (reverse (rule-rhs rule))) ; search in hash table for last symbol with prec defined
		  (setq prec-found (sht-rec-prec (gethash s (grammar-sht res))))
		  (when prec-found (return prec-found)))))))
    res))
		     
		

;;========================= SYMBOLS HASH TABLE

(defun grammar-init-sht (x)
  "Initialize symbol hash table and counts"
  (let ((sht (make-hash-table :test #'equal)) (id -1))
    (dolist (s (grammar-nts x))
      (setf (gethash s sht) (make-sht-rec :id (incf id) :types (make-array nttypes-width :initial-element 0))))
    (setf (grammar-nt-cnt x) (1+ id))
    (dolist (s (grammar-ts x))
      (setf (gethash s sht) (make-sht-rec :id (incf id))))
    (setf (grammar-symbol-cnt x) (1+ id))
    (setf (grammar-t-cnt x) (- (grammar-symbol-cnt x) (grammar-nt-cnt x)))
    (setf (grammar-sht x) sht)))

(defun grammar-symbol2id (s x)
  (sht-rec-id (gethash s (grammar-sht x))))

(defun grammar-id2symbol (id x) ;TODO: pointer arrays might be faster
  (if (< id (grammar-nt-cnt x)) (nth id (grammar-nts x))
    (nth (- id (grammar-nt-cnt x)) (grammar-ts x))))

;;========================== GRAMMAR PRE-FILTERING

(defun get-live-nts (x)
  "Return list of nonterminals that can be used in producing terminal strings"
  (let (res newres)
    (loop ; repeat while producing new results
      (setq newres nil)
      (dolist (rule (grammar-rules x)) ; from every rule
	(when (and (not (rule-status rule)) ; that is active
		   (not (member (rule-lhs rule) res)) ; and LHS is not yet selected
		   (every (f/l (s) (or (member s (grammar-ts x)) ; and RHS contains only terminals or live NTs
				       (member s res)))
			  (rule-rhs rule)))
	  (setq newres t)
	  (push (rule-lhs rule) res))) ; select LHS
      (unless newres (return (reverse res))))))

(defun get-reachable-nts (x)
  "Return list of non-terminals that are reachable from the starting symbol"
  (let ((res (list (grammar-start x))) newres)
    (loop
      (setq newres nil)
      (dolist (rule (grammar-rules x))
	(when (and (not (rule-status rule))
		   (member (rule-lhs rule) res))
	  (dolist (s (rule-rhs rule))
	    (when (and (not (member s res))
		       (member s (grammar-nts x)))
	      (setq newres t)
	      (push s res)))))
      (unless newres (return (nreverse res))))))


(defun grammar-update-status (x positive status update-cnt)
  "Given the 'disabled' status on symbols and rules, mark 'dead' and 'unreachamble' symbols and rules"
  (when update-cnt
    (setf (grammar-rule-status-cnt x) (make-array status-width :initial-element 0))
    (setf (grammar-nt-status-cnt x) (make-array status-width :initial-element 0))
    (setf (grammar-t-status-cnt x) (make-array status-width :initial-element 0)))
  (let ((negative (set-difference-equal (grammar-nts x) positive)))   
    (when (or update-cnt negative)
      (dolist (rule (grammar-rules x)) ; update rules
	(when (and (not (rule-status rule))
		   (or (member (rule-lhs rule) negative)
		       (intersection-equal (rule-rhs rule) negative)))
	  (setf (rule-status rule) status))
	(when update-cnt
	  (incf (aref (grammar-rule-status-cnt x) (or (rule-status rule) 0)))))
      (maphash (f/l (s srec) 
		    (when (and (not (sht-rec-status srec)) ; update symbols
			       (member s negative))
		      (setf (sht-rec-status srec) status))
		    (when update-cnt 
		      (incf (aref (if (< (sht-rec-id srec) (grammar-nt-cnt x)) ; select which counter to update
					   (grammar-nt-status-cnt x) (grammar-t-status-cnt x))
				  (or (sht-rec-status srec) 0)))))
	       (grammar-sht x)))))

(defun grammar-clear-status (x)
  "Clear 'dead' and 'unreachable' status of symbols and rules (but not 'disabled' status)"
  (dolist (rule (grammar-rules x)) ; clear status of rules
    (unless (= (rule-status rule) 1) ; except those that are off
      (setf (rule-status rule) nil))) ; (disabled-by-symbol will be identified next in grammar-init0)
  (maphash (f/l (s srec) 
		(unless (= (sht-rec-status srec) 1) ; clear status of symbols except those that are off
		  (setf (sht-rec-status srec) nil))
		(when (sht-rec-types srec) ; clear types of all symbols if types are stored
		  (dotimes (i nttypes-width)
		    (setf (aref (sht-rec-types srec) i) 0))))
	   (grammar-sht x)))

(defun mark-unreachable-ts (x)
  "Mark unreachable terminals"
  (let (srec (u-cnt 0) (on-cnt 0))
    (dolist (s (grammar-ts x)) ; first mark all ts as unreachable
      (unless (member s (grammar-delims x)) ; except the delimiters
	(setq srec (gethash s (grammar-sht x)))
	(unless (sht-rec-status srec)
	  (incf u-cnt)
	  (setf (sht-rec-status srec) 3))))
    (dolist (rule (grammar-rules x)) ; next for all active rules 
      (unless (rule-status rule)
	(dolist (s (rule-rhs rule)) ; for all symbols in the RHS
	  (setq srec (gethash s (grammar-sht x)))
	  (when (and (>= (sht-rec-id srec) (grammar-nt-cnt x)) ; if a terminal
		     (= (sht-rec-status srec) 3)) ; and unreachable
	    (incf on-cnt)
	    (setf (sht-rec-status srec) nil))))) ; mark it as reachable
    (setf (aref (grammar-t-status-cnt x) 0) on-cnt)
    (setf (aref (grammar-t-status-cnt x) 3) (- u-cnt on-cnt))))

(defun mark-rules-disabled-by-symbol (x)
  "Mark rules (with status 4 'disabled-by-symbol) that contain disabled symbols"
  (let ((sht (grammar-sht x)))
    (dolist (rule (grammar-rules x)) ; update rules
      (when (and (null (rule-status rule)) ; rule is enabled
		 (or (= (sht-rec-status (gethash (rule-lhs rule) sht)) 1) ;LHS is off
		     (dolist (s (rule-rhs rule) nil)
		       (when (= (sht-rec-status (gethash s sht)) 1) ;a symbol in RHS is off
			 (return t)))))
	(setf (rule-status rule) 4))))) ;set disaled-by-symbol  


;;===================== NT TYPES DETECTION

;; left- and right-recursive NTs

(defun mark-recursive-nts (x)
  "Mark all NTs that are recursive"
  (let ((sht (grammar-sht x)))
    (dolist (rule (grammar-rules x))
      (when (string= (rule-lhs rule) (car (rule-rhs rule)))
	(setf (aref (sht-rec-types (gethash (rule-lhs rule) sht)) 1) 1)) ; mark as left-recursive: X -> X a
      (when (string= (rule-lhs rule) (car (last (rule-rhs rule))))
	(setf (aref (sht-rec-types (gethash (rule-lhs rule) sht)) 2) 1))))) ; mark as right-recursive: X -> a X

;; nullable NTs

(defun is-terminal-rule (rule x)
  "T if rule contains at least one terminal in RHS"
  (some (f/l (s) (member s (grammar-ts x))) (rule-rhs rule)))

(defun exclude-terminal-rules (x)
  "Get the list of rules that do not contain any terminals"
  (subset (grammar-rules x) (f/l (rule) (not (is-terminal-rule rule x)))))

(defun get-nullable-nts (x)
  "Get list of NTs that can produce empty strings"
   (get-live-nts (make-grammar :ts (grammar-ts x) :nts (grammar-nts x)
                  :rules (exclude-terminal-rules x)
		  :start (grammar-start x))))

(defun mark-nttype (x positive idx)
  "Mark type IDX of symbols from the POSITIVE set"
  (let ((sht (grammar-sht x)))
    (dolist (s positive)
      (setf (aref (sht-rec-types (gethash s sht)) idx) 1))))


;;===================== PREPROCESSING SUMMARY

(defun grammar-init0 (x clear-status) ;TODO: also update counts, set status of symbols and rules
  "Mark and update the status of all symbols and rules (except disabled ones"
  (when clear-status
    (grammar-clear-status x))
  (mark-rules-disabled-by-symbol x) ; mark rules containing user-disabled symbols
  (grammar-update-status x (get-live-nts x) 2 nil) ; mark dead symbols and rules
  (grammar-update-status x (get-reachable-nts x) 3 t) ; mark unreachable NTs and rules
  (mark-unreachable-ts x) ; mark unreachable terminals
  (setf (grammar-nullable-nts x) (get-nullable-nts x)) ; mark nullable NTs
  (mark-nttype x (grammar-nullable-nts x) 0) 
  (mark-recursive-nts x) ; mark recursive NTs
  (setf (grammar-init1-flag x) nil)) ; reset the flag for INIT1

;;======================= RELATIONS ON SYMBOLS

(defun build-begins-directly-with-bm (x)
  (let ((res (make-nma 0 (list (grammar-nt-cnt x) (grammar-symbol-cnt x))))
	lhs-id)
    (dolist (rule (grammar-rules x) res)
      (unless (rule-status rule)
	(setq lhs-id (grammar-symbol2id (rule-lhs rule) x))
	(dolist (s (rule-rhs rule))
	  (nma-set res (list lhs-id (grammar-symbol2id s x)) 1)
	  (unless (member s (grammar-nullable-nts x)) (return t)))))))

(defun build-is-direct-end-of-bm (x)
  (let ((res (make-nma 0 (list (grammar-symbol-cnt x) (grammar-nt-cnt x))))
	lhs-id)
    (dolist (rule (grammar-rules x) res)
      (unless (rule-status rule)
	(setq lhs-id (grammar-symbol2id (rule-lhs rule) x))
	(dolist (s (reverse (rule-rhs rule)))
	  (nma-set res (list (grammar-symbol2id s x) lhs-id) 1)
	  (unless (member s (grammar-nullable-nts x)) (return t)))))))

(defun build-is-followed-directly-by-bm (x)
  (let ((res (make-nma 0 (list (grammar-symbol-cnt x) (grammar-symbol-cnt x))))
	rhs0 id0)
    (dolist (rule (grammar-rules x) res)
      (unless (rule-status rule)
	(setq rhs0 (rule-rhs rule))
	(while (cdr rhs0)
	  (setq id0 (grammar-symbol2id (car rhs0) x))
	  (dolist (s1 (cdr rhs0))
	    (nma-set res (list id0 (grammar-symbol2id s1 x)) 1)
	    (unless (member s1 (grammar-nullable-nts x)) (return t)))
	  (setq rhs0 (cdr rhs0)))))))

(defun grammar-update-follow-sets (x delims allsymbols)
  (let ((startid (grammar-symbol2id (grammar-start x) x))
	(nt-cnt (grammar-nt-cnt x)))    
    (maphash (f/l (s srec) 
		  (let ((sid (grammar-symbol2id s x)))
		    (when (or allsymbols (< sid nt-cnt)) ; only FOLLOW(nonterminals)
		      (setf (sht-rec-follow srec) 
			    (if (> (nma-elt (grammar-is-end-of x) (list sid startid)) 0)
				(cons "-!" delims) nil))
		      (dotimes (i (grammar-t-cnt x)) ; only with terminals
			(when (> (nma-elt (grammar-is-followed-by x) (list sid (+ i nt-cnt))) 0)
			  (push (grammar-id2symbol (+ i nt-cnt) x) (sht-rec-follow srec)))))))
	     (grammar-sht x))))

(defun grammar-init1 (x)
  "Build 'is-followed-by' relation, along with 'is-begins-with', 'is-end-of', and their non-transitive precursors,
   Populate the FOLLOW sets for all symbols"
  (let (followed-by0 followed-by1)
    (setf (grammar-begins-with x) (build-begins-directly-with-bm x))
    (nmau-reflexive-closure (grammar-begins-with x))
    (nmau-transitive-closure (grammar-begins-with x))
    (setf (grammar-is-end-of x) (build-is-direct-end-of-bm x))
    (nmau-reflexive-closure (grammar-is-end-of x))
    (nmau-transitive-closure (grammar-is-end-of x))
    (setq followed-by0 (build-is-followed-directly-by-bm x))
    (setq followed-by1 (nma-boolean-product (grammar-is-end-of x) followed-by0 1))
    (setf (grammar-is-followed-by x) (nma-boolean-product followed-by1 (grammar-begins-with x) 1))
    (grammar-update-follow-sets x nil t))
  (setf (grammar-init1-flag x) t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; ACTIVE RULES AND SYMBOLS

(defun grammar-add-active-symbol (x i s)
  "Add active symbol S to pre-allocated (ACTIVE-SYMBOLS X)"
  (let (srec)
    (setq srec (gethash s (grammar-sht x)))
    (unless (sht-rec-status srec)
      (setf (sht-rec-active-id srec) i)
      (setf (aref (grammar-active-symbols x) i) (cons s srec))
      (incf i))
    i))

(defun grammar-init-active (x)
  "Populate (ACTIVE-RULES X), along with their ID-s,  and (ACTIVE-SYMBOLS X), with their active-ids"
  (let ((i 0) (cnt 0) (ruleno 1) srec)
    (dolist (rule (grammar-rules x)) ; get number of active rules
      (unless (rule-status rule)
	(incf cnt)))
    (setf (grammar-active-rules x) (make-array cnt)) ; create active-rules array
    (dolist (rule (grammar-rules x)) 
      (unless (rule-status rule)
	(setf (rule-id rule) ruleno)
	(setf (aref (grammar-active-rules x) i) rule)
	(incf i))
      (incf ruleno))
    (setq cnt 0) ; get the number of active symbols
    (maphash (f/l (s srec1) (unless (sht-rec-status srec1) (incf cnt))) 
	     (grammar-sht x))
    (setf (grammar-active-symbols x) (make-array cnt)) ; creacte active-symbols array
    (setq i 0)
    (dolist (s (grammar-nts x)) ; add active NTs
      (setq i (grammar-add-active-symbol x i s)))
    (setf (grammar-active-nt-cnt x) i) ; remember count of active NTs
    (dolist (s (grammar-ts x)) ; add active Ts
      (setq i (grammar-add-active-symbol x i s)))))

(defun grammar-active-ruleno2ruleno (ruleno x)
  "Convert active rule no to original rule no"
  (if (= ruleno 0) 0 ; reserved rule number of initial occurence
    (rule-id (aref (grammar-active-rules x) (1- ruleno)))))

(defun grammar-symbol2active-id (s x)
  "Get active id of a symbol"
  (if (string= s "-!") -1
    (let ((srec (gethash s (grammar-sht x))))
      (if (sht-rec-status srec) (error (concat "Symbol " s " is not active!"))
	(sht-rec-active-id srec))))) ;return ACTIVE ID (or ID, if ATCTIVE-ID is not initialized) for active symbols

(defun grammar-symbols2active-id (symbols x)
  "Convert list of symbols to list of their active ids"
  (mapcar (f/l (s) (grammar-symbol2active-id s x)) symbols)) 

(defun grammar-active-id2srec (sid x)
  "Get symbol record given its active id"
  (cdr (aref (grammar-active-symbols x) sid)))
  

(defun grammar-active-id2sid (sid x)
  "Convert symbols active id to original id"
  (if (= sid -1) -1
    (sht-rec-id (grammar-active-id2srec sid x))))


;;;;;;;;;;;;;;;;;;;;;;;;; OCCURENCE MAP

(defun o-count (x) 
  "Count the number of occurences (in active rules)"
  (let ((res 0))
    (dolist (rule (grammar-rules x) res)
      (unless (rule-status rule)
	(incf res (length (rule-rhs rule)))))))

(defun grammar-init-occurence-map (x)
  "Initialize map of occurences"
  (setf (grammar-occurence-map x) (make-array (1+ (o-count x))))
  (let ((o-no 1) (ruleno 0) rule pos)
    (setf (aref (grammar-occurence-map x) 0) (make-occurence :ruleno 0 :pos 0 ; Add initial occurence
							     :symbol-id (grammar-symbol2active-id (grammar-start x) x)))
    (dotimes (i (length (grammar-active-rules x))) ; add occurences for every active rule
      (setq rule (aref (grammar-active-rules x) i))
      (setq pos (length (rule-rhs rule))) ; 1-based, right-to-left
      (dolist (s (rule-rhs rule))
	(setf (aref (grammar-occurence-map x) o-no) 
	      (make-occurence :ruleno (1+ i) :pos pos 
			      :symbol-id (grammar-symbol2active-id s x)))
	(incf o-no) ; 0-based, with occurence 0 being the initial occurence
	(decf pos)))))

(defun o-id2active-sid (o-id x)
  "Convert occurence ID to active symbol ID"
  (occurence-symbol-id (aref (grammar-occurence-map x) o-id)))

(defun o-id2symbol (o-id x)
  "Get symbol by occurence ID"
  (car (aref (grammar-active-symbols x) (o-id2active-sid o-id x)))) 

(defun o-to-list (o x)
  "Get (SYMBOL ACTIVE-RULENO POS) list representation of occurence record"
  (list (grammar-id2symbol (occurence-symbol-id o) x) (occurence-ruleno o) (occurence-pos o))) 

(defun o-id-to-list (o-id x)
  "Get (SYMBOL ACTIVE-RULENO POS) list representation of occurence by occurence ID"
  (cons o-id (o-to-list (aref (grammar-occurence-map x) o-id) x)))

;;;;;;;;;;;;;;;;;;;;; RELATIONS ON OCCURENCES

(defun grammar-active-nonnull-rules-cnt (x)
  "Count non-nullifying active rules"
  (let ((cnt 0))
    (dolist (rule (grammar-rules x) cnt)
      (unless (or (rule-status rule) (null (rule-rhs rule)))
	(incf cnt)))))

(defun build-o-begins-directly-with (x)
  "Return list of BDW[NTs,Rules] boolean matrix of BDW relationship on occurences,
  nt-map[Rules] array with column-to-NT map
  o-map[Rules] array with column-to-occurence map"
  (let* ((rule-cnt (grammar-active-nonnull-rules-cnt x))
	 (bdw (make-nma 0 (list (grammar-active-nt-cnt x) rule-cnt))) 
	 (nt-map (make-nma 0 (list rule-cnt)))
	 (o-map (make-nma 0 (list rule-cnt)))
	 (o-no 1) (i 0) pos)
    (dolist (rule (grammar-rules x))
      (unless (or (rule-status rule) (null (rule-rhs rule)))
	(nma-set bdw (list (grammar-symbol2active-id (rule-lhs rule) x) i) 1) ; only one 1 per column
	(nma-set nt-map (list i) (grammar-symbol2active-id (car (rule-rhs rule)) x))
	(nma-set o-map (list i) o-no)	
	(incf o-no (length (rule-rhs rule)))
	(incf i)))
    (list bdw nt-map o-map))) 

			  
(defun grammar-update-o-begins-with (x)
  "Store transitive closure 'O-BEGINS-WITH' together with column-to-occurence map"
  (let ((o-bdw-res (build-o-begins-directly-with x)))
    (setf (grammar-o-bw x) (nmau-cr-transitive-closure (first o-bdw-res) (second o-bdw-res)))
    (setf (grammar-o-bw-map x) (third o-bdw-res))))

(defun nt-get-o-bw-row (s x)
  "Get occurences Y (as integers) where S O-BEGINS-WITH Y"
  (let ((i (grammar-symbol2active-id s x))
	(o-bw (grammar-o-bw x)))	  
    (when (< i (nma-dim o-bw 0))
      (do ((j (1- (nma-dim o-bw 1)) (1- j))
	   (res nil))
	  ((< j 0) res)
	(when (> (nma-elt o-bw (list i j)) 0)
	  (push (nma-elt (grammar-o-bw-map x) (list j)) res))))))

(defun occurence-get-o-below-row (o-id x)  
  "Get occurences Y (as integers) where O O-BELOW Y, given occurence O id"
  (if (= o-id -1)
      (cons 0 (nt-get-o-bw-row (grammar-start x) x) x)
    (let* ((o (aref (grammar-occurence-map x) o-id))
	   (rule (when (> (occurence-ruleno o) 0) 
		   (aref (grammar-active-rules x) (1- (occurence-ruleno o)))))
	   (next (when rule 
		   (second (nthcdr (- (length (rule-rhs rule)) (occurence-pos o)) (rule-rhs rule))))))
      (when next
	(cons (1+ o-id) (nt-get-o-bw-row next x))))))

(defun group-occurences-by-symbol (o-ids row x)
  "Push occurence  ids into cells of ROW corresponding to symbol ids"
  (let (sid)
    (dolist (o-id o-ids)
      (setq sid (o-id2active-sid o-id x))
      (unless (member o-id (aref row sid))
	(push o-id (aref row sid))))))

(defun grammar-clear-key-lists (x)
  "Clear cached keys on symbols SHT records"
  (maphash (f/l (s srec) (setf (sht-rec-keys srec) nil)) (grammar-sht x)))

(defun grammar-build-pt (x)
  "Build hash-table with key id as key and key record as value, populate KEY property with list of occurences,
   fill PT-ROW with PushTable row, with an column for every active symbol"
  (let ((active-symbol-cnt (length (grammar-active-symbols x)))
	(key-cnt 1)
	(newkeys '(((-1) . 0)))
	key row pt-row newkey newkey-id srec krec)
    (setf (grammar-keys x) (make-hash-table :test #'equal))
    (setq row (make-array active-symbol-cnt)) ; buffer row containing sets of occurences for each active symbol
    (while newkeys
      (setq key (pop newkeys)) ; key is a (list-of-occurences . id)
      (setq pt-row (make-nma 0 (list active-symbol-cnt))) ; row to be stored in pustable
      (dolist (o (car key)) ; for every occurence O in the key
	(group-occurences-by-symbol (occurence-get-o-below-row o x) row x)) ; group all occurences O' (such that O OBELOW O') by symbol
      (dotimes (i active-symbol-cnt) 
	(setq newkey (aref row i))
	(when newkey ; for every symbol that contains non-empty key
	  (setq newkey (sort newkey '<)) ; sort the key
	  (setq srec (cdr (aref (grammar-active-symbols x) i))) ; get the symbol record
	  (setq newkey-id (cdr (assoc newkey (sht-rec-keys srec)))) ; check if key is already pushed into symbol record
	  (unless newkey-id
	    (setq newkey-id key-cnt)
	    (incf key-cnt)
	    (push (cons newkey newkey-id) (sht-rec-keys srec))
	    (push (cons newkey newkey-id) newkeys))
	  (nma-set pt-row (list i) newkey-id) ; store the newkey-id in pushtabe
	  (setf (aref row i) nil))) ; clear buffer row
      (setq krec (make-key-rec :key (car key) :pt-row pt-row))
      (setf (gethash (cdr key) (grammar-keys x)) krec))
    key-cnt)) ; return number of keys and push-table

(defun grammar-compile (x)
  "Build PT of a grammar, with all necessary preparations starting from INIT1"
  (unless (grammar-init1-flag x)
    (grammar-init1 x))
  (grammar-init-active x)
  (grammar-init-occurence-map x)
  (grammar-clear-key-lists x)
  (grammar-update-o-begins-with x)
  (grammar-build-pt x))

(defun sid2string (sid x)
  "Translate active-symbol id to string"
  (if (= sid -1) "-!"
    (mkstring (car (aref (grammar-active-symbols x) sid)))))

(defun occurence-rp2string (o)
  "Translate ruleno and pos from occurence record to string"
  (concat "-" (mkstring (occurence-ruleno o)) "." (mkstring (occurence-pos o))))  

(defun key2string (o-ids x)
  "Represent key (given as list of occurences) as string"
  (if (= (car o-ids) -1) "^" 
    (let* ((o0 (aref (grammar-occurence-map x) (car o-ids)))
	   (res (concat (sid2string (occurence-symbol-id o0) x) (occurence-rp2string o0))))
      (dolist (o-id (cdr o-ids) res)
	(setq res (concat res (occurence-rp2string (aref (grammar-occurence-map x) o-id))))))))


(defun action2string (action)
  (selectq action
	   (t "ACCEPT")
	   (0 "SHIFT")
	   (concat "REDUCE(" action ")"))) ; TODO: should map to original rule numbers

(defun get-rule-prec (ruleno x)
  (rule-prec (aref (grammar-active-rules x) (1- ruleno))))


(defun add-ct-group (kid krec kind sids x report-conflict-fn)
  "Add group of inputs identifide by active symbol ids to KREC of the control table, use group KIND and key id,
   if REPORT-CONFLIC-FN is provided, report all conflits through it, othewise raise error on first conflict"
  (let (conflicting-inputs conflict-str)
    (dolist (g (key-rec-ct-groups krec))
      (setq conflicting-inputs (intersection-equal (cdr g) sids))
      (when (and conflicting-inputs (numberp kind) (numberp (car g))) ; try to resolve s/r and r/r conflicts
	(if (and (> kind 0) (> (car g) 0)) ; if reduce-reduce conflict
	    (let* ((new-pr (car (get-rule-prec kind x)))
		   (cur-pr (when new-pr (car (get-rule-prec (car g) x)))))
	      (when (and cur-pr (not (= new-pr cur-pr))) ; both precedences are defined and not equal
		(when (< new-pr cur-pr) (setq sids nil)) ; do not add if current group has greater precedence
		(setq conflicting-inputs nil))) ; no conflict to report
	  (let* ((reduce-ruleno (if (= kind 0) (car g) kind)) ; shift-reduce conflict
		 (reduce-pr (car (get-rule-prec reduce-ruleno x)))
		 symbol-prec shift-group reduce-group new-conflicting-inputs)	    
	    (when reduce-pr
	      (setq shift-group (set-difference-equal (if (= kind 0) sids (cdr g)) conflicting-inputs))
	      (setq reduce-group (set-difference-equal (if (= kind 0) (cdr g) sids) conflicting-inputs))
	      (dolist (sid conflicting-inputs)
		(setq symbol-prec (sht-rec-prec (grammar-active-id2srec sid x)))
		(when symbol-prec
		  (cond ((> (car symbol-prec) reduce-pr)
			 (push sid shift-group))
			((< (car symbol-prec) reduce-pr)
			 (push sid reduce-group))
			((eq (cdr symbol-prec) :left)
			 (push sid reduce-group))
			((eq (cdr symbol-prec) :right)
			 (push sid shift-group))))		
		(unless (or (= sid (car shift-group))
			    (= sid (car reduce-group)))
		  (push sid new-conflicting-inputs)))
	      (setq sids (if (= kind 0) shift-group reduce-group)) ; insert this subset of sids
	      (rplacd g (if (= kind 0) reduce-group shift-group)) ; keep this subset of sids in the current group
	      (setq conflicting-inputs new-conflicting-inputs))))) ; report any remaining sids
      (when conflicting-inputs
	(setq conflict-str (concat (action2string kind) " vs. " (action2string (car g))))
	(if report-conflict-fn (funcall report-conflict-fn x kid conflicting-inputs conflict-str)
	  (error (concat "Conflict at state " (key2string (key-rec-key krec) x) " on symbols " 
			 (mapcar (f/l (sid) (sid2string sid x)) conflicting-inputs)
			 " : " conflict-str)))))
    (when sids		   
      (push (cons kind sids) (key-rec-ct-groups krec)))))

(defun grammar-build-ct-groups (kid krec x report-conflict-fn)
  "Build all Control Table groups for row in KREC"
  (let ((active-t-cnt (- (length (grammar-active-symbols x)) (grammar-active-nt-cnt x)))
	i sids o rule lhs-sid)
    (dotimes (t-i active-t-cnt) ; 1. collect terminals with non-empty PT cells
      (setq i (+ (grammar-active-nt-cnt x) t-i))
      (when (> (nma-elt (key-rec-pt-row krec) (list i)) 0)
	(push i sids)))
    (when sids 
      (add-ct-group kid krec 0 sids x report-conflict-fn)) ; add them SHIFT group
    (dolist (o-id (key-rec-key krec))
      (cond ((= o-id 0) ; 2. if initial occurence found
	     (add-ct-group kid krec t (cons -1 (grammar-symbols2active-id (grammar-delims x) x)) x report-conflict-fn)) ; add ACCEPT group for end-marker and DELIMS
	    ((and (> o-id 0) (= (occurence-pos (setq o (aref (grammar-occurence-map x) o-id))) 1)) ; 3. if rightmost occurence found for rule R
	     (let* ((ruleno (occurence-ruleno o)) ; TODO: should map to original rule numbers
		    (r (aref (grammar-active-rules x) (1- ruleno)))
		    (lhs-srec (gethash (rule-lhs r) (grammar-sht x))))
	       ; add REDUCE group with FOLLOW set of lhs symbol of R
	       (add-ct-group kid krec ruleno (grammar-symbols2active-id (sht-rec-follow lhs-srec) x) x report-conflict-fn))))) 
    (dotimes (ruleno (length (grammar-active-rules x))) ; check all rules
      (setq rule (aref (grammar-active-rules x) ruleno))
      (when (and (null (rule-rhs rule)) ; 4. if a nullifying rule
		 (> (nma-elt (key-rec-pt-row krec) (list (setq lhs-sid (grammar-symbol2active-id (rule-lhs rule) x)))) 0)) ; has its lhs symbol pushable
	; add REDUCE group with FOLLOW set of that lhs symbol
	(add-ct-group kid krec (1+ ruleno) (grammar-symbols2active-id (sht-rec-follow (cdr (aref (grammar-active-symbols x) lhs-sid))) x) x report-conflict-fn))))) 

(defun grammar-build-ct (x report-conflict-fn) 
  "Buld Control Table groups for all keys,
   if REPORT-CONFLIC-FN is provided, report all conflits through it, othewise raise error on first conflict"
  (maphash (f/l (kid krec) (grammar-build-ct-groups kid krec x report-conflict-fn))
	   (grammar-keys x)))



; (grammar-build-ct x)
  
;(setq *report-conflict-fn* (f/l (x kid inputs conflict-str) 
;				(let ((krec (gethash kid (grammar-keys x))))
;				  (print (concat "Conflict at state " (key2string (key-rec-key krec) x) " on symbols " 
;						 (mapcar (f/l (sid) (sid2string sid x)) inputs)
;						 " : " conflict-str)))))
    

; (grammar-build-ct x)

; (maphash (f/l (k v) (print (list k (key-rec-key v) (key-rec-ct-groups v)))) (grammar-keys x))


    
		   
    
    


; (load "ex6.lsp")

; (grammar-init0 x)

; (grammar-init1 x)

; (grammar-init-active x)

; (grammar-init-occurence-map x)

; (grammar-update-o-begins-with x)


; (grammar-clear-key-lists x)

; (grammar-build-pt x)

; (maphash (f/l (k v) (print (list k (key-rec-key v) (key-rec-pt-row v)))) (grammar-keys x))

; (grammar-build-ct x)
	   

