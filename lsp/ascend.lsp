;;GRM parser-generator file, adapted for ALisp
;;(C) 2003-2011, Andrej Andrejev

(defvar _accept_empty_input_ t) ;; parser will return NIL if -! or DELIMS is encoutered at initial state (^)

(defstruct grammar ts nts rules actions)             

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

;;===================== Relations on Symbols

;; compare relations

(defun conseq (x y)
   (and (eq (car x) (car y)) (eq (cdr x) (cdr y))))

;; delete from RULES those containing terminals from TS

(defun exclude-terminal-rules (rules ts) ;recursive
   (cond ((null rules) nil) 
         ((do ((right1 (cdar rules) (cdr right1))) ;look for a terminal in right-side
              ((or (null right1) (find (car right1) ts)) (null right1))) ;return T if not found
          (cons (car rules) (exclude-terminal-rules (cdr rules) ts))) ;return rule
         (t (exclude-terminal-rules (cdr rules) ts)))) ;omit rule

;; return all live NTs 
     
(defun get-live-nts (x)
   (do ((found-nt t) (res nil) (cnt 1 (1+ cnt))) ;outer rules cycle
       ((not found-nt) res) ;stop condition
      (setq found-nt nil) ;starting new search
      (do ((rules1 (grammar-rules x) (cdr rules1))) ;inner rules cycle
          ((or found-nt (null rules1)) t)
         (setq found-nt 
           (and (not (find (caar rules1) res)) ;left part not yet in RES 
                (do ((right1 (cdar rules1) (cdr right1))) ;look for NTs in right side
                    ((or (null right1) (and (find (car right1) (grammar-nts x)) ;NT found 
                                            (not (find (car right1) res)))) ;not yet in RES 
                     (null right1))))) ;return T if no new NT found
         (if found-nt (setq res (cons (caar rules1) res))) ;push left part into RES
         )))   

;; return all nullifying NTs 

(defun get-null-nts (x)
   (get-live-nts (make-grammar :ts (grammar-ts x) :nts (grammar-nts x)
                  :rules (exclude-terminal-rules (grammar-rules x) (grammar-ts x))
		  :actions (grammar-actions x))))

;; build BEGINS-DIRECTLY-WITH relation on symbols

(defun build-begins-directly-with (x null-nts) ;nullifying NTs supplied
   (let ((res nil))
      (dolist (rule (grammar-rules x) res) ;rules cycle
         (do ((right1 (cdr rule) (cdr right1)) (was-null-nt t)) ;right-part cycle
             ((or (null right1) (not was-null-nt)) res) ;until the end or first non-nullifying symbol (inclusive)
            (pushnew (cons (car rule) (car right1)) res #'conseq) ;adding relation
            (setq was-null-nt (find (car right1) null-nts)) ;delayed condition
            )))) 

;;Warschall algorithm to produce (reflexive-)transitive closure of a symbol relation

(defun warschall-rte (rel domain &optional reflexive)
   (let ((res rel))
      (dolist (nt domain res)
         (when reflexive (pushnew (cons nt nt) res #'conseq))
         (dolist (c1 res)
            (when (eq (cdr c1) nt)
               (dolist (c2 res)
                  (when (eq (car c2) nt)
                     (pushnew (cons (car c1) (cdr c2)) res #'conseq)
                     )))))))

;; build BEGINS-WITH as reflexive-transitive closure of BEGINS-DIRECTLY-WITH relation over all grammar symbols

(defun build-begins-with (x null-nts) ;nullifying NTs supplied
   (warschall-rte (build-begins-directly-with x null-nts)
     (append (grammar-ts x) (grammar-nts x)) t)) ;unify Ts and NTs

;; build IS-DIRECT-END-OF relation on symbols

(defun build-is-direct-end-of (x null-nts) ;nullifying NTs supplied
   (let ((res nil))
      (dolist (rule (grammar-rules x) res) ;rules cycle
         (do ((right1 (reverse (cdr rule)) (cdr right1)) (was-null-nt t)) ;right-side reverse cycle
             ((or (null right1) (not was-null-nt)) res) ;left to beginning or first non-nullifying symbol (inclusive)
            (pushnew (cons (car right1) (car rule)) res #'conseq) ;add to relation
            (setq was-null-nt (find (car right1) null-nts)) ;deleayed condition
            ))))

;; build IS-END-OF reflexive-transitive closure of IS-DIRECT-END-OF relation over NTs

(defun build-is-end-of (x null-nts) ;nullifying NTs supplied
   (warschall-rte (build-is-direct-end-of x null-nts) (grammar-nts x) t))

;; build subset of superposition of 3 relations for the given set of left symbols

(defun multiply3-left (left-set rel1 rel2 rel3)
   (let ((res nil))
      (dolist (c1 rel1 res)
         (when (find (car c1) left-set)
            (dolist (c2 rel2)
               (when (eq (cdr c1) (car c2))
                  (dolist (c3 rel3)
                     (when (eq (cdr c2) (car c3))
                        (pushnew (cons (car c1) (cdr c3)) res #'conseq)))))))))

;; build IS-FOLLOWED-DIRECTLY-BY relation on symbols

(defun build-is-followed-directly-by (x null-nts) ;nullifying NTs supplied
   (let ((res nil))
      (dolist (rule (grammar-rules x) res) ;rules cycle
         (do ((right1 (cdr rule) (cdr right1))) ;left symbols cycle
             ((null right1) t)
            (do ((right2 (cdr right1) (cdr right2)) (was-null-nt t)) ;right symbols cycle
                ((or (null right2) (not was-null-nt)) t) ;until the end or first non-nullifying symbol (inclusive)
              (pushnew (cons (car right1) (car right2)) res #'conseq) ;add to relation
              (setq was-null-nt (find (car right2) null-nts)) ;delayed stop condition 
              )))))

;; build IS-FOLLOWED-BY relationship for all NTs

(defun build-is-followed-by-nts (x null-nts begins-with is-end-of) ;BEGINS-WITH and IS-END-OF relations supplied
   (multiply3-left (grammar-nts x)
     is-end-of (build-is-followed-directly-by x null-nts) begins-with))

;; return FOLLOW set for given nullifying NT

(defun get-follow-set (x nt is-followed-by is-end-of delims) ;BEFORE and RTE of AT-END relations supplied
   (let ((res nil))
      (when (member-test (cons nt (car (grammar-nts x))) is-end-of #'conseq) 
         (setq res (cons '-! delims))) ;add end-marker and DELIMS
      (dolist (c1 is-followed-by res) ;IS-FOLLOWED-BY relation cycle
          (when (and (eq (car c1) nt) (find (cdr c1) (grammar-ts x))) ;when NT IS-FOLLOWED-BY some terminal
             (setq res (cons (cdr c1) res)))))) ;add that terminal

;; create assoc-list of NEXT sets for each NT

(defun list-follow-sets-complete (x delims)
  (let* ((null-nts (get-null-nts x)) ;nullifying NTs
	 (begins-with (build-begins-with x null-nts)) 
	 (is-end-of (build-is-end-of x null-nts))
	 (is-followed-by (build-is-followed-by-nts x null-nts begins-with is-end-of))
	 (res nil))
    (dolist (nt (grammar-nts x) res)
      (setq res (acons nt (get-follow-set x nt is-followed-by is-end-of delims) res)))))

;;============================ Relations on occurences


;; compare occurences' indexes

(defun n-conseq (x y)
   (and (= (car x) (car y))
        (= (cdr x) (cdr y))))


;; equality on occurences, insensitive to index perpmutation 

(defun o-eq (x y)
   (and (eq (car x) (car y)) ;compare symbols
        (do ((x1 (cdr x) (cdr x1)) (y1 (cdr y) (cdr y1))) ;index cycle
            ((or (null x1) (null y1)
                 (not (member-test (car x1) (cdr y) #'n-conseq)))
             (and (null x1) (null y1)))
           )))

;;compare occurence relations

(defun o-conseq (x y)
  (and (o-eq (car x) (car y)) 
       (o-eq (cdr x) (cdr y))))


;;return all occurences of symbols in SS

(defun list-os (x ss)
  (let ((res (list (cons (car (grammar-nts x)) '((0 . 0)))))
	(rnum 1) pnum)
    (dolist (rule (grammar-rules x) res) ;rules cycle
      (setq pnum 1) ;enumerate positions from 1 right-to-left in each rule
      (dolist (right1 (reverse (cdr rule))) ;positions cycle
	(when (find right1 ss) ;put occurence into result
	  (setq res (cons (list right1 (cons rnum pnum)) res)))
	(setq pnum (1+ pnum)))
      (setq rnum (1+ rnum)))))
    

;; build O-BEGINS-DIRECTLY-WITH relation on occurences

(defun build-o-begins-directly-with (x o-list null-nts) ;occurences supplied, nullifying NTs should be nil
  (let ((res nil) (rnum 1))
    (dolist (rule (grammar-rules x) res) ;rules cycle
      (do ((right1 (cdr rule) (cdr right1)) (was-null-nt t) ;rule right-sides cycle
	   (pnum (length (cdr rule)) (1- pnum))) ;enumerate positions from 1 right-to-left
	  ((or (null right1) (not was-null-nt)) res) ;until first non-nullifying symbol
	(dolist (o o-list) ;rule head's occurences cycle
	  (when (eq (car o) (car rule))
	    (pushnew (cons o (list (car right1) (cons rnum pnum))) res #'o-conseq)))
	(setq was-null-nt (find (car right1) null-nts)))
      (setq rnum (1+ rnum)))))

;;Warschall algorithm to produce (reflexive-)transitive closure of an occurence relation

(defun o-warschall-rte (rel domain &optional reflexive)
   (let ((res rel))
      (dolist (nt domain res)
         (when reflexive (pushnew (cons nt nt) res #'o-conseq))
         (dolist (c1 res)
            (when (o-eq (cdr c1) nt)
               (dolist (c2 res)
                  (when (o-eq (car c2) nt)
                     (pushnew (cons (car c1) (cdr c2)) res #'o-conseq)
                     )))))))

;; builds O-BELOW relation

(defun build-o-below (x o-begins-with null-nts) ;O-BEGINS-WITH supplied, nullifying NTs should be nil
   (let ((res nil) (nt1 (car (grammar-nts x))) (ont1 nil) (rnum 1) lo ro)
      (pushnew (cons (list '^) (list nt1 (cons 0 0))) res #'o-conseq) ;add initial occurence
      (dolist (c1 o-begins-with) ;cycle through transitive closure of O-BEGINS-WITH relation
         (when (if ont1 (o-eq (car c1) ont1) ;condition if initial occurence already found in O-BEGINS-WITH
                  (when (eq (caar c1) nt1) (setq ont1 (car c1)))) ;and if not
            (pushnew (cons (list '^) (cdr c1)) res #'o-conseq))) ;add its O-FIRST set to relation
      (dolist (rule (grammar-rules x) res) ;rules cycle
         (do ((right1 (cdr rule) (cdr right1)) ;outer right-side cycle
               (pnum1 (length (cdr rule)) (1- pnum1))) ;enumerate left-to-right down to 1
             ((null (cdr right1)) t) ;until next-to-last symbol
            (setq lo (list (car right1) (cons rnum pnum1))) ;left occurence in relation
            (do ((right2 (cdr right1) (cdr right2)) (was-null-nt t) ;inner right-side cycle
                 (pnum2 (1- pnum1) (1- pnum2))) ;enumerate left-to-right down to 1
                ((or (null right2) (not was-null-nt)) t) ;until end or first non-nullifying symbol (inclusive)
               (setq ro (list (car right2) (cons rnum pnum2))) ;right occurence in relation
               (pushnew (cons lo ro) res #'o-conseq) ;add "direct" O-BELOW relation
               (when (find (car right2) (grammar-nts x)) ;and, if right occurence is non-terminal 
                  (dolist (c1 o-begins-with) ;cycle through O-BEGINS-WITH
                     (when (o-eq (car c1) ro) ;from the O-FIRST set for the terminal
                        (pushnew (cons lo (cdr c1)) res #'o-conseq)))) ;add all elements to relation as well
               (setq was-null-nt (find (car right2) null-nts)))) ;delayed stop condition for inner cycle 
         (setq rnum (1+ rnum)))))

(defvar *pushtable*)

;; build deterministic *PUSHTABLE*, return list of all keys

(defun build-pushtable (x)
  (let ((o-list (list-os x (grammar-nts x))) ;list occurences for all NTs
	o-begins-directly-with o-begins-with o-below (keys nil) cell)
    (formatl t "Generated " (length o-list) " occurences" t)
    (setq o-begins-directly-with (build-o-begins-directly-with x o-list nil))
    (formatl t "Generated " (length o-begins-directly-with) " O-BEGINS-DIRECTLY-WITH relations (on occurrences)" t)
    (setq o-begins-with (o-warschall-rte o-begins-directly-with o-list))
    (formatl t "Generated " (length o-begins-with) " O-BEGINS-WITH relations (on occurrences)" t)
    (setq o-below (build-o-below x o-begins-with nil))
    (formatl t "Generated " (length o-below) " O-BELOW relations" t)
    (setq *pushtable* nil)
    (do ((to-add (list (list '^))) (row nil nil)) ;pushtable rows cycle
	((null to-add) keys) ;for all generated keys
      (dolist (c1 o-below) ;O-BELOW relation cycle
	(when (and (eq (caar to-add) (caar c1)) ;if occurence has same symbol as key, and
		   (or (null (cdar c1)) ;either left-side has no index
		       (member-test (cadar c1) (cdar to-add)  #'n-conseq))) ;or index is among those of key
	  (setq cell (assoc (cadr c1) row)) ;search for table cell
	  (if cell (unless (member-test (caddr c1) (cdr cell) #'n-conseq) ;if found
		     (setf (cdr cell) (cons (caddr c1) (cdr cell)))) ;add new index
	    (setq row (cons (copy-list (cdr c1)) row))))) ;else add new cell
      (when row (setq *pushtable* (acons (car to-add) row *pushtable*))) ;add row if non-empty
      (setq keys (cons (car to-add) keys)) ;add key to result
      (setq to-add (cdr to-add)) ;remove it from processing list
      (dolist (ms1 row) (unless (member-test ms1 keys #'o-eq)
			  (pushnew ms1 to-add #'o-eq)))) ;add new keys to processing list
    (formatl t "Generated " (length keys) " pushtable keys" t)
    keys))

;; access to PUSHTABLE 

(defun pushtable-ref (table row col)
   (assoc col (cdr (assoc-test row table #'o-eq))))

;; group actions for confilct messages

(defun groupkey-to-string (gk)
  (cond ((eq gk t) "ACCEPT")
	((= gk 0) "SHIFT")
	(t (concat "REDUCE(" gk ")"))))

;; add group to the row of control table

(defun add-group (key body row)
  (if body
      (do ((body1 body (cdr body1)) (res t)) ;group elements cycle
          ((or (null body1) (not res)) ;until end or fault
           (when res (setf (cdr row) (cons (cons key body) (cdr row))) ;add group
		 res)) ;and return RES, else NIL
	(do ((groups1 (cdr row) (cdr groups1))) ;cycle through existing groups
	    ((or (null groups1) (not res)) t) ;until end or fault
	  (when (find (car body1) (cdar groups1)) 
	    (formatl t "Conflict detected at Key = " (car row) ", Input = " (car body1) " : "
		     (groupkey-to-string (caar groups1)) " vs. " (groupkey-to-string key) t)
	    (setq res nil)))) ;fault case
      t)) ;silent on empty group


;; check if X is an SLR(1)-grammar, build *PUSHTABLE*,
;; return control table if positive

(defun is-slr1-grammar (x delims)
   (let ((keys (build-pushtable x)) ;fill *PUSHTABLE*
         (next-sets-list (list-follow-sets-complete x delims)) ;build NEXT sets for NTs
         (res nil))
      (dolist (row *pushtable*) ;(1) cycle through *PUSHTABLE* rows
         (let ((key (car row)) (group nil)) ;row key
            (dolist (cell (cdr row)) ;group construction cycle
               (when (find (car cell) (grammar-ts x)) ;add terminals of non-empty cells in push table 
                  (setq group (cons (car cell) group))))
            (setq res (acons key (list (cons 0 group)) res)))) ;add line with SHIFT group
      (do ((keys1 keys (cdr keys1)) (row nil)) ;(2) keys cycle
          ((or (null keys1) (null res)) res) ;until end or first conflict
         (unless (setq row (assoc-test (car keys1) res #'o-eq)) ;row for this key
            (setq row (list (car keys1))) ;is never NIL,
            (setq res (cons row res))) ;add to RES
         (do ((ncs1 (cdar keys1) (cdr ncs1))) ;(2a) cycle through key's indexes
             ((or (null ncs1) (null res)) t) ;until end or first conflict
            (cond ((= (caar ncs1) 0) ;if key contains initial occurence
		   (unless (add-group t (cons '-! delims) row) ;add ACCEPT group for end-marker and other DELIMS
		     (setq res nil))) ;conflict
                  ((= (cdar ncs1) 1) ;if key contains some rightmost occurence
                   (unless (add-group (caar ncs1) ;add REDUCE group
                            (cdr (assoc (car (nth (1- (caar ncs1)) (grammar-rules x)))
                                   next-sets-list)) row) ;for the NEXT set of left part of corresponding rule
                      (setq res nil))))) ;conflict
         (do ((rules1 (grammar-rules x) (cdr rules1)) ;(2b) cycle through nullifying rules
              (rnum 1 (1+ rnum))) ;enumerating all rules
             ((or (null rules1) (null res)) t) ;until end or conflict
            (when (and (null (cdar rules1)) ;if rule is nullifying
                       (pushtable-ref *pushtable* (car keys1) (caar rules1))) ;and pushtable cell non-empty
               (unless (add-group rnum (cdr (assoc (caar rules1) next-sets-list)) row) ;add REDUCE group
                  (setq res nil))))))) ;conflict

;; control table access

(defun controltable-ref (table row col)
   (do ((groups1 (cdr (assoc-test row table #'o-eq)) (cdr groups1))) ;search in row
       ((or (null groups1) (find col (cdar groups1))) (caar groups1)))) ;until found 
   

;; ascending processor impelemnation utilizing conrol table and push table

(defun ascending-processor (input x controltable pushtable)
   (do ((stack (list (list '^))) (input1 input) (res nil) (act nil)) ;main cycle with stack
       (res res) ;return RES as soon as assigned
      (unless input1 (setq input1 (list '-!))) ;process end-marker as the end of input
      (setq act (controltable-ref controltable (car stack) (car input1))) ;get action from controltable
      (cond ((eq act t) (setq res 'ACCEPT)) ;ACCEPT
            ((numberp act)
             (if (= act 0) 
                (progn ;SHIFT
                  (setq stack (cons (pushtable-ref pushtable (car stack) (car input1)) stack))
                  (setq input1 (cdr input1))) ;feed input	       
                (let ((rule (nth (1- act) (grammar-rules x)))) ;REDUCE (rule number)
                   (dolist (r1 (reverse (cdr rule))) ;remove keys from stack, checking for conformance with right-side
                      (if (eq (caar stack) r1) 
                         (setq stack (cdr stack))
                         (setq res 'INTERNAL-ERROR))) ;not possible
                   (setq stack (cons (pushtable-ref pushtable (car stack) (car rule)) stack)))))
            (t (setq res 'REJECT))))) ;REJECT if no action found in control table

;;========================== convert grammars from Johnson's format

(defun grammar-from-johnsons (j)
  (let ((nts nil) (symbols nil) (rules nil) (actions nil) rp)
    (dolist (rule j)
      (pushnew (car rule) nts #'eq)
      (setq rp (cddr (butlast rule)))
      (dolist (s rp)
	(pushnew s symbols #'eq))
      (push (cons (car rule) rp) rules)
      (let ((action (car (last rule))))
	(if (numberp action) (push action actions)
	  (push (cadr action) actions))))
    (make-grammar :ts (set-difference (nreverse symbols) nts) :nts (nreverse nts) :rules (nreverse rules) :actions (nreverse actions))))

;;======================== CODE GENERATION

(defun nc< (a b) (cond ((< (car a) (car b)) t)
		       ((> (car a) (car b)) nil)
		       (t (< (cdr a) (cdr b)))))

(defun key-to-state (key)
  (apply #'concat (cons (car key) 
			(mapcar (f/l (ncs) (concat "-" (mkstring (car ncs)) "." (mkstring (cdr ncs)))) 
				(sort (cdr key) #'nc<)))))

(defun make-slr1-parser (x delims fn-name output-file verbose)
  (let ((outs (openstream output-file "w")) ct)
    (unwind-protect
	(when (setq ct (is-slr1-grammar x delims))
	  (formatl outs ";;; SLR1-parser generated by GRM Parser Builder" t
		   ";;; (C) 2001-2011, Andrej Andrejev" t t
		   ";;; INPUT-FN is a function returning (terminal . value) pairs" t
		   ";;; or NIL when input is exhausted" t
		   ";;; DATA is any data structure transmitted to the parser and visible in reduce actions" t t
		   "(defun " fn-name " (input-fn data)" t
		   "  (let ((stack '((^))) ; contains pairs (key . value), ^ is bottom-marker" t
		   "        (input (funcall input-fn)) (delayed-input nil))" t
		   "    (flet ((shift (key) ; SHIFT op" t
		   "             (push (cons key (cdr input)) stack) ; push new key with input value" t
		   (if verbose "             (printl 'SHIFT (caar stack) 'VALUE (cdar stack))" "") (if verbose t "")
		   "             (if delayed-input (progn (setq input delayed-input)" t
		   "                                      (setq delayed-input nil)) ; restore input after REDUCE" t
		   "               (setq input (funcall input-fn)))) ; feed input" t
		   "           (reduce (n) ; REDUCE op: returns list of n top values from stack" t
		   "             (let ((values nil))" t
		   "               (dotimes (i n) (push (cdr (pop stack)) values))" t
		   "               (setq delayed-input input) ; non-terminal will be put into INPUT" t 
		   "               values)))" t
		   "      (do () (nil t) ; main cycle - no stop condition" t
		   "       (unless input (setq input (cons '-! nil))) ; end-marker" t
		   "       (selectq (caar stack) ; outer KEY switch" t)
	  (dolist (cline ct)
	    (formatl outs 
		     "         (" (key-to-state (car cline)) t
		     "           (selectq (car input)" t)
	    (dolist (key (cdr (assoc-test (car cline) *pushtable* #'o-eq))) ; pushing non-terminals
	      (when (member (car key) (grammar-nts x))
		(formatl outs "            (" (car key) " (shift '" (key-to-state key) ")) ; REDUCE completion" t)))
	    (dolist (group (cdr cline)) ; translating ACCEPT group
	      (cond ((eq (car group) t)
		     (formatl outs "            (" (cdr group) " (return (cdar stack))) ; ACCEPT op" t))
		    ((numberp (car group))
		     (if (= (car group) 0)
			 (dolist (symbol (cdr group)) ; translating SHIFT group - pushing terminals
			   (formatl outs "            (" symbol " (shift '" 
				    (key-to-state (pushtable-ref *pushtable* (car cline) symbol)) ")) ; SHIFT op" t))
		       (let* ((r (1- (car group))) ; translating REDUCE group
			      (rule (nth r (grammar-rules x))) (action (nth r (grammar-actions x))))
			 (formatl outs 
				  "            (" (cdr group) " ; REDUCE(" (1+ r) ") op" t 
				  "              (setq input (cons '" (car rule) 
				  (if (numberp action) (if (= action 0) " (prog1 nil" (concat " (nth " (1- action))) 
				    (concat " (apply #'" (with-string s (prin1 action s))))
				  " (reduce " ;INPUT is saved to DELAYED-INPUT in REDUCE
				  (1- (length rule)) "))))" (if verbose t ")")
				  (if verbose (concat "              (printl 'REDUCE " (1+ r) " (car input) 'VALUE (cdr input)))") "") t))))))
	    (when (and _accept_empty_input_ (eq (caar cline) '^))
	      (formatl outs "            (" (cons '-! delims) " (return nil)) ; ACCEPT EMPTY INPUT" t))
	    (formatl outs "            (return (list 'SYNTAX-ERROR (caar stack) '" (apply #'append (mapcar #'cdr (cdr cline)))
		     " input))))" t)) ;unexpected input for this state	  
	  (formatl outs "         (return 'INTERNAL-ERROR))))))" t) ;unknown state
	  )
      (closestream outs)) t))  



		 
    
                
   


           