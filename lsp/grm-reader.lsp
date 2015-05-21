(defun Get_ssdm_version+ (fno res)
  (osql-result _ssdm_version_))

(osql "
create function Get_ssdm_version() -> Number
  as foreign 'Get_ssdm_version+';
")

(defvar _as_strings_ t)

(defstruct exported-grammar start-line end-line g (rule-cnt 0) comments)

(defun preread-grammar (filename)
  "Identifies symbols assigned to quoted lists in given lisp file, collects comments
   return an alist of (grammar-symbol . exported-grammar) where the latter is partly initialized"
  (let ((str (openstream filename "r")) res line (line-cnt 0) 
	token (state :before) grm-symbol eg (par-cnt 0) comment-buffer comment-list)
    (unwind-protect
	(do () (nil t) 
	  (setq line (read-line str))	
	  (incf line-cnt)
	  (if (eq line '*eof*) (return nil)
	    (with-textstream line-str line
			     (do () (nil t)
			       (setq token (read-token line-str))
			       (if (or (eq token '*eof*)
				       (and (not (eq state :read-rules)) (stringp token) (string= token ";"))) (return nil)
				 (selectq state
					  (:before (when (and (stringp token) (string= token "(")) 
						     (setq state :expect-assignment)))
					  (:expect-assignment (if (member token '("defvar" "defparameter" "setq"))
								  (progn  ;start constructing a new grammar symbol
								    (setq eg (make-exported-grammar :start-line line-cnt))
								    (setq comment-list nil)
								    (setq comment-buffer "")
								    (setq par-cnt 0)
								    (setq state :expect-grm-symbol))
								(setq state :before)))
					  (:expect-grm-symbol (if (stringp token) 
								  (progn
								    (setq grm-symbol token)
								    (setq state :expect-quote))
								(setq state :before)))
					  (:expect-quote (setq state (if (and (stringp token) (string= token "'")) 
									 :expect-left-par :before)))
					  (:expect-left-par (setq state (if (and (stringp token) (string= token "(")) 
									    :read-rules :before)))
					  (:read-rules (when (stringp token)
							 (cond ((and (string= token ";") (= par-cnt 0))  ;add comments to COMMENT-BUFFER, next token is *EOF*
								(setq comment-buffer (concat comment-buffer 
											     (if (string= comment-buffer "") "" "|") 
											     (read-line line-str))))
							       ((string= token "(") 
								(when (= par-cnt 0) 
								  (when (> (exported-grammar-rule-cnt eg) 0) 
								    (push comment-buffer comment-list)
								  (setq comment-buffer "")))
								(incf par-cnt))
							       ((string= token ")") 
								(decf par-cnt)
								(cond ((= par-cnt 0) 
								       (incf (exported-grammar-rule-cnt eg)))
								      ((= par-cnt -2) 
								       (push comment-buffer comment-list) ;store comment on the last rule
								       (setf (exported-grammar-end-line eg) line-cnt)
								       (setf (exported-grammar-comments eg) (nreverse comment-list))
								       (push (cons grm-symbol eg) res)
								       (setq state :before)))))))
					  t))))))
      (closestream str))
    (nreverse res)))

;; (preread-grammar "../test-grammar.lsp")

(defvar *grm-symbols*)

(defun symbol-to-grammar (symbol)
  (cdr (assoc symbol *grm-symbols*)))

(defun Preread_grammar-++ (fno filename symbol rulecnt)
  (setq *grm-symbols* (preread-grammar filename))
  (dolist (entry *grm-symbols*)
    (osql-result filename (car entry) (exported-grammar-rule-cnt (cdr entry)))))

(osql "
create function Preread_grammar(Charstring filename) -> Bag of (Charstring symbol, Integer rulecnt)
  as foreign 'Preread_grammar-++';
")

(defun load-lines (filename start-line end-line)
  "Loads lisp forms from FILENAME starting at START-LINE and ending at END-LINE (1-based)"
  (let ((instr (openstream filename "r")) line (linecnt 0))
    (unwind-protect
	(parse (with-string s (do () (nil t)
				(setq line (read-line instr))
				(incf linecnt)
				(if (or (eq line '*eof*) (> linecnt end-line)) (return t)
				  (when (>= linecnt start-line)
				    (formatl s line t)))))
	       t "lisp")
      (closestream instr))))

(defun Load_grammar-- (fno filename symbol)
  (let ((eg (symbol-to-grammar symbol)) g)
    (load-lines filename (exported-grammar-start-line eg) (exported-grammar-end-line eg))
    (setq g (eval `(grammar-from-johnsons ,(mksymbol symbol))))
    (setf (exported-grammar-g eg) g)   
    (setq _as_strings_ (stringp (rule-lhs (car (grammar-rules g)))))
;    (grammar-init-sht g)
    (grammar-init0 g nil)
    ))

(osql "
create function Load_grammar(Charstring filename, Charstring symbol) -> Boolean
  as foreign 'Load_grammar--';
")

(defun Get_grammar_rules-++++ (fno symbol lp rp status comment)
  (let ((eg (symbol-to-grammar symbol)) (ruleno 0))
    (dolist (rule (grammar-rules (exported-grammar-g eg)))
      (osql-result symbol (mkstring (rule-lhs rule)) 
		   (listtoarray (mapcar #'mkstring (rule-rhs rule))) 
		   (or (rule-status rule) 0)
		   (nth ruleno (exported-grammar-comments eg)))
      (incf ruleno))))

(defun Get_grammar_symbols--++++ (fno symbol terminals s id status types)
  (let ((g (exported-grammar-g (symbol-to-grammar symbol))) srec)
    (dolist (s (if (> terminals 0) (grammar-ts g) (grammar-nts g)))
      (setq srec (gethash s (grammar-sht g)))
      (osql-result symbol terminals (mkstring s) (sht-rec-id srec)
		   (or (sht-rec-status srec) 0) (sht-rec-types srec)))))

(defun Get_grammar_counts--+ (fno symbol kind counts)
  (let ((g (exported-grammar-g (symbol-to-grammar symbol))))
    (osql-result symbol kind
		 (selectq kind
			  (1 (grammar-t-status-cnt g))
			  (2 (grammar-nt-status-cnt g))
			  (3 (grammar-rule-status-cnt g))
			  (error "Unknown kind in Get_grammar_counts")))))

(osql "
create function Get_grammar_rules(Charstring symbol) -> Bag of (Charstring lp, Vector of Charstring rp, Integer status, Charstring comment)
  as foreign 'Get_grammar_rules-++++';

create function Get_grammar_symbols(Charstring symbol, Integer terminals) -> Bag of (Charstring s, Integer id, Integer status, Vector of Integer types)
  as foreign 'Get_grammar_symbols--++++';

create function Get_grammar_counts(Charstring symbol, Integer kind) -> Vector of Integer counts
  as foreign 'Get_grammar_counts--+';
")

(defun grammar-get-prev (g x)
  "gets 'directly-follows' set for X, together with respective rule numbers"
  (let ((rulecnt 0) left1 res)
    (dolist (rule (grammar-rules g) res)
      (incf rulecnt)
      (unless (rule-status rule)
	(setq left1 (cdr (member x (reverse (rule-rhs rule))))) 
	(dolist (r left1)
	  (push (cons r rulecnt) res)
	  (unless (member r (grammar-nullable-nts g)) 
	    (return nil)))))))

(defun grammar-get-next (g x)
  "gets 'is-directly-followed-by' set for X, together with respective rule numbers"
  (let ((rulecnt 0) right1 res)
    (dolist (rule (grammar-rules g) res)
      (incf rulecnt)
      (unless (rule-status rule)
	(setq right1 (cdr (member x (rule-rhs rule)))) 
	(dolist (r right1)
	  (push (cons r rulecnt) res)
	  (unless (member r (grammar-nullable-nts g)) 
	    (return nil)))))))

(defun grammar-get-idb (g x)
  "gets 'is-direct-beginning-of' set for X, together with respective rule numbers"
  (let ((rulecnt 0) res)
    (dolist (rule (grammar-rules g) res)
      (incf rulecnt)
      (unless (rule-status rule)
	(dolist (r (rule-rhs rule))
	  (when (equal r x)
	    (push (cons (rule-lhs rule) rulecnt) res))
	  (unless (member r (grammar-nullable-nts g))
	    (return nil)))))))

(defun grammar-get-bdw (g x)
  "gets 'begins-directly-with' set for X, together with respective rule numbers"
  (let ((rulecnt 0) res)
    (dolist (rule (grammar-rules g) res)
      (incf rulecnt)
      (when (and (not (rule-status rule))
		 (equal (rule-lhs rule) x))
	(dolist (r (rule-rhs rule))
	  (push (cons r rulecnt) res)
	  (unless (member r (grammar-nullable-nts g))
	    (return nil)))))))

(defun grammar-get-ide (g x) 
  "gets 'is-direct-end-of' set for X, together with respective rule numbers"
  (let ((rulecnt 0) res)
    (dolist (rule (grammar-rules g) res)
      (incf rulecnt)
      (unless (rule-status rule)
	(dolist (r (reverse (rule-rhs rule))) 
	  (when (equal r x)
	    (push (cons (rule-lhs rule) rulecnt) res))
	  (unless (member r (grammar-nullable-nts g))
	    (return nil)))))))

(defun grammar-get-edw (g x) 
  "gets 'ends-directly-with' set for X, together with respective rule numbers"
  (let ((rulecnt 0) res)
    (dolist (rule (grammar-rules g) res)
      (incf rulecnt)
      (when (and (null (rule-status rule))
		 (equal (rule-lhs rule) x))
	(dolist (r (reverse (rule-rhs rule))) 
	  (push (cons r rulecnt) res)
	  (unless (member r (grammar-nullable-nts g))
	    (return nil)))))))

(defun grammar-get-occurs-in (g x)
  "gets 'occurs-in' set for X, together with respective rule numbers"
  (let ((rulecnt 0) res)
    (dolist (rule (grammar-rules g) res)
      (incf rulecnt)
      (unless (rule-status rule)
	(when (member x (rule-rhs rule))
	  (push (cons (rule-lhs rule) rulecnt) res))))))

(defun grammar-get-contains (g x)
  "gets 'occurs-in' set for X, together with respective rule numbers"
  (let ((rulecnt 0) res this-rule-findings)
    (dolist (rule (grammar-rules g) res)
      (incf rulecnt)
      (when (and (null (rule-status rule))
		 (equal (rule-lhs rule) x))
	(setq this-rule-findings nil)
	(dolist (r (rule-rhs rule))
	  (unless (member r this-rule-findings)
	    (push r this-rule-findings)
	    (push (cons r rulecnt) res)))))))

(defun grammar-produces (g x)
  "gets set of symbols that X might produce"
  (let ((rulecnt 0) res nullable-tail-length this-rule-findings)
    (dolist (rule (grammar-rules g) res)
      (incf rulecnt)
      (when (and (not (rule-status rule))
		 (equal (rule-lhs rule) x))
	(setq nullable-tail-length 0)
	(dolist (r (reverse (rule-rhs rule)))
	  (if (member r (grammar-nullable-nts g))
	      (incf nullable-tail-length)
	    (return nil)))
	(setq this-rule-findings nil)
	(do ((r0 (rule-rhs rule) (cdr r0)))
	    ((or (null r0)) t)
	  (when (and (not (member (car r0) this-rule-findings))
		     (<= (length (cdr r0)) nullable-tail-length))
	    (push (cons (car r0) rulecnt) res)
	    (push (car r0) this-rule-findings))
	  (unless (member (car r0) (grammar-nullable-nts g))
	    (return nil)))))))

(defun grammar-reduces-to (g x)
  "gets set of NTs that might produce X alone"
  (let ((rulecnt 0) res nullable-tail-length)
    (dolist (rule (grammar-rules g) res)
      (incf rulecnt)
      (unless (rule-status rule)
	(setq nullable-tail-length 0)
	(dolist (r (reverse (rule-rhs rule)))
	  (if (member r (grammar-nullable-nts g))
	      (incf nullable-tail-length)
	    (return nil)))
	(do ((r0 (rule-rhs rule) (cdr r0)))
	    ((or (null r0)) t)
	  (when (and (equal (car r0) x)
		     (<= (length (cdr r0)) nullable-tail-length))
	    (push (cons (rule-lhs rule) rulecnt) res)
	    (return nil)) ; do not return many instances of one symbol!
	  (unless (member (car r0) (grammar-nullable-nts g))
	    (return nil)))))))


(defun tag-to-gfn (tag)
  (selectq tag
	   (-2 'grammar-get-prev)
	   (-1 'grammar-get-next)
	   (0 'grammar-get-idb)
	   (1 'grammar-get-ide)
	   (2 'grammar-get-occurs-in)
	   (3 'grammar-reduces-to)
	   (4 'grammar-get-bdw)
	   (5 'grammar-get-edw)
	   (6 'grammar-get-contains)
	   (7 'grammar-produces)
	   t))

(defun group-grammar-set (srs)
  (let (res res-entry)
    (dolist (sr srs)
      (setq res-entry (assoc (car sr) res))
      (if res-entry (push (cdr sr) (cdr res-entry)) ;add rule-number to an existing entry
	(push (list (car sr) (cdr sr)) res))) ;add new entry
    res))


(defun Get_grammar_set---+++ (fno symbol x tag ruleno y ntflag)
  (let ((g (exported-grammar-g (symbol-to-grammar symbol))))
    (dolist (res (nreverse (eval `(,(tag-to-gfn tag) g (if _as_strings_ x (mksymbol x))))))
      (osql-result symbol x tag (cdr res) (mkstring (car res))
		   (if (member (car res) (grammar-nts g)) 1 0)))))

(defun Get_grammar_grouped_set---+++ (fno symbol x tag rulenos y ntflag)
  (let ((g (exported-grammar-g (symbol-to-grammar symbol))))
    (dolist (res (group-grammar-set (eval `(,(tag-to-gfn tag) g (if _as_strings_ x (mksymbol x))))))
      (osql-result symbol x tag (listtoarray (cdr res)) (mkstring (car res))
		   (if (member (car res) (grammar-nts g)) 1 0)))))

(osql "
create function Get_grammar_set(Charstring symbol, Charstring x, Integer settag) -> Bag of (Integer ruleno, Charstring y, Integer ntflag)
  as foreign 'Get_grammar_set---+++';

create function Get_grammar_grouped_set(Charstring symbol, Charstring x, Integer settag) -> Bag of (Vector of Integer rulenos, Charstring y, Integer ntflag)
  as foreign 'Get_grammar_grouped_set---+++';
")

;;NOT USED!
(defun Get_grammar_follows--+ (fno symbol x y)
  (let ((g (exported-grammar-g (symbol-to-grammar symbol))) srec)
    (unless (grammar-init1-flag g)
      (grammar-init1 g))
    (setq srec (gethash (if _as_strings_ x (mksymbol x)) (grammar-sht g)))
    (when srec
      (dolist (r (sht-rec-follow srec))
	(osql-result symbol x (mkstring r))))))

(osql "
create function Get_grammar_follows(Charstring symbol, Charstring x) -> Bag of Charstring y
  as foreign 'Get_grammar_follows--+';
")


(defun Update_grammar_rule--- (fno symbol ruleno status)
  (let* ((g (exported-grammar-g (symbol-to-grammar symbol)))
	 (rule (nth (1- ruleno) (grammar-rules g))))    
    (setf (rule-status rule) status)
    (grammar-init0 g t)))

(osql "
create function Update_grammar_rule(Charstring symbol, Integer ruleno, Integer status) -> Boolean
  as foreign 'Update_grammar_rule---';

/*trace('Update_grammar_rule'); /*DEBUG*/
")

(defun Update_grammar_symbol--- (fno symbol s status)
  (let* ((g (exported-grammar-g (symbol-to-grammar symbol)))
	 (srec (gethash (if _as_strings_ s (mksymbol s)) (grammar-sht g))))
    (setf (sht-rec-status srec) status)
    (grammar-init0 g t)))

(osql "
create function Update_grammar_symbol(Charstring symbol, Charstring s, Integer status) -> Boolean
  as foreign 'Update_grammar_symbol---';
")

(defun Compile_grammar- (fno symbol)
  (let ((g (exported-grammar-g (symbol-to-grammar symbol))))
    (grammar-compile g)))

(osql "
create function Compile_grammar(Charstring symbol) -> Boolean
  as foreign 'Compile_grammar-';
")

(defun Get_grammar_keys-++++ (fno gid kid s rulenos positions)
  (let ((g (exported-grammar-g (symbol-to-grammar gid))))
    (maphash (f/l (kid krec)
		  (if (= kid 0) (osql-result gid 0 "^BOTTOM^" #() #())
		    (let ((occurences (mapcar (f/l (o-id) (aref (grammar-occurence-map g) o-id))
					      (key-rec-key krec))))
		      (osql-result gid kid (mkstring (o-id2symbol (car (key-rec-key krec)) g))
				   (listtoarray (mapcar (f/l (o) (grammar-active-ruleno2ruleno (occurence-ruleno o) g)) occurences))
				   (listtoarray (mapcar #'occurence-pos occurences))))))
	     (grammar-keys g))))

(osql "
create function Get_grammar_keys(Charstring gid) -> Bag of (Integer, Charstring, Vector of Integer, Vector of Integer)
  as foreign 'Get_grammar_keys-++++';
") 

(defun Grammar_build_ct-+++ (fno gid kid inputs conflict-str)
  (let ((g (exported-grammar-g (symbol-to-grammar gid))))
    (grammar-build-ct g (f/l (x kid inputs conflict-str)
			     (osql-result gid kid (listtoarray (mapcar (f/l (sid) (grammar-active-id2sid sid x)) inputs)) conflict-str)))))

(osql "
create function Grammar_build_ct(Charstring gid) -> Bag of (Integer, Vector of Integer, Charstring)
  as foreign 'Grammar_build_ct-+++';
")

(defun Get_grammar_table_header--+ (fno gid kind header)
  (let* ((g (exported-grammar-g (symbol-to-grammar gid)))
	 (header (make-array (+ (length (grammar-active-symbols g)) 
				(if (= kind 0) 0 (- 1 (grammar-active-nt-cnt g)))))))
    (dotimes (i (length header)) 
      (setf (aref header i) (cond ((= kind 0) ; return all active symbols for PT
				   (sht-rec-id (cdr (aref (grammar-active-symbols g) i))))
				  ((= (1+ i) (length header)) ; return -! symbol in the end of CT header
				   -1)
				  (t ; return only active terminals for CT
				   (sht-rec-id (cdr (aref (grammar-active-symbols g) (+ i (grammar-active-nt-cnt g)))))))))
    (osql-result gid kind header)))

(defun Get_grammar_pt_rows-++ (fno gid kid row)
  (maphash (f/l (kid krec) 
		(osql-result gid kid (nma2array (key-rec-pt-row krec))))
	   (grammar-keys (exported-grammar-g (symbol-to-grammar gid)))))

(defun Get_grammar_ct_rows-++ (fno gid kid row)
  (let* ((g (exported-grammar-g (symbol-to-grammar gid)))
	 (active-t-cnt (- (length (grammar-active-symbols g)) (grammar-active-nt-cnt g))))
    (maphash (f/l (kid krec)
		  (setq row (make-array (1+ active-t-cnt)))
		  (dolist (group (key-rec-ct-groups krec))
		    (dolist (sid (cdr group))
		      (push (car group) (aref row (if (< sid 0) active-t-cnt (- sid (grammar-active-nt-cnt g)))))))
		  (dotimes (i (length row)) ; convert row of action lists to row of NIL, single actions or vectors of actions (on conflicts)
		    (cond ((cdr (aref row i))
			   (setf (aref row i) (listtoarray (aref row i))))
			  ((aref row i)
			   (setf (aref row i) (car (aref row i))))))
		  (osql-result gid kid row))
	     (grammar-keys g))))

(osql "
create function Get_grammar_table_header(Charstring gid, Integer kind) -> Vector of Integer
  as foreign 'Get_grammar_table_header--+';

create function Get_grammar_pt_rows(Charstring gid) -> Bag of (Integer, Vector of Integer)
  as foreign 'Get_grammar_pt_rows-++';

create function Get_grammar_ct_rows(Charstring gid) -> Bag of (Integer, Vector)
  as foreign 'Get_grammar_ct_rows-++';
")
						 
		       

;; Preread_grammar("../ex2.lsp");

;; Load_grammar("../ex2.lsp","ex2");


;; Preread_grammar("../gram1.lsp");

;; Load_grammar("../gram1.lsp","gram1");

;; Compile_grammar("gram1");

;; Grammar_build_ct("gram1");


;; Get_grammar_grouped_set("ex2","<EXPR>",1);

;; Preread_grammar("../../../sqond/lsp/sparql-grammar.lsp");
;; Load_grammar("../../../sqond/lsp/sparql-grammar.lsp","sparql-grammar");
;; Get_grammar_grouped_set("sparql-grammar","LEFT-PAR",1);
;; Get_grammar_follows("sparql-grammar", "<NODES>");
;; Get_grammar_follows("sparql-grammar", "STRING");

;; Preread_grammar("../sparql11-grammar.lsp");
;; Load_grammar("../sparql11-grammar.lsp","sparql11-grammar-grammar");
;; Get_grammar_grouped_set("sparql-grammar","LEFT-PAR",1);


;; Preread_grammar("../../../SQoND/lsp/sparql-grammar.lsp");
;; Load_grammar("../../../SQoND/lsp/sparql-grammar.lsp","sparql-grammar");
;; Get_grammar_grouped_set("sparql-grammar","<TRIPLES>",3);
;; Get_grammar_grouped_set("sparql-grammar","<CONDS>",3);


    
      

