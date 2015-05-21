(load "../ascend.lsp")

(defparameter ebnf-grammar '(
			     (<rules> -> <rule> 1) ;1
			     (<rules> -> <rules> <rule> 2) ;2
			     (<rule> -> left-bracket int right-bracket id cce <right-part> ;3
				     #'(lambda (a b c d e f) (push (make-ebnf-rule :left d :alts f :comment (concat "[" b "]")) (ebnf-parser-data-rules data)) ; push into RULES
					 (let ((ar (ebnf-parser-data-aux-rules data))) ; flush AUX-RULES buffer into RULES
					   (when ar (setf (ebnf-parser-data-rules data) (append ar (ebnf-parser-data-rules data)))
						 (setf (ebnf-parser-data-aux-rules data) nil)))))
			     (<right-part> -> <alternative> 1) ;5
			     (<right-part> -> <alternative> bar <right-part> #'(lambda (a b c) (append a c))) ;6 ; union of alternatives
			     (<alternative> -> <unary> 1) ;7
			     (<alternative> -> <unary> <alternative> #'(lambda (a b) (let ((b-rev (nreverse b)) res) ;8 ; cartesian product of alternatives
										       (dolist (lbranch (nreverse a) res)
											 (dolist (rbranch b-rev)
											   (push (append lbranch rbranch) res))))))
			     (<unary> -> <atom-or-parexpr> 1) ;9
			     (<unary> -> <atom-or-parexpr> question #'(lambda (a b) (cons nil a))) ;10
			     (<unary> -> <atom-or-parexpr> plus #'(lambda (a b) (list (list (make-plus-symbol a data))))) ;11
			     (<unary> -> <atom-or-parexpr> asterisk #'(lambda (a b) (list nil (list (make-plus-symbol a data))))) ;12
			     (<atom-or-parexpr> -> id #'(lambda (a) (list (list a)))) ;13
			     (<atom-or-parexpr> -> string #'(lambda (a) (list (list a)))) ;14
			     (<atom-or-parexpr> -> left-par <right-part> right-par 2) ;15
			     ))

(make-slr1-parser (grammar-from-johnsons ebnf-grammar) nil "ebnf-slr1-parser" "ebnf-slr1.lsp" nil)