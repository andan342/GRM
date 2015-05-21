;;; ============================================================
;;; AMOS2
;;; 
;;; Author: (c) 2010-2014, Andrej Andrejev, UDBL
;;; $RCSfile: yy2grm.lsp,v $
;;; $Revision: 1.9 $ $Date: 2014/06/01 20:11:43 $
;;; $State: Exp $ $Locker:  $
;;;
;;; Description: Bison/Yacc to GRM grammar converter
;;; =============================================================
;;; $Log: yy2grm.lsp,v $
;;; Revision 1.9  2014/06/01 20:11:43  andan342
;;; Made grammar converters loadable on top of SSDM image
;;;
;;;
;;; =============================================================

;; Differences between GRM and Bison/Yacc:
;; - No default behavior on reduce/reduce conflicts
;; - No Generalized LR parsing
;;
;; Grammars valid in Bison/Yacc might be invalid in GRM parser generator.

(unless (boundp '_file_reader_loaded_)
  (load "../file-reader.lsp"))

(unless (boundp 'nlt)
  (load "../parse-utils.lsp"))

;;characters disallowed in lisp symbols
(defparameter lisp-symbol-escapes '(("(" . "^LPAR") (")" . "^RPAR")
				    ("[" . "^LBRACKET") ("]" . "^RBRACKET")
				    ("{" . "^LBRACE") ("}" . "^RBRACE")
				    ("\"" . "^DOUBLEQUOTE") ("'" . "^SINGLEQUOTE") 
				    ("." . "^DOT") ("," . "^COMMA") (":" . "^COLON") (";" . "^SEMICOLON")
				    ("\\" . "^BACKSLASH") (" " . "^SPACE") ("~" . "^TILDE")))

(defun yy-lexer (fr do-lisp-escape)
  "Simple state-machine implementing tokenizer of Yacc grammar files, uses FILE-READER struct"
  (do ((ch nil) (block-count 0) (string-opener nil))
      (nil nil) ; never return normally     
    (setq ch (fr-nextchar fr)) ; read next character
    (selectq (fr-lexerstate fr)
	     (:start (cond 
		      ((eq (fr-data fr) :double-percent-2) (return nil)) ; do not anything after the second double percent
		      ((string= ch "") (return nil))
		      ((string= ch ":") (return '(colon)))
		      ((string= ch "|") (return '(bar)))
		      ((string= ch ";") (return '(semicolon)))
		      ((string= ch "{") (fr-newstate fr :block))
		      ((string= ch "/") (fr-newstate fr :after-slash))
		      ((string= ch "%") (fr-newstate fr :after-percent))
		      ((member ch '("\"" "'")) (progn (setq string-opener ch) 
						      (fr-startbuffer fr 1) 
						      (fr-newstate fr :string)))
		      ((or (basechar-p ch) (digit-p ch) (string= ch "<"))
		       (fr-startbuffer fr 0)
		       (fr-newstate fr :id))))
	     ;; C blocks
	     (:block (cond ; until respective "}", counted
		      ((string= ch "{") (1++ block-count))
		      ((string= ch "/") (fr-newstate fr :block-after-slash))
		      ((string= ch "}") (if (= block-count 0) (fr-newstate fr :start) (1-- block-count)))
		      ((member ch '("\"" "'")) 
		       (setq string-opener ch) 
		       (fr-newstate fr :block-string))))
	     ;; correcttly handle '<%' and '%>' digraphs for inner braces in C blocks
	     (:block-after-less (progn (when (string= ch "%") (1++ block-count)) (fr-newstate fr :block)))
	     (:block-after-percent (progn (if (string= ch ">") (1-- block-count) (fr-pushback fr ch)) 
					  (fr-newstate fr :block)))
	     ;; correctly handle strings in C blocks
	     (:block-string (cond ; until non-escaped doublequote
			     ((string= ch "\\") (fr-newstate fr :block-string-after-backslash))
			     ((string= ch string-opener) (fr-newstate fr :block))))
	     (:block-string-after-backslash (fr-newstate fr :block-string)) ; ignore escaped character	     
	     ;; correctlty handle comments in C blocks
	     (:block-after-slash (cond 
				  ((string= ch "/") (fr-passline fr)) 
				  ((string= ch "*") (fr-newstate fr :block-comment2))
				  (t (fr-newstate fr :block))))
	     (:block-comment2 (when (string= ch "*") (fr-newstate fr :block-comment2-after-asterisk))) ; until "*/"
	     (:block-comment2-after-asterisk (if (string= ch "/") (fr-newstate fr :block)
					       (unless (string= ch "*") (fr-newstate fr :block-comment2))))
	     ;; comments in grammar section
	     (:after-slash (cond 
			    ((string= ch "/") (fr-passline fr))
			    ((string= ch "*") (fr-newstate fr :comment)) 
			    (t (fr-newstate fr :start))))
	     (:comment (when (string= ch "*") (fr-newstate fr :comment-after-asterisk))) ; until "*/"
	     (:comment-after-asterisk (if (string= ch "/") (fr-newstate fr :start)
					(unless (string= ch "*") (fr-newstate fr :comment))))
	     ;; end of grammar section
	     (:after-percent (cond ;declarations and section delimiters
			      ((string= ch "%") 
			       (setf (fr-data fr) (if (fr-data fr) :double-percent-2 :double-percent-1)) ;store indicator of how many double percents are emitted
			       (return '(double-percent)))
			      ((basechar-p ch) 
			       (fr-startbuffer fr 0)			       
			       (fr-newstate fr :decl))
			      (t (fr-pushback fr ch) (fr-newstate fr :start))))
	     ;; reading identifiers
	     (:id (unless (or (basechar-p ch) (digit-p ch) (string= ch ">"))
		    (fr-newstate fr :start) 
		    (fr-pushback fr ch) ;; hold CH to process it next time
		    (return (cons 'id (fr-popbuffer fr)))))
	     ;; reading declarations
	     (:decl (unless (basechar-p ch)
		      (fr-newstate fr :start)
		      (fr-pushback fr ch)
		      (return (cons 'decl (fr-popbuffer fr)))))
	     ;; reading strings
	     (:string (cond ; until non-escaped string-opener
		       ((string= ch "\\") (fr-newstate fr :string-after-backslash))
		       ((string= ch string-opener) (fr-newstate fr :start) 
			(return (cons 'string (if do-lisp-escape (string-substitute (fr-popbuffer fr) lisp-symbol-escapes)
						(fr-popbuffer fr)))))))
	     (:string-after-backslash (fr-newstate fr :string)) ; ignore escaped character
	     t)))

(defun print-yy-lexems (filename)
  (let ((fr (fr-open filename)))
    (unwind-protect
	(do ((token t))
	    ((null token) t)
	  (setq token (yy-lexer fr nil))
	  (print token))
      (fr-close fr))))

(load "yy-slr1.lsp")

(defun parse-yy-rules (filename do-lisp-escape)
  "Syntax parser of Yacc grammar files"
  (let ((fr (fr-open filename)) res)
    (unwind-protect
	(progn
	  (setq res (yy-slr1-parser (f/l () (yy-lexer fr do-lisp-escape)) fr)) ; run SLR(1) parser routine
	  (if (eq (car res) 'syntax-error)
	      (error (concat "Syntax error at Line " (fr-line fr) " Col " (fr-col fr) 
			     ": Input: " (fourth res) " Expected: " (third res) " at state " (second res)))
	    res))      
      (fr-close fr))))

(defun yy2grm (sourcefile targetfile targetsymbol &optional as-strings)
  "Traslator of Yacc grammar files to GRM format"
  (let* ((rd (parse-yy-rules sourcefile (not as-strings)))	 
	 (rules (second rd)) 
	 (rulecnt 0)
	 (ntab (+ (length targetsymbol) 17))
	 (outs (openstream targetfile "w"))
	 first-elt i symbols)
    (unwind-protect
	(progn
	  (formatl outs ";;; GRM translation of file " sourcefile t
		  ";;; generated by yy2grm (C) 2010-2014, Andrej Andrejev" t t
		  "(defparameter " targetsymbol " '(" t)	 
	  (dolist (rule rules) ; 1. translate rules
	    (dolist (rightpart (cdr rule))
	      (spaces ntab outs)
	      (formatl outs (if as-strings "(\"" "(") (car rule) (if as-strings "\"" ""))
	      (setq first-elt t)
	      (dolist (id rightpart) 
		(if (listp id) ; process rule flags
		    (when (string= (first id) "prec") ; translate %prec flag, ignore others
		      (formatl outs " :prec ")
		      (if as-strings (formatl outs "\"" (second id) "\"") (formatl outs (second id))))
		  (progn ; translate RHS symbols
		    (when first-elt
		      (formatl outs " ->")
		      (setq first-elt nil))		    
		    (if as-strings (formatl outs " \"" id "\"") (formatl outs " " id)))))
	      (formatl outs " 0) ;" (1++ rulecnt) t)))
	  (setq first-elt t)
	  (dolist (decl (first rd)) ; 2. translate precedence declarations 
	    (when (member (first decl) '("left" "right" "nonassoc" "precedence"))
	      (if first-elt
		  (progn
		    (spaces ntab outs)
		    (formatl outs "(:prec")
		    (setq first-elt nil))
		(progn
		  (formatl outs t)
		  (spaces (+ ntab 6) outs)))
	      (setq symbols
		    (if (and (second decl) (string= (substring 0 0 (second decl)) "<") ;skip type declaration
			     (string= (substring (setq i (1- (length (second decl)))) i (second decl)) ">"))
			(cddr decl) (cdr decl)))		  
	      (formatl outs " (:" (first decl) " "
		       (if as-strings (strings-to-string symbols "\"" " " "\"") (strings-to-string symbols "" " " "")) ")")))
	  (unless first-elt
	    (formatl outs ")" t))	    
	  (dolist (decl (first rd)) ; 3. translate start declaration
	    (when (string= (first decl) "start")
	      (spaces ntab outs)
	      (formatl outs "(:start ")
	      (if as-strings (formatl outs "\"" (second decl) "\"") (formatl outs (second decl)))
	      (formatl outs ")" t)))
	  (spaces ntab outs)
	  (formatl outs "))" t t)
	  (formatl outs ";;; This will generate SLR(1) parser, ascend.lsp should be loaded first" t
		   "(make-slr1-parser (grammar-from-johnsons " targetsymbol ") nil \"" 
		   targetsymbol "-slr1\" \"" targetsymbol  "-slr1.lsp\" nil)" t))
      (closestream outs)) t))
	  
		       
