;;; ============================================================
;;; AMOS2
;;; 
;;; Author: (c) 2010-2014, Andrej Andrejev, UDBL
;;; $RCSfile: ebnf-parser-datamodel.lsp,v $
;;; $Revision: 1.2 $ $Date: 2014/06/01 20:11:41 $
;;; $State: Exp $ $Locker:  $
;;;
;;; Description: EBNF to GRM grammar converter
;;; =============================================================
;;; $Log: ebnf-parser-datamodel.lsp,v $
;;; Revision 1.2  2014/06/01 20:11:41  andan342
;;; Made grammar converters loadable on top of SSDM image
;;;
;;;
;;; =============================================================


(defstruct ebnf-parser-data (outs nil) (ntab 0) (rule-no 0) (next-aux-no 0) (next-term-no 0) string-to-term psymbs rules aux-rules)

(defstruct ebnf-rule left alts comment)


(defun make-plus-symbol (alts data)
  (let ((res (dolist (psymb (ebnf-parser-data-psymbs data) nil) ; lookup if equivalent symbol already created
	       (when (equal alts (cdr psymb)) (return (car psymb))))))
    (unless res ; unless found
      (setq res (if (and (null (cdr alts)) (null (cdar alts)))
		    (concat (caar alts) "+") ; create with simple + postfix to single symbol in ALTS
		  (concat "aux" (incf (ebnf-parser-data-next-aux-no data))))) ; create special AUX# symbol
      (push (cons res alts) (ebnf-parser-data-psymbs data)) ; record the symbol 
      (push (make-ebnf-rule :left res :alts (append alts (mapcar (f/l (alt) (cons res alt)) alts))) (ebnf-parser-data-aux-rules data))) ; push AUX rules into buffer
    res))

(defun valid-symbol-p (a)
  (let ((ch (substring 0 0 a)))
    (when (or (basechar-p ch) (string= ch "<"))
      (dotimes (i (- (length a) 1) t)
	(setq ch (substring (1+ i) (1+ i) a))
	(unless (or (basechar-p ch) (digit-p ch) (member ch '("+" "-" "<" ">")))
	  (return nil))))))
	

(defun print-ebnf-rule (rule data nts)
  (let ((outs (ebnf-parser-data-outs data))
	(comment (ebnf-rule-comment rule)) symb)
    (dolist (alt (ebnf-rule-alts rule) t)
      (spaces (ebnf-parser-data-ntab data) outs)
      (formatl outs "(<" (ebnf-rule-left rule) "> ->")
      (dolist (a alt)
	(if (valid-symbol-p a) (setq symb a)
	  (let ((s2t (assoc a (ebnf-parser-data-string-to-term data))))
	    (if s2t (setq symb (cdr s2t))
	      (progn (setq symb (concat "term" (incf (ebnf-parser-data-next-term-no data))))
		     (push (cons a symb) (ebnf-parser-data-string-to-term data))))))
	(when (member symb nts) 
	  (setq symb (concat "<" symb ">")))
	(formatl outs " " symb))
      (formatl outs " 1) ;" (incf (ebnf-parser-data-rule-no data)))
      (when comment 
	(formatl outs " ;" comment)
	(setq comment nil))
      (formatl outs t))))
    
    
      