;;; ============================================================
;;; AMOS2
;;; 
;;; Author: (c) 2011, Andrej Andrejev, UDBL
;;; $RCSfile: parse-utils.lsp,v $
;;; $Revision: 1.4 $ $Date: 2012/02/09 22:25:58 $
;;; $State: Exp $ $Locker:  $
;;;
;;; Description: Utility functions for parsers
;;; =============================================================
;;; $Log: parse-utils.lsp,v $
;;; Revision 1.4  2012/02/09 22:25:58  andan342
;;; Added STRING-REPLACE-CHAR, STRING-RIGHTPOS, LEFTPAD to utils.lsp
;;;
;;; Revision 1.3  2012/01/12 17:31:54  andan342
;;; Added string-padding utility
;;;
;;; Revision 1.2  2011/10/23 12:14:42  andan342
;;; Updated to more general version of STRINGS-TO-STRINGS as used in SQoND translator
;;;
;;; Revision 1.1  2011/10/23 11:47:59  andan342
;;; Collected commonly used parsiong utility functions into parse-utils.lsp
;;;
;;; Revision 1.1  2006/02/12 20:01:09  torer
;;; Folder AmosNT/headers contains CVS header templates
;;;
;;; =============================================================

; newlines and whitespaces

(defparameter nlt (concat (int-char 10) (int-char 9))) ;;newline+tab

(defun nl (offset)
  (let ((res (int-char 10)))
    (dotimes (i offset) (setq res (concat res " ")))
    res))    

; basic character sets

(defun whitespace-p (ch) (member (char-int ch) '(9 10 32)))

(defun digit-p (ch) (and (>= (char-int ch) 48) (<= (char-int ch) 57)))

(defun basechar-p (ch) (let ((ci (char-int ch)))			
			 (or 
			  (and (>= ci 65) (<= ci 90)) ;; A-Z
			  (= ci 95) ;; underscore
			  (and (>= ci 97) (<= ci 122))))) ;; a-z

(defun rdf-basechar-p (ch) 
  (let ((ci (char-int ch)))			
    (or 
     (and (>= ci 65) (<= ci 90)) ;; A-Z
     (= ci 95) ;; underscore
     (and (>= ci 97) (<= ci 122)) ;; a-z			 
     (= ci 183) ;; middle dot
     (and (>= ci 192) (<= ci 255) (not (= ci 215)) (not (= ci 247)))))) ;;letters from Latin-1

; set operations using EQUAL (work on strings!)

(defun set-difference-equal (a b)
  (subset a (f/l (x) (not (member x b)))))

(defun union-equal (a b)
  (dolist (x a)
    (unless (member x b) (push x b)))
  b)

(defun intersection-equal (a b)
  (let (res)
    (dolist (x a)
      (when (member x b)
	(push x res)))
    res))

(defmacro pushnew-equal (item place)
  `(unless (member ,item ,place)
     (push ,item ,place)))

; flexible string aggregation

(defun strings-to-string (strings prefix separator postfix)
  (do ((s1 strings (cdr s1)) (res ""))
      ((null s1) res)
    (unless (string= (car s1) "")
      (unless (string= res "")
	(setq res (concat res separator)))
      (setq res (concat res prefix (car s1) (if postfix postfix ""))))))


    
      