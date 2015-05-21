;;; ============================================================
;;; AMOS2
;;; 
;;; Author: (c) 2010 Andrej Andrejev, UDBL
;;; $RCSfile: file-reader.lsp,v $
;;; $Revision: 1.7 $ $Date: 2014/06/01 20:11:40 $
;;; $State: Exp $ $Locker:  $
;;;
;;; Description: Basic file-reader functionality for parsers
;;; =============================================================
;;; $Log: file-reader.lsp,v $
;;; Revision 1.7  2014/06/01 20:11:40  andan342
;;; Made grammar converters loadable on top of SSDM image
;;;
;;; Revision 1.6  2011/10/23 11:47:58  andan342
;;; Collected commonly used parsiong utility functions into parse-utils.lsp
;;;
;;; Revision 1.1  2006/02/12 20:01:09  torer
;;; Folder AmosNT/headers contains CVS header templates
;;;
;;; =============================================================

(defparameter _file_reader_loaded_ t)

(defstruct fr filename filestr linebuf linebuflen (line 0) (col -1) buf bufdelay bufstartcol bufline (lexerstate :start) (hold nil) data subst)

(defun fr-open (filename)
  "Create a File Reader object associated with a specified filename"
  (let* ((str (openstream filename "r")) ; open stream
	 (lb (read-line str))) ; read first line
    (make-fr :filename filename :filestr str :linebuf lb :linebuflen (length lb))))

(defun fr-newstate (fr newstate)
  "Change the lexer state stored in FR"
  (setf (fr-lexerstate fr) newstate))

(defun fr-pushback (fr char)
  "Return HOLD character next time FR-NEXTCHAR is called"
  (push char (fr-hold fr)))

(defun fr-nextchar (fr)
  "Return next character (or HOLD character), #10 for EOL and empty string for EOF,
   update COL and LINE values, buffer if active, BUFDELAY
   trigger start or substitution of buffer if BUFDELAY comes to 0"
  (if (fr-hold fr) (pop (fr-hold fr))
    (progn
      (incf (fr-col fr)) ; update col counter
      (when (numberp (fr-bufdelay fr)) ; start buffering if appropriate
	(decf (fr-bufdelay fr)) ; decrement delay
	(when (= (fr-bufdelay fr) 0) 
	  (setf (fr-bufdelay fr) nil) ; remove delay
	  (if (fr-buf fr) (fr-substbuffer fr 0) ;actually do substitution
	    (fr-startbuffer fr 0)))) ; actually start buffering
      (if (< (fr-col fr) (fr-linebuflen fr)) ; normal return
	  (substring (fr-col fr) (fr-col fr) (fr-linebuf fr))
	(progn ; read next line
	  (when (fr-buf fr) ; update buffer if appropriate
	    (fr-updatebuffer fr (1- (fr-linebuflen fr)))
	    (setf (fr-bufstartcol fr) 0))
	  (setf (fr-linebuf fr) (read-line (fr-filestr fr))) ; read next line
	  (setf (fr-col fr) -1) ;reset col counter
	  (if (eq (fr-linebuf fr) '*eof*) ""  ; return empty char to signalize EOF
	    (progn 
	      (setf (fr-linebuflen fr) (length (fr-linebuf fr))) ; measure line just read
	      (incf (fr-line fr)) ; update line counter
	      (int-char 10)))))))) ; return EOL

(defun fr-passline (fr)
  "Proceed immediatelty to the end of current line, if buffer is active, the contents will still be bufferized"
  (setf (fr-col fr) (fr-linebuflen fr)))

(defun fr-pos-string (fr)
  "Print the file position (1-based Line number and 0-based Column number)"
  (concat "Line " (1+ (fr-line fr)) " Col " (fr-col fr)))

(defun fr-startbuffer (fr offset)
  "Start the buffer in OFFSET characters"
  (if (= offset 0)
      (progn
	(setf (fr-buf fr) "")
	(setf (fr-bufline fr) (fr-line fr))
	(setf (fr-bufstartcol fr) (fr-col fr)))
    (setf (fr-bufdelay fr) offset)))

(defun fr-updatebuffer (fr col)
  "Update the buffer (internal)"
  (when (> (fr-col fr) 0)
    (setf (fr-buf fr) 
	  (concat (fr-buf fr) 
		  (if (= (fr-bufline fr) (fr-line fr)) ""
		    (progn (setf (fr-bufline fr) (fr-line fr)) (int-char 10))) ; add EOL & update buffer's line
		  (substring (fr-bufstartcol fr) col (fr-linebuf fr))))))

(defun fr-substitute (fr offset lastlen newchars)
  "Substitute LASTLEN of buffer with NEWCHARS after processing OFFSET characters"
  (setf (fr-subst fr) (cons lastlen newchars))
  (fr-substbuffer fr offset))

(defun fr-substbuffer (fr offset)
  "Perform buffer subsitution with SUBST data (internal)"
  (if (= offset 0)
      (let* ((lastlen (car (fr-subst fr)))
	     (act-col (max (fr-col fr) 0)) ; 'actual' current col
	     (remchars (- lastlen (- act-col (fr-bufstartcol fr)))))
	(cond ((> remchars 0) ; cut-off REMCHARS from buffer
	       (setf (fr-buf fr) (substring 0 (- (length (fr-buf fr)) (1+ remchars)) (fr-buf fr))))
	      ((< remchars 0) ; update buffer with LASTLEN less characters than normal
	       (fr-updatebuffer fr (- act-col (1+ lastlen)))))
	(setf (fr-buf fr) (concat (fr-buf fr) (cdr (fr-subst fr)))) ; append NEWCHARS here
	(setf (fr-bufstartcol fr) act-col)) ; resume buffering from current Col, or next if Col=-1
    (setf (fr-bufdelay fr) offset)))    

(defun fr-popbuffer (fr)
  "Update and deactivate the buffer, return its contents"
  (fr-updatebuffer fr (1- (fr-col fr)))
  (let ((res (fr-buf fr)))
    (setf (fr-buf fr) nil)
    res))

(defun fr-close (fr)
  "Close the File Reader"
  (closestream (fr-filestr fr)))

