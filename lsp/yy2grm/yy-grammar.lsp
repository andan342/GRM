(load "../ascend.lsp")

(defparameter yy-grammar '(
			   (<yy-file> -> <decls> double-percent <rules> #'(lambda (a b c) (list a c))) ;1
			   (<yy-file> -> <yy-file> double-percent 1) ;2
			   (<decls> -> <decl> #'(lambda (a) (list a))) ;3
			   (<decls> -> <decl> <decls> #'(lambda (a b) (cons a b))) ;4
			   (<decls> -> <decl> semicolon <decls> #'(lambda (a b c) (cons a c))) ;5
			   (<decl> -> decl #'(lambda (a) (list a))) ;6
			   (<decl> -> <decl> id #'(lambda (a b) (append a (list b)))) ;7
			   (<decl> -> <decl> string #'(lambda (a b) (append a (list b)))) ;8
			   (<rules> -> <rule> #'(lambda (a) (list a))) ;9
			   (<rules> -> <rule> <rules> #'(lambda (a b) (cons a b))) ;10
			   (<rule> -> id colon <rightparts> semicolon #'(lambda (a b c d) (cons a c))) ;11
			   (<rightparts> -> <rightpart-ex> #'(lambda (a) (list a))) ;12
			   (<rightparts> -> <rightpart-ex> bar <rightparts> #'(lambda (a b c) (cons a c))) ;13
			   (<rightpart-ex> -> <rightpart> 1) ;14
			   (<rightpart-ex> -> <rightpart> <decl> #'(lambda (a b) (cons b a))) ;15
			   (<rightpart> -> 0) ;16
			   (<rightpart> -> id <rightpart> #'(lambda (a b) (cons a b))) ;17
			   (<rightpart> -> string <rightpart> #'(lambda (a b) (cons a b))) ;18
			   ))

(make-slr1-parser (grammar-from-johnsons yy-grammar) nil "yy-slr1-parser" "yy-slr1.lsp" nil)