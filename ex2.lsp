(defparameter ex2 '(
		    (<expr> -> <expr> + <term> #'(lambda (a b c) (list '+ a c))) ;1
		    (<expr> -> <expr> - <term> #'(lambda (a b c) (list '- a c))) ;2
		    (<expr> -> <term> 1) ;3
		    (<term> -> <term> * <factor> #'(lambda (a b c) (list '* a c))) ;4
		    (<term> -> <term> / <factor> #'(lambda (a b c) (list '/ a c))) ;5
		    (<term> -> <factor> 1) ;6
		    (<factor> -> const 1) ;7
		    (<factor> -> var 1) ;8
		    (<factor> -> lp <expr> rp 2) ;9
		    ))