(require-extension typeclass input-classes abnf abnf-charlist  abnf-consumers)

;; Docs:
;; http://wiki.call-cc.org/eggref/4/abnf

;; Example:
;; https://code.call-cc.org/svn/chicken-eggs/release/4/json-abnf/trunk/json-abnf.scm

(define char-list-<Input>
  (make-<Input> null? car cdr))

(define char-list-<Token>
  (Input->Token char-list-<Input>))

(define char-list-<CharLex>
  (Token->CharLex char-list-<Token>))

(define char-list-<CoreABNF>
  (CharLex->CoreABNF char-list-<CharLex>))

(import-instance (<Token> char-list-<Token> char-list/)
		 (<CharLex> char-list-<CharLex> char-list/)
                 (<CoreABNF> char-list-<CoreABNF> char-list/)
                 )

(define (value?  x)    (or (string? x) (number? x) (boolean? x)
			   (vector? x) (null? x) (pair? x) (symbol? x)))

(define consumed-values (consumed-objects value?))

(define consumed-values->list
  (consumed-objects-lift consumed-values))

;; shortcut for (abnf:bind (consumed-values->list ...) ... )
(define-syntax bind-consumed-values->list
  (syntax-rules () 
    ((_ l p)    (bind (consumed-values->list l)  p))
    ((_ p)      (bind (consumed-values->list)    p))
    ))

(define fws
  (concatenation
   (optional-sequence 
    (concatenation
     (repetition char-list/wsp)
     (drop-consumed 
      (alternatives char-list/crlf char-list/lf char-list/cr))))
   (repetition char-list/wsp)))


(define (between-fws p)
  (concatenation
   (drop-consumed (optional-sequence fws)) p 
   (drop-consumed (optional-sequence fws))))

(define select-name 
  (alternatives
   (char-list/lit "SELECT")
   (char-list/lit "select")))

(define select-query
  (concatenation
   (between-fws select-name)
   (between-fws (char-list/lit "{"))
   
   (between-fws (char-list/lit "}"))))

(define (err s)
  (print "lexical error on stream: " s)
  `(error))


(define PNAME_NS (between-fws (concatenation (repetition1 char-list/alpha) (char-list/lit ":"))))

(define IRI_REF (between-fws (repetition1 char-list/alpha)))

(define PrefixDecl
  (concatenation
   (between-fws (char-list/lit "PREFIX"))
   PNAME_NS
   IRI_REF))

(define Prologue (repetition PrefixDecl))

(define (lit/sp str)
  (between-fws (char-list/lit str)))

(define Var (lit/sp "?a"))

(define Expression (lit/sp "?b"))

(define SelectClause
  (concatenation (lit/sp "SELECT")
		  (optional-sequence
		   (alternatives  (lit/sp "DISTINCT") (lit/sp "REDUCED")))
		  (alternatives
		   (repetition1
		    (alternatives
		     Var
		     (concatenation (lit/sp "(") Expression (lit/sp "AS") Var (lit/sp ")"))))
		   (lit/sp "*"))))

; ( ( Var | ( '(' Expression 'AS' Var ')' ) )+ | '*' )


(define SelectQuery (concatenation SelectClause)) ; DatasetClause* WhereClause SolutionModifier

(define Query (concatenation
	       (bind-consumed-values->list Prologue)
	       (bind-consumed-values->list SelectQuery)))
;	       (alternatives SelectQuery ))); ConstructQuery DescribeQuery AskQuery )
;	       ValuesClause))

(require-extension lexgen)

(define r (lex Prologue err "PREFIX pre: <http://www.home.com/>  
                  PREFIX pre: <http:go/>")) ;SELECT ?a ?a"))

(print r)

