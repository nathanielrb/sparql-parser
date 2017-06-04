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

(define (value? x)
  (or (string? x) (number? x) (boolean? x)
      (vector? x) (null? x) (pair? x) (symbol? x)))

(define (value? x)
  (or (symbol? x) (char? x) (pair? x)))

(define consumed-values (consumed-objects val?))

;; helper macro for mutually-recursive parser definitions

(define-syntax vac
  (syntax-rules ()
    ((_ fn) (lambda args (apply fn args)))))

(define consumed-values->list
  (consumed-objects-lift consumed-values))

;; shortcut for (abnf:bind (consumed-values->list ...) ... )

(define-syntax bind-consumed-values->list
  (syntax-rules () 
    ((_ l p)    (bind (consumed-values->list l)  p))
    ((_ p)      (bind (consumed-values->list)  p))
    ))

(define-syntax bind-consumed-values->alist
  (syntax-rules () 
    ((_ label l p)    (bind (consumed-values->list label l)  p))
    ((_ label p)      (bind (consumed-values->list label)  p))
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

(define (err s)
  (print "lexical error on stream: " s)
  `(error))

(define PNAME_NS (between-fws (concatenation (repetition1 char-list/alpha) (char-list/lit ":"))))

(define IRI_REF
   (between-fws
    (concatenation
     (char-list/lit "<http://")
     (repetition1 
      (alternatives ;; should be list-from-string
       char-list/alpha
       (char-list/lit ".")
       (char-list/lit "/")))
     (char-list/lit ">"))))

(define PrefixDecl
  (bind-consumed-values->alist
   'Prefix
   (concatenation
    (between-fws (char-list/lit "PREFIX"))
    PNAME_NS
    IRI_REF)))

(define Prologue (repetition PrefixDecl))

(define (lit/sp str)
  (between-fws (char-list/lit str)))

(define Var
  (bind-consumed-values->alist
   'Var
   (lit/sp "?a")))

(define Expression (lit/sp "?b"))

(define SelectClause
  (bind-consumed-values->alist
   'Select
   (concatenation (lit/sp "SELECT")
		  (optional-sequence
		   (alternatives  (lit/sp "DISTINCT") (lit/sp "REDUCED")))
		  (alternatives
		   (repetition1
		    (alternatives
		     Var
		     (concatenation (lit/sp "(") Expression (lit/sp "AS") Var (lit/sp ")"))))
		   (lit/sp "*")))))

; ( ( Var | ( '(' Expression 'AS' Var ')' ) )+ | '*' )


(define SelectQuery (concatenation SelectClause)) ; DatasetClause* WhereClause SolutionModifier

(define Query
  (bind-consumed-values->alist
   'Query
   (concatenation
    (bind-consumed-values->alist 'Prologue Prologue)
    (bind-consumed-values->alist 'SelectQuery SelectQuery))))
;	       (alternatives SelectQuery ))); ConstructQuery DescribeQuery AskQuery )
;	       ValuesClause))

(require-extension lexgen)

(define r (lex Query err "PREFIX pre: <http://www.home.com/>  
                  PREFIX pre: <http://www.gooogle.com/> 
SELECT ?a ?a"))

(print r)

