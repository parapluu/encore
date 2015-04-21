#lang scribble/manual

@(require (for-syntax scheme/base))
@(require scribble/bnf)
@(require (for-syntax racket/syntax))

@title{Grammar}
@margin-note{Please note that this grammar tree is out of date.}
This section introduces the Encore grammar by using the BNF-grammar notation and
 show examples on how to build syntactically valid Encore programs.

@; Define macro that creates keyword
@(define-syntax (encore/keyword stx)
  (begin
  (syntax-case stx ()
    [(_ a)
     (with-syntax ([value (symbol->string (syntax->datum #'a))])
       #'(define a (litchar value)))]
    [(_ a b)
       #'(define a (litchar b))])))

@(define-syntax (encore/nonterm stx)
  (syntax-case stx ()
    [(_ a)
     (with-syntax ([value (symbol->string (syntax->datum #'a))])
       #'(define a (nonterm value)))]))

@; Definition of my keywords
@(encore/keyword open-paren "(")
@(encore/keyword close-paren ")")
@(encore/keyword open-bracket "[")
@(encore/keyword close-bracket "]")
@(encore/keyword bar "|")
@(encore/keyword open-c "{")
@(encore/keyword close-c "}")
@(encore/keyword equal "=")
@(encore/keyword colon ":")
@(encore/keyword semic ";")
@(encore/keyword exc "!")
@(encore/keyword k_let "let")
@(encore/keyword repeat "repeat")
@(encore/keyword arrow "->")
@(encore/keyword larrow "<-")
@(encore/keyword lamb "\\")
@(encore/keyword dot ".")
@(encore/keyword l "<")
@(encore/keyword b ">")
@(encore/keyword equals "==")
@(encore/keyword distinct "!=")
@(encore/keyword plus "+")
@(encore/keyword minus "-")
@(encore/keyword prod "*")
@(encore/keyword div "/")
@(encore/keyword mod "%")
@(encore/keyword comma ",")
@(encore/keyword def)
@(encore/keyword embed)
@(encore/keyword import)
@(encore/keyword where)
@(encore/keyword bundle)
@(encore/keyword as)
@(encore/keyword class)
@(encore/keyword print)
@(encore/keyword in)
@(encore/keyword if)
@(encore/keyword then)
@(encore/keyword else)
@(encore/keyword unless)
@(encore/keyword while)
@(encore/keyword null)
@(encore/keyword true)
@(encore/keyword false)
@(encore/keyword get)
@(encore/keyword new)
@(encore/keyword not)
@(encore/keyword and)
@(encore/keyword or)
@(encore/keyword string)
@(encore/keyword int)
@(encore/keyword bool)
@(encore/keyword void)
@(encore/keyword passive)
@(encore/keyword end)

@; Non-terminals
@(encore/nonterm Program)
@(encore/nonterm BundleDecl)
@(encore/nonterm Imports)
@(encore/nonterm EmbedTL)
@(encore/nonterm ClassDecl)
@(encore/nonterm eps)
@(encore/nonterm Name)
@(encore/nonterm QName)
@(encore/nonterm FieldDecls)
@(encore/nonterm MethodDecls)
@(encore/nonterm Type)
@(encore/nonterm Expr)
@(encore/nonterm Seq)
@(encore/nonterm Sequence)
@(encore/nonterm ParamDecls)
@(encore/nonterm Args)
@(encore/nonterm Arguments)
@(encore/nonterm LetDecls)
@(encore/nonterm Int)
@(encore/nonterm Real)
@(encore/nonterm Op)
@(encore/nonterm String)
@(encore/nonterm Arrow)
@(encore/nonterm NonArrow)
@(encore/nonterm Types)
@(encore/nonterm Tys)
@(encore/nonterm RefType)
@(encore/nonterm Fut)
@(encore/nonterm Par)


@(define seq (lambda xs (apply BNF-seq xs)))
@(define alt (lambda xs (apply BNF-alt xs)))

@(let ([nt (lambda (x) (nonterm x))]
       [let @litchar{let}]
       [lambda @litchar["\\"]])
     @BNF[(list Program
    		@alt[
		  @seq[
			@optional[BundleDecl]
			@kleenestar[Imports]
			@optional[EmbedTL]
		  	@nonterm{ClassDecl}]
		eps])

      (list BundleDecl
         @seq[bundle QName where])

	  (list ClassDecl
	  	@seq[@(optional passive) class  Name open-c FieldDecls MethodDecls close-c])

          (list Imports
                @seq[import
		         @optional{@litchar{qualified}} 
			 QName
			 @optional[open-paren Name @elem{, ...} close-paren] 
			 @optional[@seq[as Name]]])

	  (list EmbedTL
	  	@alt[
			@seq[embed @elem{.* body .*} end]
			@seq[embed @elem{.*} end]])
	  
	  (list ClassDecl
	  	@seq[@(optional passive) class  Name open-c FieldDecls MethodDecls close-c])
	
	  (list FieldDecls
	  	@alt[
  		 	@seq[Name colon Type FieldDecls]
			eps])

	(list ParamDecls
	  	@alt[
  		 	@seq[Name colon Type ParamDecls]
			eps])

	(list MethodDecls
	      @seq[def Name open-paren ParamDecls close-paren colon Type Expr])

	(list Sequence
	      @alt[
			@seq[Expr Seq]
			eps])

	(list Seq
	      @alt[
			@seq[semic Expr Seq]
			semic
			eps])

	(list Arguments
	      @alt[
			@seq[Expr Args]
			eps])

	(list Args
	      @alt[
			@seq[semic Expr Args]
			eps])

	(list LetDecls
	      @alt[
			@seq[Name equal Expr LetDecls]
			eps])

	(list Expr
	      @seq[open-paren close-paren]
	      @seq[embed Type @elem{.*} end]
	      @seq[Expr dot Name]
	      @seq[Expr dot Name open-paren Arguments close-paren]
	      @seq[Expr exc Name open-paren Arguments close-paren]
	      @seq[print Expr]
	      @seq[Name open-paren Arguments close-paren]
	      @seq[open-paren Expr close-paren]
	      @seq[Name]
	      @seq[let LetDecls in Expr]
	      @seq[repeat Name larrow Expr Expr]
	      @seq[Expr equal Expr]
	      @seq[open-c Sequence close-c]
	      @seq[if Expr then Expr else Expr]
	      @seq[if Expr then Expr]
	      @seq[unless Expr then Expr]
	      @seq[while Expr Expr]
	      @seq[get Expr]
	      @seq[new Type open-paren Arguments close-paren]
	      @seq[new Type]
	      @seq[Expr open-bracket Expr close-bracket]
	      @seq[open-bracket Expr @elem{, ...} close-bracket]
	      @seq[bar Expr bar]
	      null
	      true
	      false
	      @elem{"String"}
	      Int
	      Real
	      @seq[Expr Op Expr]
	      @seq[not Expr]
	      @seq[lambda open-paren ParamDecls close-paren arrow Expr])

	  (list Op
	      @alt[l b equals distinct plus minus prod div mod and or])

	  (list Name
	      @elem{[a-zA-Z][a-zA-Z0-9]*})
	  
	  (list QName
		      @seq[Name @optional[dot QName]])
			
	  (list Int @elem{[0-9]+})

	  (list Real @seq[Int dot Int])

	  (list String @kleenestar[open-paren @alt[@elem{[^\"]} @elem{\\\"}] close-paren])

	  (list Type @alt[Arrow NonArrow])

	  (list Arrow
	      @alt[
			@seq[open-paren Types close-paren arrow NonArrow]
			@seq[NonArrow arrow NonArrow]])

	(list NonArrow
	      string int bool void RefType 
	      @seq[Fut Type]
	      @seq[Par Type]
	      @seq[open-paren Type close-paren]
	      @seq[open-bracket Type close-bracket])

	(list Types
	      @alt[
			@seq[Type Tys]
			eps])

	(list Tys 
	      @alt[
			@seq[comma Type Tys]
			eps])

	(list RefType @seq[@elem{[A-Z]} @kleenestar[@elem{[a-zA-Z0-9_]}]])
])


