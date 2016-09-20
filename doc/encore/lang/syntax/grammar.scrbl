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
@(encore/keyword party_par "||")
@(encore/keyword party_seq ">>")
@(encore/keyword party_join "join")

@; TODO: Add this keyword as soon as as issue #434 is fixed
@; (encore/keyword party_extract "extract")

@(encore/keyword party_each "each")
@(encore/keyword party_liftv "liftv")
@(encore/keyword party_liftf "liftf")
@(encore/keyword for "for")
@(encore/keyword by "by")
@(encore/keyword arrow "->")
@(encore/keyword larrow "<-")
@(encore/keyword lamb "\\")
@(encore/keyword comment "--")
@(encore/keyword open-bcomment "{-")
@(encore/keyword close-bcomment "-}")
@(encore/keyword dot ".")
@(encore/keyword dotdot "..")
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
@(encore/keyword trait)
@(encore/keyword require)
@(encore/keyword print)
@(encore/keyword in)
@(encore/keyword if)
@(encore/keyword then)
@(encore/keyword else)
@(encore/keyword unless)
@(encore/keyword while)
@(encore/keyword match)
@(encore/keyword with)
@(encore/keyword when)
@(encore/keyword bold-arrow "=>")
@(encore/keyword null)
@(encore/keyword true)
@(encore/keyword false)
@(encore/keyword get)
@(encore/keyword new)
@(encore/keyword not)
@(encore/keyword and)
@(encore/keyword or)
@(encore/keyword char)
@(encore/keyword int)
@(encore/keyword bool)
@(encore/keyword void)
@(encore/keyword passive)
@(encore/keyword end)
@(encore/keyword typedef)
@(encore/keyword Just)
@(encore/keyword Nothing)

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
@(encore/nonterm Option)
@(encore/nonterm Op)
@(encore/nonterm Char)
@(encore/nonterm String)
@(encore/nonterm Arrow)
@(encore/nonterm NonArrow)
@(encore/nonterm MatchClause)
@(encore/nonterm Types)
@(encore/nonterm Tys)
@(encore/nonterm RefType)
@(encore/nonterm Fut)
@(encore/nonterm Par)
@(encore/nonterm TypeDef)
@(encore/nonterm TypeParams)
@(encore/nonterm TypeVar)


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
            @kleenestar[TypeDef]
		  	@nonterm{ClassDecl}]
		eps])

      (list BundleDecl
         @seq[bundle QName where])

      (list TypeDef
         @seq[typedef Name @optional[TypeParams] equal Type])

      (list TypeParams
         @seq[l TypeVar @kleenestar[@BNF-group[comma TypeVar]] b])

      (list TypeVar @seq[@elem{[a-z]} @kleenestar[@elem{[a-zA-Z0-9_]}]])

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
              @seq[comment "this is a comment"]
              @seq[open-bcomment "this is a block comment" close-bcomment]
	      @seq[open-paren close-paren]
	      @seq[embed Type @elem{.*} end]
	      @seq[Expr dot Name]
	      @seq[Expr dot Name open-paren Arguments close-paren]
	      @seq[Expr exc Name open-paren Arguments close-paren]
	      @seq[print Expr]
	      @seq[Name open-paren Arguments close-paren]
	      @seq[open-paren Expr close-paren]
	      @seq[Name]
              @seq[party_liftv Expr]
              @seq[party_liftf Expr]
              @seq[party_join Expr]

              @; TODO: Add this keyword as soon as as issue #434 is fixed
              @; seq[party_extract Expr]

              @seq[party_each Expr]
              @seq[Expr party_seq Arrow]
              @seq[Expr party_par Expr]
	      @seq[let LetDecls in Expr]
	      @seq[repeat Name larrow Expr Expr]
	      @seq[for Name in Expr Expr]
	      @seq[for Name in Expr by Expr Expr]
	      @seq[Expr equal Expr]
	      @seq[open-c Sequence close-c]
	      @seq[if Expr then Expr else Expr]
	      @seq[if Expr then Expr]
	      @seq[unless Expr then Expr]
	      @seq[while Expr Expr]
	      @seq[match Expr with @kleenestar[MatchClause]]
	      @seq[get Expr]
	      @seq[new Type open-paren Arguments close-paren]
	      @seq[new Type]
	      @seq[new open-bracket Expr dotdot Expr close-bracket]
	      @seq[new open-bracket Expr dotdot Expr by Expr close-bracket]
	      @seq[Expr open-bracket Expr close-bracket]
	      @seq[open-bracket Expr @elem{, ...} close-bracket]
	      @seq[bar Expr bar]
	      null
	      true
	      false
              Char
	      @elem{"String"}
	      Int
	      Real
              Option
	      @seq[Expr Op Expr]
	      @seq[not Expr]
	      @seq[lambda open-paren ParamDecls close-paren arrow Expr])

	  (list Op
	      @alt[l b equals distinct plus minus prod div mod and or])

          (list Option
              @seq[Just Expr]
              Nothing)
	  (list Name
	      @elem{[a-zA-Z][a-zA-Z0-9]*})

	  (list QName
		      @seq[Name @optional[dot QName]])

	  (list MatchClause @seq[Expr @optional[when Expr] bold-arrow Expr])

	  (list Int @elem{[0-9]+})

	  (list Real @seq[Int dot Int])

	  (list Char @alt[@elem{[^']} @elem{\'}])

	  (list String @kleenestar[open-paren @alt[@elem{[^"]} @elem{\"}] close-paren])

	  (list Type @alt[Arrow NonArrow])

	  (list Arrow
	      @alt[
			@seq[open-paren Types close-paren arrow NonArrow]
			@seq[NonArrow arrow NonArrow]])

	(list NonArrow
	      char int bool void RefType
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
