/* CYACC.Y		Yacc grammar for C	-*- Mode: Fundamental; Tab-width: 5 -*-

*/

%{
#define YYSTYPE	lispval
#define short		int		/* ZETA-C shorts are slow */

#define PREINC		1
#define POSTINC	2
#define FATAL		1

#define YYMAXDEPTH	512

lispval LMisc(), LWhere();
lispval PNil(), PCons(), PCar(), PCdr();
lispval PList();
lispval PReverse(), PAppend(), PStringCat();
lispval PIncrForm(), PDefinedp();
void PPushEnv(), PPopEnv(), PDeclare(), PError(), PAccept();

%}

%left LOW

%token NUMBER
%token CHARCONST
%token STRING

%token STORAGECLASS
%token TYPE
%token TYPE_ADJECTIVE
%left TYPEDEF_TYPE
%token ELLIPSIS
%token STRUCT
%token ENUM

%token IF
%token ELSE
%token WHILE
%token DO
%token FOR
%token SWITCH
%token CASE
%token BREAK
%token CONTINUE
%token RETURN
%token GOTO
%token DEFAULT

%token SYMBOL
%token SALLOC
%token AALLOC

%token SEMI

%left ELSE

%left SEMI
%left COMMA
%right ASSIGN OP_ASSIGN
%right QMARK COLON
%left OR
%left AND
%left BIT_OR
%left BIT_XOR
%left AND_ADDRESS
%left EQUALITY
%left COMPARISON
%left SHIFT
%left PLUSMINUS
%left MUL_PTR DIVMOD
%right UNARY INCREMENT NOT BIT_NOT SIZEOF
%nonassoc LBRACKET RBRACKET LPAREN RPAREN LBRACE RBRACE
%left ELEMENT
%left HIGH

/* Preprocessor stuff */
%token SHARPIF
%token DEFINED
%token LISP_INCLUSION		/* #lisp ... #endlisp */
%token MACTOKSTR			/* The parser never sees these two */
%token MACTOKCAT

%token ENDOFSTREAM

%start top_level
%%

expression:	SYMBOL
				/* {$$ = $1;} */
		|	expr_1
				/* {$$ = $1;} */
		|	exp_list
				{$$ = PCons (LMisc ("progn+"), PReverse ($1));}
		|	symbol_list
				{$$ = PCons (LMisc ("progn+"), PReverse ($1));}
		;

/* A list of symbols only will be a symbol_list. */
exp_list	:	expr_1 COMMA exp_no_list
				{$$ = PList ($3, $1);}
		|	exp_list COMMA exp_no_list
				{$$ = PCons ($3, $1);}
		|	SYMBOL COMMA expr_1
				{$$ = PList ($3, $1);}
		|	symbol_list_comma expr_1
				{$$ = PCons ($2, $1);}
		;

symbol_list:	SYMBOL COMMA SYMBOL
				{$$ = PList ($3, $1);}
		|	symbol_list_comma SYMBOL
				{$$ = PCons ($2, $1);}
		;

/* This is to avoid an interaction with param_list (below) that would
   require LR(2)ness. */
symbol_list_comma:	symbol_list COMMA
				{$$ = $1;}
		;

/* Any kind of expression except an unparenthesized comma-separated list. */
exp_no_list:	SYMBOL %prec LOW   /* lower anyhow than ( */
				/* {$$ = $1;} */
		|	expr_1 %prec LOW
				/* {$$ = $1;} */
		;

/* Neither a comma-separated list, nor just a symbol. */
expr_1	:	constant
				/* {$$ = $1;} */
		|	paren_exp
				/* {$$ = $1;} */
		|	MUL_PTR exp_no_list %prec UNARY
				{$$ = PList ($1, $2);}
		|	AND_ADDRESS exp_no_list %prec UNARY
				{$$ = PList ($1, $2);}
		|	PLUSMINUS exp_no_list %prec UNARY
				{$$ = PList ($1, $2);}
		|	NOT exp_no_list
				{$$ = PList ($1, $2);}
		|	BIT_NOT exp_no_list
				{$$ = PList ($1, $2);}
		|	INCREMENT exp_no_list
				{$$ = PIncrForm ($1, $2, PREINC);}
		|	exp_no_list INCREMENT
 				{$$ = PIncrForm ($2, $1, POSTINC);}
		|	SIZEOF exp_no_list
				{$$ = PList ($1, $2);}
		|	SIZEOF LPAREN type_name RPAREN  /* easiest to distinguish */
				{$$ = PList ($1, $3, PNil());}	   /* the 2 cases here */
		|	LPAREN type_name RPAREN exp_no_list %prec UNARY
				{$$ = PList (LMisc ("cast+"), $2, $4);}
		|	SALLOC LPAREN type_name RPAREN
				{$$ = PList ($1, $3);}
		|	AALLOC LPAREN type_name COMMA exp_no_list RPAREN
				{$$ = PList ($1, $3, $5);}
		|	exp_no_list MUL_PTR exp_no_list
				{$$ = PList ($2, $1, $3);}
		|	exp_no_list DIVMOD exp_no_list
				{$$ = PList ($2, $1, $3);}
		|	exp_no_list PLUSMINUS exp_no_list
				{$$ = PList ($2, $1, $3);}
		|	exp_no_list SHIFT exp_no_list
				{$$ = PList ($2, $1, $3);}
		|	exp_no_list COMPARISON exp_no_list
				{$$ = PList ($2, $1, $3);}
		|	exp_no_list EQUALITY exp_no_list
				{$$ = PList ($2, $1, $3);}
		|	exp_no_list AND_ADDRESS exp_no_list
				{$$ = PList ($2, $1, $3);}
		|	exp_no_list BIT_XOR exp_no_list
				{$$ = PList ($2, $1, $3);}
		|	exp_no_list BIT_OR exp_no_list
				{$$ = PList ($2, $1, $3);}
		|	exp_no_list AND exp_no_list
				{$$ = PList ($2, $1, $3);}
		|	exp_no_list OR exp_no_list
				{$$ = PList ($2, $1, $3);}
		|	exp_no_list QMARK exp_no_list COLON exp_no_list
				{$$ = PList (LMisc ("?:"), $1, $3, $5);}
		|	exp_no_list ASSIGN exp_no_list
				{$$ = PList ($2, $1, $3);}
		|	exp_no_list OP_ASSIGN exp_no_list
				{$$ = PList ($2, $1, $3);}
		|	exp_no_list LBRACKET exp_no_list RBRACKET
				{$$ = PList (LMisc ("[]"), $1, $3);}
		|	exp_no_list ELEMENT ident	/* Note "ident": any symbol okay */
				{$$ = PList ($2, $1, $3);}
		|	DEFINED ident				/* for #if processing */
				{$$ = PDefinedp (SELF, $2);}
		|	DEFINED LPAREN ident RPAREN
				{$$ = PDefinedp (SELF, $3);}
		/* Varieties of function call -- necessitated by C listener */
		/* Could this effect have been gotten with precedences? */
		|	amb_funcall
				/* {$$ = $1;} */
		|	SYMBOL LPAREN expr_1 RPAREN
				{$$ = PList ($1, $3);}
		|	SYMBOL LPAREN exp_list RPAREN
				{$$ = PCons ($1, PReverse ($3));}
		|	expr_1 LPAREN RPAREN %prec HIGH
				{$$ = PList ($1);}
		|	expr_1 LPAREN SYMBOL RPAREN
				{$$ = PList ($1, $3);}
		|	expr_1 LPAREN symbol_list RPAREN
				{$$ = PCons ($1, PReverse ($3));}
		|	expr_1 LPAREN expr_1 RPAREN
				{$$ = PList ($1, $3);}
		|	expr_1 LPAREN exp_list RPAREN
				{$$ = PCons ($1, PReverse ($3));}
		|	error %prec HIGH
				{PError (SELF, "Expression syntax"); $$ = $1;}
		;

/* An identifier, for declaration purposes only.  Note that typedef'ed names can be
   reused in other ways, shadowing the typedef. */
ident	:	SYMBOL
				/* {$$ = $1;} */
		|	TYPEDEF_TYPE
				/* {$$ = $1;} */
		;

paren_exp	:	LPAREN expression RPAREN
				{$$ = $2;}
		;

expr_opt	:	expression
				/* {$$ = $1;} */
		|	/* empty */
				{$$ = PNil();}
		;

constant	:	NUMBER
				/* {$$ = $1;} */
		|	CHARCONST
				/* {$$ = $1;} */
		|	string
				/* {$$ = $1;} */
		;

string	:	STRING
				/* {$$ = $1;} */
		|	string STRING
				{$$ = PStringCat ($1, $2);}
		;

/* Could be either a function-call expression at statement
   level (for the C listener) or a function definition */
amb_funcall:	amb_fcl_0
				/* {$$ = $1;} */
		|	amb_fcl_n
				/* {$$ = $1;} */
		;

amb_fcl_0	:	SYMBOL LPAREN RPAREN
				{$$ = PList ($1);}
		;

amb_fcl_n	: 	SYMBOL LPAREN ident RPAREN
				{$$ = PList ($1, $3);}			
		| 	SYMBOL LPAREN param_list RPAREN
				{$$ = PCons ($1, PReverse ($3));}
		;

param_list:	symbol_list %prec LOW
				/* {$$ = $1;} */
		|	symbol_list_comma TYPEDEF_TYPE
				{$$ = PCons ($2, $1);}
		|	TYPEDEF_TYPE COMMA ident
				{$$ = PList ($3, $1);}
		|	param_list COMMA ident
				{$$ = PCons ($3, $1);}
		;

/* A "decl" covers all cases of parameter and local declarations,
   and some cases of external declarations. */
decl		:	decl_spec init_var_specs_opt SEMI
				{$$ = PCons ($1, $2);}
		;

/* A decl_spec must contain at least one token. */
decl_spec	:	STORAGECLASS type_spec
				{$$ = PCons ($1, $2);}
		|	type_spec
				/* {$$ = $1;} */
/* Consider "typedef int foo; static foo ..."  We want the second "foo" to be a type
   ... hence we give this reduction low precedence */   
		|	STORAGECLASS %prec LOW
				{$$ = PList ($1);}
		;

/* A type_spec must contain at least one token. */
type_spec	:	type_sym
				{$$ = PList ($1);}
		|	adj_list TYPE			/* no typedef_types after adjectives */
				{$$ = PAppend (PReverse ($1), PList ($2));}
		|	adj_list
				{$$ = PReverse ($1);}
		|	enum_spec
				{$$ = PList ($1);}
		|	struct_spec
				{$$ = PList ($1);}
		;

adj_list	:	TYPE_ADJECTIVE
				{$$ = PList ($1);}
		|	adj_list TYPE_ADJECTIVE
				{$$ = PCons ($2, $1);}
		;

type_sym	:	TYPE
				/* {$$ = $1;} */
		|	TYPEDEF_TYPE
				/* {$$ = $1;} */
		;

enum_spec	:	ENUM ident
				{$$ = PList ($1, $2);}
		|	ENUM enum_list
				{$$ = PCons ($1, PCons (PNil(), $2));}
		|	ENUM ident enum_list
				{$$ = PCons ($1, PCons ($2, $3));}
		;

enum_list :	LBRACE enum_syms RBRACE
				{$$ = PReverse ($2);}
		;

enum_syms	:	enum_elt
				{$$ = PList ($1);}
		|	enum_syms COMMA enum_elt
				{$$ = PCons ($3, $1);}
		;

enum_elt	:	ident
				/* {$$ = $1;} */
		|	ident ASSIGN exp_no_list
				{$$ = PList ($2, $1, $3);}
		;			

struct_spec:	STRUCT ident			/* "ident" because in different namespace */
				{$$ = PList ($1, $2);}
		|	STRUCT struct_decls
				{$$ = PCons ($1, PCons (PNil(), $2));}
		|	STRUCT ident struct_decls
				{$$ = PCons ($1, PCons ($2, $3));}
		;

struct_decls:	LBRACE struct_declsA RBRACE
				{$$ = PReverse ($2);}
		;

struct_declsA:	struct_declsA struct_decl
				{$$ = PCons ($2, $1);}
		|	/* empty */
				{$$ = PNil();}
		;

struct_decl:	type_spec elt_specs_opt SEMI
				{$$ = PCons ($1, $2);}
		;

elt_specs_opt:	elt_specs
				{$$ = PReverse ($1);}
		|	/* empty */
				{$$ = PNil();}
		;

elt_specs	:	elt_spec
				{$$ = PList ($1);}
		|	elt_specs COMMA elt_spec
				{$$ = PCons ($3, $1);}
		;

elt_spec	:	var_spec
				/* {$$ = $1;} */
		|	var_spec COLON exp_no_list
				{$$ = PList (LMisc ("bits+"), $1, $3);}
		|	COLON exp_no_list
				{$$ = PList (LMisc ("bits+"), PNil(), $2);}
		;

init_var_specs_opt:	init_var_specs
				{$$ = PReverse ($1);}
		|	/* empty */
				{$$ = PNil();}
		;

init_var_specs: init_var_spec
				{$$ = PList ($1);}
		|	init_var_specs COMMA init_var_spec
				{$$ = PCons ($3, $1);}
		;

init_var_spec:	var_spec
				/* {$$ = $1;} */
		|	var_spec initializer
				{$$ = PList (PCar ($2), $1, PCdr ($2));}
		;

var_spec	:	SYMBOL %prec LOW
				/* {$$ = $1;} */
		|	TYPEDEF_TYPE %prec LOW	 /* to allow redeclaration of typedefs */
				/* {$$ = $1;} */
		|	LPAREN var_spec RPAREN
				{$$ = $2;}
		|	MUL_PTR var_spec
				{$$ = PList ($1, $2);}
		|	var_spec LPAREN RPAREN
				{$$ = PList (LMisc ("fcn+"), $1);}
		|	var_spec LBRACKET expr_opt RBRACKET
				{$$ = PList (LMisc ("[]"), $1, $3);}
		;

initializer:	ASSIGN init_exp
				{$$ = PCons ($1, $2);}
		;

init_exp	:	exp_no_list
					/* {$$ = $1;} */
		|	LBRACE RBRACE
					{$$ = PNil();}
		|	LBRACE init_list RBRACE
				{$$ = PCons (LMisc ("list+"), PReverse ($2));}
		|	LBRACE init_list COMMA RBRACE
				{$$ = PCons (LMisc ("list+"), PReverse ($2));}
		;

init_list	:	init_exp
				{$$ = PList ($1);}
		|	init_list COMMA init_exp
				{$$ = PCons ($3, $1);}
		;

type_name	:	type_spec abstract_decl
				{$$ = PCons ($1, $2);}
		;

abstract_decl:	/* empty */ %prec LOW
				{$$ = PNil();}
		|	LPAREN abstract_decl RPAREN
				{$$ = $2;}
		|	MUL_PTR abstract_decl
				{$$ = PList (PCons ($1, $2));}
		|	abstract_decl LPAREN RPAREN
				{$$ = PList (PCons (LMisc ("fcn+"), $1));}
		|	abstract_decl LBRACKET expr_opt RBRACKET
				{$$ = PList (PList (LMisc ("[]"), PCar ($1), $3));}
		;

cpd_stmt	:	LBRACE {PPushEnv (SELF);} decl_list stmt_list_opt RBRACE
				{$$ = PCons (LMisc ("block+"), PCons (PReverse ($3), $4));
				 PPopEnv (SELF);}
		;

decl_list	:	decl_list decl
				{PDeclare (SELF, $2); $$ = PCons ($2, $1);}
		|	/* empty */
				{$$ = PNil();}
		;

/* We do things this slightly roundabout way (instead of just letting
   stmt_list:  <empty> ) so we don't have to tell from the first token
   of something whether it's a statement or a declaration. */
stmt_list_opt:	/* empty */
				{$$ = PNil();}
		|	stmt_list
				{$$ = PReverse($1);}
		;

stmt_list	:	stmt_list statement
				{$$ = PCons ($2, $1);}
		|	statement
				{$$ = PList ($1);}
		;

statement	:	cpd_stmt
				/* {$$ = $1;} */
		|	expression SEMI
				/* {$$ = $1;} */
		|	IF LPAREN expression RPAREN statement %prec LOW
				{$$ = PList ($1, $3, $5);}
		|	IF LPAREN expression RPAREN statement ELSE statement
				{$$ = PList ($1, $3, $5, $7);}
		|	WHILE LPAREN expression RPAREN statement
				{$$ = PList ($1, $3, $5);}
		|	DO statement WHILE LPAREN expression RPAREN
				{$$ = PList ($1, $2, $5);}
		|	FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN statement
				{$$ = PList ($1, $3, $5, $7, $9);}
		|	SWITCH LPAREN expression RPAREN cpd_stmt
				{$$ = PList ($1, $3, $5);}
		|	CASE expression COLON
				{$$ = PList ($1, $2);}
		|	DEFAULT COLON
				/* {$$ = $1;} */
		|	BREAK SEMI
				{$$ = PList ($1);}
		|	CONTINUE SEMI
				{$$ = PList ($1);}
		|	RETURN SEMI
				{$$ = PList ($1);}
		|	RETURN expression SEMI
				{$$ = PList ($1, $2);}
		|	GOTO ident SEMI
				{$$ = PList ($1, $2);}
		/* "ident" doesn't work here, sigh! */
		|	SYMBOL COLON statement
				{$$ = PList (LMisc ("label+"), $1, $3);}
		|	TYPEDEF_TYPE COLON statement
				{$$ = PList (LMisc ("label+"), $1, $3);}
		|	SEMI
				{$$ = PNil();}
		|	LISP_INCLUSION
				/* {$$ = $1;} */
		|	error SEMI
				{PError (SELF, "Statement syntax"); $$ = $1;}
		;

top_level	:	external_def
				{PAccept ($1);}
		|	ENDOFSTREAM
				{PAccept (PNil());}
		;

external_def:	function_def
				/* {$$ = $1;} */
		|	data_def
				{PDeclare (SELF, $1);		/* for C-SOURCE macro */
				 $$ = PCons (LMisc ("decl+"), PCons (LWhere (SELF), $1));}
		|	statement		/* for C listener */
				/* {$$ = $1;} */
		|	SHARPIF expression SHARPIF
				{$$ = $2;}
		|	error
				{PError (SELF, "External definition syntax", FATAL); $$ = $1;}
		;

function_def:	funct_decl {PPushEnv (SELF);} decl_list cpd_stmt
				{$$ = PList (LMisc ("defunc+"), LWhere (SELF), $1, 
						   PReverse ($3), $4);
				 PPopEnv (SELF);}
		;

data_def	:	amb_decl SEMI
				/* {$$ = $1;} */
		|	amb_decl COMMA init_var_specs SEMI
				{$$ = PAppend ($1, PReverse ($3));}
		|	decl
				/* {$$ = $1;} */
		;

amb_decl	:	decl_spec amb_spec
				{$$ = PCons ($1, PList ($2));}
		;

funct_decl:	amb_decl
				/* {$$ = $1;} */
		|	amb_funcall
				{$$ = PList (PNil(), PCons (LMisc ("fcn+"), $1));}
		|	decl_spec funct_spec
				{$$ = PList ($1, $2);}
		;

funct_spec:	amb_fcl_n
				{$$ = PCons (LMisc ("fcn+"), $1);}
		|	LPAREN funct_spec RPAREN
				{$$ = $2;}
		|	MUL_PTR funct_spec
				{$$ = PList ($1, $2);}
/* 		|	MUL_PTR funct_spec LPAREN RPAREN
				{$$ = PList ($1, PList (LMisc ("fcn+"), $2));}   */
		|	funct_spec LPAREN RPAREN
				{$$ = PList (LMisc ("fcn+"), $1);}
		;

amb_spec	:	amb_fcl_0
				{$$ = PCons (LMisc ("fcn+"), $1);}
		|	LPAREN amb_spec RPAREN
				{$$ = $2;}
		|	MUL_PTR amb_spec
				{$$ = PList ($1, $2);}
/*		|	MUL_PTR amb_spec LPAREN RPAREN
				{$$ = PList ($1, PList (LMisc ("fcn+"), $2));}    */
		|	amb_spec LPAREN RPAREN
				{$$ = PList (LMisc ("fcn+"), $1);}
		;

%%

/* End of CYACC.Y */
