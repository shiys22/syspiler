/*****************************************************
 *  The GNU Bison Specification for Mind Language.
 *
 *  We have provided complete SECTION I & IV for you.
 *  Please complete SECTION II & III.
 *
 *  In case you want some debug support, we provide a
 *  "diagnose()" function for you. All you need is to
 *  call this function in main.cpp.
 *
 *  Please refer to the ``GNU Flex Manual'' if you have
 *  problems about how to write the lexical rules.
 *
 *  Keltin Leung 
 */
%output "parser.cpp"
%skeleton "lalr1.cc"
%defines
%define api.value.type variant
%define api.token.constructor
%define parse.assert
%locations
/* SECTION I: preamble inclusion */
%code requires{
#include "config.hpp"
#include "ast/ast.hpp"
#include "location.hpp"
#include "parser.hpp"

using namespace mind;

void yyerror (char const *);
void setParseTree(ast::Program* tree);

  /* This macro is provided for your convenience. */
#define POS(pos)	(new Location(pos.begin.line, pos.begin.column))


void scan_begin(const char* filename);
void scan_end();
}
%code{
  #include "compiler.hpp"
}
/* SECTION II: definition & declaration */

/*   SUBSECTION 2.1: token(terminal) declaration */


%define api.token.prefix {TOK_}
%token
   END  0  "end of file"
   BOOL "bool"
   INT  "int"
   RETURN "return"
   IF "if"
   ELSE  "else"
   DO "do"
   WHILE "while"
   FOR "for"
   BREAK "break"
   CONTINUE "continue"
   EQU "=="
   NEQ "!="
   AND "&&" 
   OR "||"
   LEQ "<="
   GEQ ">="
   PLUS "+"
   MINUS "-"
   TIMES "*"
   SLASH "/"
   MOD "%"
   LT "<"
   GT ">"
   COLON ":"
   SEMICOLON ";"
   LNOT "!"
   BNOT "~"
   COMMA ","
   DOT "."
   ASSIGN "="
   QUESTION "?"
   LPAREN "("
   RPAREN ")"
   LBRACK "["
   RBRACK "]"
   LBRACE "{"
   RBRACE "}"
;
%token <std::string> IDENTIFIER "identifier"
%token<int> ICONST "iconst"
%nterm<mind::ast::StmtList*> StmtList
%nterm<mind::ast::ExprList*> ExprList
%nterm<mind::ast::VarList* > FormalList 
%nterm<mind::ast::Program* > Program FoDList
%nterm<mind::ast::FuncDefn* > FuncDefn
%nterm<mind::ast::Type*> Type
%nterm<mind::ast::Statement*> Stmt  ReturnStmt ExprStmt IfStmt  CompStmt WhileStmt VarDecl1 VarDecl2 VarDecl
%nterm<mind::ast::Expr*> EmptyExpr Expr
/*   SUBSECTION 2.2: associativeness & precedences */
%nonassoc QUESTION
%left	 OR
%left	 AND
%left EQU NEQ
%left LEQ GEQ LT GT
%left	 PLUS MINUS
%left	 TIMES SLASH MOD
%nonassoc LNOT NEG BNOT
%nonassoc LBRACK DOT

%{
  /* we have to include scanner.hpp here... */
#define YY_NO_UNISTD_H 1
%}

/*   SUBSECTION 2.5: start symbol of the grammar */
%start Program

/* SECTION III: grammar rules (and actions) */
%%
Program		: FoDList
				{ /* we don't write $$ = XXX here. */
				  setParseTree($1); }
			;
FoDList		:   
			FuncDefn 
				{$$ = new ast::Program($1,POS(@1)); }
			|
			VarDecl 
				{$$ = new ast::Program($1,POS(@1)); }
			| FoDList FuncDefn
				{$1 -> func_and_globals->append($2); $$ = $1; }
			| FoDList VarDecl
				{$1 -> func_and_globals->append($2); $$ = $1; }
			;

FuncDefn	: Type IDENTIFIER LPAREN FormalList RPAREN LBRACE StmtList RBRACE {
			  $$ = new ast::FuncDefn($2,$1,$4,$7,POS(@1));
		  } |
		  Type IDENTIFIER LPAREN FormalList RPAREN SEMICOLON{
			  $$ = new ast::FuncDefn($2,$1,$4,new ast::EmptyStmt(POS(@6)),POS(@1));
		  }
		  ;
Type		: INT
				{ $$ = new ast::IntType(POS(@1)); }
			;
StmtList	: /* empty */
				{ $$ = new ast::StmtList(); }
			| StmtList Stmt
				{ $1 -> append($2); $$ = $1; }
			| StmtList VarDecl
				{ $1 -> append($2); $$ = $1; }
	  		;
FormalList	:  /* empty */
				{ $$ = new ast::VarList(); } 
			| VarDecl1
				{ $$ = new ast::VarList(); $$ -> append(dynamic_cast<mind::ast::VarDecl*>($1)); } 
			| FormalList COMMA VarDecl1
				{ $1 -> append(dynamic_cast<mind::ast::VarDecl*>($3)); $$ = $1; }
			;
ExprList	: /* empty */
				{ $$ = new ast::ExprList(); }
			| Expr
				{ $$ = new ast::ExprList(); $$ -> append($1); }
			| ExprList COMMA Expr
				{ $1 -> append($3); $$ = $1; }
			;

Stmt		: ReturnStmt{$$ = $1;}|
			  ExprStmt	{$$ = $1;}|
			  IfStmt	{$$ = $1;}|
			  WhileStmt	{$$ = $1;}|
			  CompStmt	{$$ = $1;}|
			  BREAK SEMICOLON  
				{$$ = new ast::BreakStmt(POS(@1));} |
			  CONTINUE SEMICOLON  
				{$$ = new ast::ContStmt(POS(@1));} |
			  SEMICOLON
				{$$ = new ast::EmptyStmt(POS(@1));}
			;
CompStmt	: LBRACE StmtList RBRACE
				{$$ = new ast::CompStmt($2,POS(@1));}
			;
WhileStmt   : WHILE LPAREN Expr RPAREN Stmt
				{ $$ = new ast::WhileStmt($3, $5, POS(@1)); }
			| DO Stmt WHILE LPAREN Expr RPAREN
				{ $$ = new ast::DoWhileStmt($5, $2, POS(@1)); }
			| FOR LPAREN EmptyExpr SEMICOLON EmptyExpr SEMICOLON EmptyExpr RPAREN Stmt
				{ $$ = new ast::ForExprStmt($3, $5, $7, $9, POS(@1));}
			| FOR LPAREN VarDecl EmptyExpr SEMICOLON EmptyExpr RPAREN Stmt
				{ $$ = new ast::ForDeclStmt(dynamic_cast<mind::ast::VarDecl*>($3), $4, $6, $8, POS(@1));}
			;
IfStmt		: IF LPAREN Expr RPAREN Stmt
				{ $$ = new ast::IfStmt($3, $5, new ast::EmptyStmt(POS(@5)), POS(@1)); }
			| IF LPAREN Expr RPAREN Stmt ELSE Stmt
				{ $$ = new ast::IfStmt($3, $5, $7, POS(@1)); }
			;

ReturnStmt  : RETURN Expr SEMICOLON
				{ $$ = new ast::ReturnStmt($2, POS(@1)); }
			;
ExprStmt	: Expr SEMICOLON
				{ $$ = new ast::ExprStmt($1, POS(@1)); } 
			;
EmptyExpr	: Expr
				{ $$ = $1; }
			|	{ $$ = new ast::IntConst(0, 0); } 
			;
Expr		: ICONST
				{ $$ = new ast::IntConst($1, POS(@1)); }			
			| LPAREN Expr RPAREN
				{ $$ = $2; }
			| Expr PLUS Expr
				{ $$ = new ast::AddExpr($1, $3, POS(@2)); }
			| Expr MINUS Expr
				{ $$ = new ast::SubExpr($1, $3, POS(@2)); }
			| Expr TIMES Expr
				{ $$ = new ast::MulExpr($1, $3, POS(@2)); }
			| Expr SLASH Expr
				{ $$ = new ast::DivExpr($1, $3, POS(@2)); }
			| Expr MOD Expr
				{ $$ = new ast::ModExpr($1, $3, POS(@2)); }
			| Expr QUESTION Expr COLON Expr
				{ $$ = new ast::IfExpr($1, $3, $5, POS(@2)); }
			| MINUS Expr
				{ $$ = new ast::NegExpr($2, POS(@1)); }
			| BNOT Expr
				{ $$ = new ast::BitNotExpr($2, POS(@1)); }
			| LNOT Expr
				{ $$ = new ast::NotExpr($2, POS(@1)); }
			| Expr LEQ Expr 
				{ $$ = new ast::LeqExpr($1, $3, POS(@2)); }
			| Expr GEQ Expr
				{ $$ = new ast::GeqExpr($1, $3, POS(@2)); }
			| Expr LT Expr
				{ $$ = new ast::LesExpr($1, $3, POS(@2)); }
			| Expr GT Expr
				{ $$ = new ast::GrtExpr($1, $3, POS(@2)); }
			| Expr EQU Expr
				{ $$ = new ast::EquExpr($1, $3, POS(@2)); }
			| Expr NEQ Expr
				{ $$ = new ast::NeqExpr($1, $3, POS(@2)); }
			| Expr OR Expr
				{ $$ = new ast::OrExpr($1, $3, POS(@2));  }
			| Expr AND Expr
				{ $$ = new ast::AndExpr($1, $3, POS(@2)); }
	 		| IDENTIFIER ASSIGN Expr
				{ $$ = new ast::AssignExpr(new ast::VarRef($1, POS(@1)), $3, POS(@2)); }
	 		| IDENTIFIER
				{ $$ = new ast::LvalueExpr(new ast::VarRef($1, POS(@1)), POS(@1)); }
	 		| IDENTIFIER LPAREN ExprList RPAREN
				{ $$ = new ast::Call($1, $3, POS(@1)); }
			;

VarDecl		: VarDecl1 SEMICOLON { $$ = $1; }
			| VarDecl2 SEMICOLON { $$ = $1; }

VarDecl1	: Type IDENTIFIER
				{ $$ = new ast::VarDecl($2, $1, POS(@1)); }
VarDecl2	: Type IDENTIFIER ASSIGN Expr
				{ $$ = new ast::VarDecl($2, $1, $4, POS(@1)); }
			;


%%

/* SECTION IV: customized section */
#include "compiler.hpp"
#include <cstdio>

static ast::Program* ptree = NULL;
extern int myline, mycol;   // defined in scanner.l

// bison will generate code to invoke me
void
yyerror (char const *msg) {
  err::issue(new Location(myline, mycol), new err::SyntaxError(msg));
  scan_end();
  std::exit(1);
}

// call me when the Program symbol is reduced
void
setParseTree(ast::Program* tree) {
  ptree = tree;
}

/* Parses a given mind source file.
 *
 * PARAMETERS:
 *   filename - name of the source file
 * RETURNS:
 *   the parse tree (in the form of abstract syntax tree)
 * NOTE:
 *   should any syntax error occur, this function would not return.
 */
ast::Program*
mind::MindCompiler::parseFile(const char* filename) {  
  scan_begin(filename);
  /* if (NULL == filename)
	yyin = stdin;
  else
	yyin = std::fopen(filename, "r"); */
  yy::parser parse;
  parse();
  scan_end();
  /* if (yyin != stdin)
	std::fclose(yyin); */
  
  return ptree;
}

void
yy::parser::error (const location_type& l, const std::string& m)
{
  //std::cerr << l << ": " << m << '\n';
  err::issue(new Location(l.begin.line, l.begin.column), new err::SyntaxError(m));
  
  scan_end();
  std::exit(1);
}
