/* **********************************************************
 * Copyright 2009 VMware, Inc.  All rights reserved.
 * -- VMware Confidential
 * **********************************************************/

/*
 * Emmett.mly: the grammar for Emmett, a language for specifying
 * dynamic instrumentation. Since the expression and type syntax is a
 * subset of C, most of this file is a mechanical translation of the
 * grammar in the appendix to the ISO C 99 standard.
 */

%{
  open Globals
  open Ast
  open Actions
  open Symtab
  open Printf
  open Lexing
%}

%token <int64>  INTCONST
%token <string> STRCONST
%token <string> PNAME
%token <string> IDENT
%token <int>    ASSERT
%token <string> TYPE_NAME
%token <string> MEMMODEL_SPEC
%token <string> SYMFILE_SPEC
%token <string * string> TARGET_SPEC

%token
  EOF ELSE IF RETURN SIZEOF TYPEDEF STATIC EXTERN REGISTER AUTO VOID CHAR
  INT LONG SHORT SIGNED UNSIGNED STRUCT UNION ENUM VOLATILE CONST
  RIGHT_ASSIGN LEFT_ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN
  MOD_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN AGGR_ASSIGN
  RIGHT_OP LEFT_OP INC_OP PTR_OP AND_OP OR_OP
  LT_OP GT_OP LE_OP GE_OP EQ_OP NE_OP
  SEMI LBRACE RBRACE COMMA COLON ASSIGN LPAREN RPAREN LBRACK RBRACK
  DOT AMPERSAND BANG TILDA QUESTION MINUS PLUS STAR DIV MOD CARET BAR
  STRING BAG AGGR AT MEMMODEL
  PERTHREAD PERVM PERVMK PERHOST

%type <unit> program

%left  COMMA
%right ASSIGN, ADD_ASSIGN, SUB_ASSIGN, MUL_ASSIGN, DIV_ASSIGN, MOD_ASSIGN,
       AND_ASSIGN, OR_ASSIGN, XOR_ASSIGN, RIGHT_ASSIGN, LEFT_ASSIGN
%right QUESTION, COLON
%left  OR_OP
%left  AND_OP
%left  BAR
%left  CARET
%left  AMPERSAND
%left  EQ_OP, NE_OP
%left  LT_OP, GT_OP, LE_OP, GE_OP
%left  RIGHT_OP, LEFT_OP
%left  PLUS, MINUS
%left  STAR, DIV, PERCENT, CONST, RESTRICT, VOLATILE
%right BANG, TILDA, INC_OP, RPAREN, SIZEOF
%left  LBRACK
%left  DOT, PTR_OP, LPAREN, LBRACE
%right TYPE_NAME
%left  IDENT

%nonassoc PNAME

%nonassoc IF
%nonassoc ELSE

%start program

%%

primary_expr:
  | IDENT              { actionsExprIdent $1 }
  | INTCONST           { ExprIntConst $1 }
  | string_seq         { astStrConcat $1 }
  | LPAREN expr RPAREN { $2 }

string_seq:
  | STRCONST            { [$1] }
  | STRCONST string_seq { $1 :: $2 }

call_expr:
  | IDENT  LPAREN RPAREN            { ExprCall($1, []) }
  | IDENT  LPAREN expr_list RPAREN  { ExprCall($1, $3) }
  | ASSERT LPAREN expr_list RPAREN  { actionsAssert $1 $3 }

aggr_assign_expr:
  | aggr_expr AGGR_ASSIGN assignment_expr { actionsAssignBagOrAggr $1 $3 }
  | aggr_expr INC_OP                      { actionsAggrInc $1 }

aggr_expr:
  | IDENT                         { ExprIdent $1 }
  | IDENT LBRACK expr_list RBRACK { let id = ExprIdent $1 in
                                    actionsExprIndexed id $3 }

postfix_expr:
  | primary_expr { $1 }
  | call_expr    { $1 }
  | postfix_expr INC_OP                  { actionsAggrInc $1 }
  | postfix_expr LBRACK expr_list RBRACK { actionsExprIndexed $1 $3 }
  | postfix_expr DOT IDENT               { ExprField(false, $1, $3) }
  | postfix_expr PTR_OP IDENT            { ExprField(true, $1, $3) }

unary_expr:
  | postfix_expr { $1 }
  | AMPERSAND cast_expr             { actionsExprAddr($2) }
  | STAR cast_expr                  { ExprPointer($2) }
  | PLUS cast_expr                  { exprUnary("+", $2) }
  | MINUS cast_expr                 { exprUnary("-", $2) }
  | TILDA cast_expr                 { exprUnary("~", $2) }
  | BANG cast_expr                  { exprUnary("!", $2) }
  | SIZEOF unary_expr               { actionsSizeOfE $2 }
  | SIZEOF LPAREN type_name RPAREN  { actionsSizeOfT $3 }

cast_expr:
  | unary_expr { $1 }
  | LPAREN type_name RPAREN cast_expr { ExprCast($2, $4) }

multiplicative_expr:
  | cast_expr { $1 }
  | multiplicative_expr STAR cast_expr { exprBinary("*", $1, $3) }
  | multiplicative_expr DIV  cast_expr { exprBinary("/", $1, $3) }
  | multiplicative_expr MOD  cast_expr { exprBinary("%", $1, $3) }

additive_expr:
  | multiplicative_expr { $1 }
  | additive_expr PLUS  multiplicative_expr { exprBinary("+", $1, $3) }
  | additive_expr MINUS multiplicative_expr { exprBinary("-", $1, $3) }

shift_expr:
  | additive_expr { $1 }
  | shift_expr LEFT_OP  additive_expr { exprBinary("<<", $1, $3) }
  | shift_expr RIGHT_OP additive_expr { exprBinary(">>", $1, $3) }

relational_expr:
    shift_expr { $1 }
  | relational_expr LT_OP shift_expr { exprBinary("<", $1, $3) }
  | relational_expr GT_OP shift_expr { exprBinary(">", $1, $3) }
  | relational_expr LE_OP shift_expr { exprBinary("<=", $1, $3) }
  | relational_expr GE_OP shift_expr { exprBinary(">=", $1, $3) }

equality_expr:
  | relational_expr { $1 }
  | equality_expr EQ_OP relational_expr { exprBinary("==", $1, $3) }
  | equality_expr NE_OP relational_expr { exprBinary("!=", $1, $3) }

and_expr:
  | equality_expr { $1 }
  | and_expr AMPERSAND equality_expr { exprBinary("&", $1, $3) }

exclusive_or_expr:
  | and_expr { $1 }
  | exclusive_or_expr CARET and_expr { exprBinary("^", $1, $3) }

inclusive_or_expr:
  | exclusive_or_expr { $1 }
  | inclusive_or_expr BAR exclusive_or_expr { exprBinary("|", $1, $3) }

logical_and_expr:
  | inclusive_or_expr { $1 }
  | logical_and_expr AND_OP inclusive_or_expr  { exprBinary("&&", $1, $3) }

logical_or_expr:
  | logical_and_expr { $1 }
  | logical_or_expr OR_OP logical_and_expr { exprBinary("||", $1, $3) }

conditional_expr:
  | logical_or_expr { $1 }
  | logical_or_expr QUESTION expr COLON conditional_expr
       { ExprCond($1, $3, $5) }

assignment_expr:
  | conditional_expr { $1 }
  | assign_expr      { $1 }
  | unary_expr AGGR_ASSIGN assignment_expr { actionsAssignBagOrAggr $1 $3 }

assign_expr:
  | IDENT ASSIGN       assignment_expr     { actionsAssign ""   $1 $3 }
  | IDENT MUL_ASSIGN   assignment_expr     { actionsAssign "*"  $1 $3 }
  | IDENT DIV_ASSIGN   assignment_expr     { actionsAssign "/"  $1 $3 }
  | IDENT MOD_ASSIGN   assignment_expr     { actionsAssign "%"  $1 $3 }
  | IDENT ADD_ASSIGN   assignment_expr     { actionsAssign "+"  $1 $3 }
  | IDENT SUB_ASSIGN   assignment_expr     { actionsAssign "-"  $1 $3 }
  | IDENT LEFT_ASSIGN  assignment_expr     { actionsAssign "<<" $1 $3 }
  | IDENT RIGHT_ASSIGN assignment_expr     { actionsAssign ">>" $1 $3 }
  | IDENT AND_ASSIGN   assignment_expr     { actionsAssign "&"  $1 $3 }
  | IDENT XOR_ASSIGN   assignment_expr     { actionsAssign "^"  $1 $3 }
  | IDENT OR_ASSIGN    assignment_expr     { actionsAssign "|"  $1 $3 }

expr_list:
  | assignment_expr                 { [$1] }
  | assignment_expr COMMA expr_list { $1 :: $3 }

expr:
  | expr_list { actionsExprComma($1) }

positive_int64:
  | conditional_expr { actionsPositiveInt64 $1 }

positive_int31:
  | conditional_expr { actionsPositiveInt31 $1 }

declaration:
  | declaration_specifiers  SEMI                     { }
  | declaration_specifiers init_declarator_list SEMI { actionsDecls $1 $2 }

declaration_specifiers:
  | type_qualifier                                 { [$1], [], [] }
  | type_qualifier declaration_specifiers          { actionsMergeQ $1 $2 }
  | storage_class_specifier                        { [], [$1], [] }
  | storage_class_specifier declaration_specifiers { actionsMergeS $1 $2 }
  | type_specifier                                 { [], [], [$1] }
  | type_specifier declaration_specifiers          { actionsMergeT $1 $2 }

init_declarator_list:
  | init_declarator                            { [$1] }
  | init_declarator COMMA init_declarator_list { $1 :: $3 }

init_declarator:
  | declarator                         { $1, None }
  | declarator ASSIGN conditional_expr { $1, Some $3 }

storage_class_specifier:
  | TYPEDEF  { ClassTypedef }
  | EXTERN   { ClassExtern }
  | STATIC   { ClassStatic }
  | AUTO     { ClassAuto }
  | REGISTER { ClassRegister }
  | PERTHREAD { ClassPerThread }
  | PERVM     { ClassPerVM     }
  | PERVMK    { ClassPerVMK    }
  | PERHOST   { ClassPerHost   }

type_specifier:
  | VOID                      { SpecVoid }
  | CHAR                      { SpecInt("char") }
  | SHORT                     { SpecInt("short") }
  | INT                       { SpecInt("int") }
  | LONG                      { SpecInt("long") }
  | SIGNED                    { SpecSign("signed") }
  | UNSIGNED                  { SpecSign("unsigned") }
  | STRING                    { SpecString }
  | BAG                       { SpecBag }
  | AGGR                      { SpecAggr }
  | struct_or_union_specifier { $1 }
  | enum_specifier            { $1 }
  | TYPE_NAME                 { SpecTypeName($1) }

/*
 * The lexer returns a TYPE_NAME token whenever the matched symbol is 
 * the name of a typedef type. Unfortunately, if a symbol is used both 
 * as a typedef name and as a struct (or union, or enum) name, then the
 * lexer's choice may be wrong. See PR 555228. Example:
 * 
 * typedef struct A { int x; } A;
 * struct A *y;  // The lexer returns TYPE_NAME for A, instead of IDENT
 *
 * This non-terminal is a workaround for this problem. It allows 
 * accepting both a typename and an identifer in a place where an 
 * identifier is needed, undoing the wrong choice of the lexer.
 */
ident_or_type:
  | IDENT     { $1 }
  | TYPE_NAME { $1 }

struct_or_union_specifier:
  | STRUCT ident_or_type { actionsStructDecl $2 KSTRUCT }
  | UNION  ident_or_type { actionsStructDecl $2 KUNION }
  | struct_or_union_lbrace struct_declaration_list RBRACE
                         { actionsStructPop $1 }

struct_or_union_lbrace:
  | STRUCT ident_or_type LBRACE { actionsStructPush $2 KSTRUCT }
  | UNION  ident_or_type LBRACE { actionsStructPush $2 KUNION }
  | STRUCT LBRACE  { actionsStructPush (freshStructName()) KSTRUCT }
  | UNION  LBRACE  { actionsStructPush (freshStructName()) KUNION }

struct_declaration_list:
  |                                            { }
  | struct_declaration struct_declaration_list { }

offset:
  | AT positive_int64 { $2 }

struct_declaration:
  | specifier_qualifier_list struct_declarator_list SEMI 
      { actionsDeclFields $1 $2 }
  | offset specifier_qualifier_list struct_declarator SEMI
      { actionsDeclField $1 $2 $3 }

specifier_qualifier_list:
  | type_qualifier                          { [$1], [], [] }
  | type_qualifier specifier_qualifier_list { actionsMergeQ $1 $2 }
  | type_specifier                          { [], [], [$1] }
  | type_specifier specifier_qualifier_list { actionsMergeT $1 $2 }

struct_declarator_list:
  | struct_declarator                              { [$1] }
  | struct_declarator COMMA struct_declarator_list { $1 :: $3 }

struct_declarator:
  | declarator             { $1, -1 }
  | field_width            { DeclNull, $1 }
  | declarator field_width { $1, $2 }

field_width:
  | COLON positive_int31 { $2 }

enum_specifier:
  | ENUM ident_or_type                       { SpecInt "long" }
  | enum_ident_lbrace enumerator_list RBRACE { $1 }

enum_ident_lbrace:
  | ENUM LBRACE               { actionsEnum (freshStructName()) }
  | ENUM ident_or_type LBRACE { actionsEnum $2 }

enumerator_list:
  | enumerator { }
  | enumerator COMMA enumerator_list { }

enumerator:
  | IDENT                         { symtabEnumConstDecl $1 None }
  | IDENT ASSIGN conditional_expr { symtabEnumConstDecl $1 (Some $3) }

type_qualifier:
  | CONST    { QUAL_CONST }
  | VOLATILE { QUAL_VOLATILE }

declarator:
  | pointer direct_declarator { actionsDeclPtrMerge $2 $1 }
  | direct_declarator         { $1 }

direct_declarator:
  | IDENT                            { DeclId $1 }
  | LPAREN declarator RPAREN         { $2 }
  | direct_declarator LBRACK positive_int31 RBRACK
                                     { DeclArray($1, Some $3) }
  | direct_declarator LBRACK RBRACK  { DeclArray($1, None) }
  | direct_declarator LPAREN parameter_list_opt RPAREN
                                     { DeclFunc($1, $3) }

pointer:
  | STAR                             { DeclPointer(DeclNull, []) }
  | STAR type_qualifier_list         { DeclPointer(DeclNull, $2) }
  | STAR pointer                     { DeclPointer($2, []) }
  | STAR type_qualifier_list pointer { DeclPointer($3, $2) }

type_qualifier_list:
  | type_qualifier                     { [$1] }
  | type_qualifier type_qualifier_list { $1 :: $2 }

parameter_list_opt:
  |                                                { [] }
  | parameter_declaration                          { [$1] }
  | parameter_declaration COMMA parameter_list_opt { $1 :: $3 }

parameter_declaration:
  | declaration_specifiers                     { None , specToType $1 }
  | declaration_specifiers declarator          { specDeclToPair $1 $2 }
  | declaration_specifiers abstract_declarator { specDeclToPair $1 $2 }

type_name:
  | specifier_qualifier_list                     { specToType $1 }
  | specifier_qualifier_list abstract_declarator { specDeclToType $1 $2 }

abstract_declarator:
  | pointer                            { $1 }
  | direct_abstract_declarator         { $1 }
  | pointer direct_abstract_declarator { actionsDeclPtrMerge $2 $1 }

direct_abstract_declarator:
  | LPAREN abstract_declarator RPAREN        { $2 }
  | LBRACK RBRACK                            { DeclArray(DeclNull, None) }
  | LBRACK positive_int31 RBRACK             { DeclArray(DeclNull, Some $2) }
  | direct_abstract_declarator LBRACK RBRACK { DeclArray($1, None) }
  | direct_abstract_declarator LBRACK positive_int31 RBRACK
                                             { DeclArray($1, Some $3) }
  | LPAREN parameter_list_opt RPAREN         { DeclFunc(DeclNull, $2) }
  | direct_abstract_declarator LPAREN parameter_list_opt RPAREN
                                             { DeclFunc($1, $3) }

stmnt:
  | compound_stmnt  { $1 }
  | expr_stmnt      { $1 }
  | selection_stmnt { $1 }

/*
 * Statements representing probe bodies must avoid conflicts with
 * probe argument lists. In particular, probe bodies must not begin
 * with an open paren. We restrict probe bodies to block statements,
 * if's, assignments, and aggregate operations.
 */
probe_stmnt:
  | compound_stmnt        { $1 }
  | selection_stmnt       { $1 }
  | call_expr SEMI        { StatExpr($1) }
  | assign_expr SEMI      { StatExpr($1) }
  | aggr_assign_expr SEMI { StatExpr($1) }

compound_stmnt:
  | LBRACE declaration_list stmnt_list RBRACE { StatBlock($3) }
  | LBRACE stmnt_list RBRACE { StatBlock($2) }

declaration_list:
  | declaration                  { [] }
  | declaration declaration_list { $1 :: $2 }

stmnt_list:
  |                  { [] }
  | stmnt stmnt_list { $1 :: $2 }

expr_stmnt:
  | SEMI       { StatEmpty }
  | expr SEMI  { StatExpr($1) }

selection_stmnt:
  | IF LPAREN expr RPAREN stmnt            { StatIf($3, $5) }
  | IF LPAREN expr RPAREN stmnt ELSE stmnt { StatIfElse($3, $5, $7) }

probe_definition:
  | probe_def_begin probe_stmnt { actionsProbeDefFinish $1 $2 }

/*
 * Probe names follow special lexical rules. In the simplest case,
 * a probe name can be an IDENT. The more complex names are
 * recognized as PNAME tokens.
 *
 * We allow either IDENT or PNAME in a probe declaration, because
 * the lexer doesn't know whether an IDENT is in fact a probe name.
 */
probe_name:
  | IDENT  { $1 }
  | PNAME  { $1 }

probe_def_begin:
  | probe_name                                   { actionsProbeDefBegin $1 [] }
  | probe_name LPAREN parameter_list_opt RPAREN  { actionsProbeDefBegin $1 $3 }

function_definition:
  | function_def_begin return_compound_stmnt { actionsFuncDefFinish $1 $2 }

function_def_begin:
  | declaration_specifiers declarator { actionsFuncDefBegin $1 $2 }

return_compound_stmnt:
  | LBRACE declaration_list return_stmnt_list RBRACE { StatBlock($3) }
  | LBRACE return_stmnt_list RBRACE { StatBlock($2) }

return_stmnt_list:
  |                         { [] }
  | RETURN expr SEMI        { [StatReturn $2] }
  | stmnt return_stmnt_list { $1 :: $2 }

memmodel:
  | MEMMODEL IDENT SEMI { symtabSetMemModel $2 }

ext_declaration:
  | memmodel            { }
  | declaration         { }
  | function_definition { }
  | probe_definition    { }

ext_declaration_list:
  |                                      { [] }
  | ext_declaration ext_declaration_list { $1 :: $2 }

spec:
  | TARGET_SPEC   { actionsEnvSpec $1  } 
  | MEMMODEL_SPEC { defaultMemModel := $1 }
  | SYMFILE_SPEC  { symbolFile := $1 }

spec_sublist:
  |                     { }
  | spec spec_sublist   { }

spec_list:
  | spec_sublist        { actionsSpecListDone() }

program:
  | spec_list ext_declaration_list EOF  { () }
