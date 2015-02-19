/* BlooP and FlooP parser - http://en.wikipedia.org/wiki/BlooP_and_FlooP */

/* Code blocks are inserted at the top of the generated module. */
%{
  var ast = require('./lib/ast'),

  Program = ast.Program,
  ThreadStatement = ast.ThreadStatement,
  IfStatement = ast.IfStatement,
  CompoundStatement = ast.CompoundStatement,
  StatementList = ast.StatementList,

  ProcedureStmt = ast.ProcedureStmt,
  BlockStmt   = ast.BlockStmt,
  LoopStmt    = ast.LoopStmt,
  MuLoopStmt  = ast.MuLoopStmt,
  NumberLit   = ast.NumberLit,
  BooleanLit  = ast.BooleanLit,
  OutputExpr  = ast.OutputExpr,
  Identifier  = ast.Identifier,
  CellExpr    = ast.CellExpr,
  PlusExpr    = ast.PlusExpr,
  TimesExpr   = ast.TimesExpr,
  ApplyExpr   = ast.ApplyExpr,
  LessCond    = ast.LessCond,
  GreaterCond   = ast.GreaterCond,
  GreaterCond   = ast.GreaterCond,
  EqualCond   = ast.EqualCond,
  CompoundCond  = ast.CompoundCond,
  AssignStmt  = ast.AssignStmt,
  IfThenStmt  = ast.IfThenStmt,
  QuitStmt    = ast.QuitStmt,
  AbortStmt   = ast.AbortStmt;
%}

%nonassoc NO_ELSE
%nonassoc ELSE

%right '=' '+=' '-='

%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%left '==' '!='
%left '<' '<=' '>' '>='
%left '+' '-'
%left '*' '%' '/'
%left '!' '~' UMINUS '++' '--'
%left '.' '::' ':'
%left '[' ']' '(' ')'


/* enable EBNF grammar syntax */
%ebnf

%%

program
    : statement* EOF
        { return Program({},$1) }
    ;

statement
    : identifier prim_expr* ':' // method definition
    | CASE integer prim_expr* ':'
    | CASE identifier prim_expr* ':'
    | compound_statement
    | IF prim_expr statement %prec NO_ELSE
    | IF prim_expr statement ELSE statement
    | WHILE prim_expr statement
    //| FOR '(' statement? ';' expr? ';' statement* ')' statement
    | FOR '(' statement ';' expr ';' statement* ')' statement
    | TRY compound_statement CATCH compound_statement
    | SWITCH prim_expr compound_statement
    | BREAK
    | CONTINUE
    //| identifier prim_expr*
    //| nonident_prim_expr identifier prim_expr*
    | nonident_prim_expr '=' expr
    | nonident_prim_expr '+=' expr
    | nonident_prim_expr '-=' expr
    | nonident_prim_expr '++'
    | nonident_prim_expr '--'
    ;

compound_statement
    : '{' statement* '}'
    ;

expr
    : expr '&&' expr
    | expr '||' expr
    | expr '&' expr
    | expr '|' expr
    | expr '^' expr
    | expr '==' expr
    | expr '!=' expr
    | expr '<' expr
    | expr '>' expr
    | expr '<=' expr
    | expr '>=' expr
    | expr '+' expr
    | expr '-' expr
    | expr '*' expr
    | expr '/' expr
    | expr '%' expr
    | nonident_prim_expr
    | func_prim_expr
    //| identifier // not in original
    ;

func_prim_expr
    //: identifier prim_expr*
    //| nonident_prim_expr identifier prim_expr*
    : '-' func_prim_expr %prec UMINUS
    | '~' func_prim_expr
    | '!' func_prim_expr
    | identifier '::' prim_expr
    | nonident_prim_expr '::' prim_expr
    ;

prim_expr
    : nonident_prim_expr
    //| identifier_prim // what the hell is this
    | prim_expr '::' prim_expr
    ;

nonident_prim_expr
    : '$' '(' prim_expr ')' // targetname operator
    | nonident_prim_expr '.' identifier
    //| nonident_prim_expr '.' size // what is this for?
    | nonident_prim_expr '[' expr ']'
    | string
    | number
    | '(' number number number ')'
    | builtin_vars
    | '(' expr ')'
    | '-' nonident_prim_expr %prec UMINUS
    | '~' nonident_prim_expr
    | '!' nonident_prim_expr
    | NULL
    | NIL
    ;

builtin_vars
    : GAME
    | LEVEL
    | LOCAL
    | PARM
    | SELF
    | GROUP
    ;

identifier
    : IDENTIFIER -> Identifier({value: $1})
    ;

string
    : STRING -> String({value: $1})
    ;

number
    : integer
    | float
    ;

float
    : FLOAT
    ;

integer
    : INTEGER
    ;

%%

// additional user code here
