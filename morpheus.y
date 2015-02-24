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

/*%left '||'
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
%left '[' ']' '(' ')'*/

/* enable EBNF grammar syntax */
%ebnf

%%

program
    : statement* EOF
        { return Program({},$1) }
    ;

statement
    : compound_statement
    | if_statement
    | iteration_statement
    | continue_statement
    | break_statement
    | return_statement
    | switch_statement
    | try_statement
    | expression_statement
    | identifier primary_expression* ':' // method definition
    //| identifier prim_expr*
    ;



compound_statement
    : '{' statement_list '}'
    ;

statement_list
    : statement_list statement
    | // empty
    ;

if_statement
    : IF '(' expression ')' statement %prec NO_ELSE
    | IF '(' expression ')' statement ELSE statement
    ;

continue_statement
    : CONTINUE ';'
    | CONTINUE error
    | CONTINUE IDENTIFIER ';'
    | CONTINUE IDENTIFIER error
    ;

break_statement
    : BREAK ';'
    | BREAK error
    | BREAK IDENTIFIER ';'
    | BREAK IDENTIFIER error
    ;

return_statement
    : END ';'
    | END error
    | END IDENTIFIER ';'
    | END IDENTIFIER error
    ;

iteration_statement
    : WHILE '(' expression ')' statement
    //| FOR '(' statement? ';' expr? ';' statement* ')' statement
    | FOR '(' expression ';' expression ';' expression* ')' statement
    ;

try_statement
    : TRY compound_statement CATCH compound_statement
    ;

switch_statement
    : SWITCH '(' expression ')' case_block
    ;

case_block
    : '{' case_clauses '}'
    | '{' case_clauses default_clause case_clauses '}'
    ;

case_clauses
    : case_clauses case_clause
    | // empty
    ;

case_clause
    : CASE (integer|string|identifier) ':' statement_list
    ;

default_clause
    : DEFAULT ':' statement_list
    ;

expression_statement
    : expression ';'
    | expression error
    ;

primary_expression
    : builtin_vars
    | identifier
    | literal
    | array_literal_constant
    | vector_literal
    | '(' expression ')'
    | NULL
    | NIL
    ;

array_literal_constant
    : (literal '::')+ literal
    ;

vector_literal
    : '(' number number number ')'
    ;

literal
    : string
    | number
    ;

member_expression
    : primary_expression
    | function_expression // ?
    | member_expression '[' expression ']'
    | member_expression '.' identifier
    | '$' '(' member_expression ')'
    ;

assignment_expression
    : conditional_expression
    | lefthandside_expression '=' assignment_expression
    | lefthandside_expression assignment_operator assignment_expression
    ;

assignment_operator
    : '*='
    | '/='
    | '%='
    | '+='
    | '-='
    | '<<='
    | '>>='
    | '>>>='
    | '&='
    | '^='
    | '|='
    ;

expression
    : assignment_expression
    | expression ',' assignment_expression // TODO: is this allowed?
    ;

// ternary
conditional_expression
    : logical_or_expression
    //| logical_or_expression '?' assignment_expression ':' assignment_expression
    ;

logical_or_expression
    : logical_and_expression
    | logical_or_expression '||' logical_and_expression
    ;

logical_and_expression
    : bitwise_or_expression
    | logical_and_expression '&&' bitwise_or_expression
    ;

bitwise_or_expression
    : bitwise_xor_expression
    | bitwise_or_expression '|' bitwise_xor_expression
    ;

bitwise_xor_expression
    : bitwise_and_expression
    | bitwise_xor_expression '^' bitwise_and_selection
    ;

bitwise_and_expression
    : equality_expression
    | bitwise_and_expression '&' equality_expression
    ;

equality_expression
    : relational_expression
    | equality_expression '==' relational_expression
    | equality_expression '!=' relational_expression
    ;

relational_expression
    : shift_expression
    | relational_expression '<' shift_expression
    | relational_expression '>' shift_expression
    | relational_expression '<=' shift_expression
    | relational_expression '>=' shift_expression
    ;

// is the shift operator in morpheus?
shift_expression
    : additive_expression
    ;

additive_expression
    : multiplicative_expression
    | additive_expression '+' multiplicative_expression
    | additive_expression '-' multiplicative_expression
    ;

multiplicative_expression
    : unary_expression
    | multiplicative_expression '*' unary_expression
    | multiplicative_expression '/' unary_expression
    | multiplicative_expression '%' unary_expression
    ;

unary_expr
    : '++' unary_expression
    | '--' unary_expression
    | '+' unary_expression
    | '-' unary_expression
    | '~' unary_expression
    | '!' unary_expression
    | ISALIVE unary_expression
    ;

unary_expression
    : postfix_expression
    | unary_expr
    ;

postfix_expression
    : call_expression
    | call_expression '++'
    | call_expression '--'
    ;

call_expression
    : lefthandside_expression
    | call_expression call_literal lefthandside_expression
    ;

lefthandside_expression
    : member_expression
    //| call_expression
    ;

/*call_expression
    : member_expression call_literal primary_expression*
    ;*/

call_literal
    : THREAD
    | WAITTHREAD
    | EXEC
    | WAITEXEC
    | WAITTILL
    ;

func_prim_expr
    //: identifier prim_expr*
    //| nonident_prim_expr identifier prim_expr*
    //: '-' func_prim_expr %prec UMINUS
    : '~' func_prim_expr
    //| '!' func_prim_expr
    | identifier '::' prim_expr
    | nonident_prim_expr '::' prim_expr
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

/* Begin Parser Customization Methods */
var _originalParseMethod = parser.parse;

parser.parse = function(source, args) {
    parser.wasNewLine = false;
    parser.newLine = false;
    parser.restricted = false;

    return _originalParseMethod.call(this, source);
};

// parser.parseError = function(str, hash) {
//     //		alert(JSON.stringify(hash) + "\n\n\n" + parser.newLine + "\n" + parser.wasNewLine + "\n\n" + hash.expected.indexOf("';'"));
//     if (!((hash.expected && hash.expected.indexOf("';'") >= 0) && (hash.token === "}" || hash.token === "EOF" || hash.token === "BR++" || hash.token === "BR--" || parser.newLine || parser.wasNewLine))) {
//         throw new SyntaxError(str);
//     }
// };
