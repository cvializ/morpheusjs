
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
    : source_elements EOF
        {
            $$ = new ProgramNode($1, createSourceLocation(null, @1, @2));
            return $$;
        }
    ;

source_elements
    : source_elements source_element
        {
            $$ = $1.concat($2);
        }
    | //empty
        {
            $$ = [];
        }
    ;

source_element
    : statement
    ;

statement
    : compound_statement
    | empty_statement
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
        {
            $$ = new BlockStatementNode($2, createSourceLocation(null, @1, @3));
        }
    ;

empty_statement
    : ';'
        {
            return EmptyStatementNode(createSourceLocation(null, @1, @1));
        }
    ;

statement_list
    : statement_list statement
        {
            $$ = $1.concat($2);
        }
    | // empty
        {
            $$ = [];
        }
    ;

if_statement
    : IF '(' expression ')' statement %prec NO_ELSE
        {
            $$ = new IfStatementNode($3, $5, null, createSourceLocation(null, @1, @5));
        }
    | IF '(' expression ')' statement ELSE statement
        {
            $$ = new IfStatementNode($3, $5, $7, createSourceLocation(null, @1, @7));
        }
    ;

continue_statement
    : CONTINUE ';'
        {
            $$ = new ContinueStatementNode(new IdentifierNode($2, createSourceLocation(null, @2, @2)), createSourceLocation(null, @1, @2));
        }
    | CONTINUE error
        {
            $$ = new ContinueStatementNode(new IdentifierNode($2, createSourceLocation(null, @2, @2)), createSourceLocation(null, @1, @3));
        }
    | CONTINUE IDENTIFIER ';'
        {
            $$ = new ContinueStatementNode(null, createSourceLocation(null, @1, @1));
        }
    | CONTINUE IDENTIFIER error
        {
            $$ = new ContinueStatementNode(null, createSourceLocation(null, @1, @2));
        }
    ;


break_statement
    : BREAK ';'
        {
            $$ = new BreakStatementNode(new IdentifierNode($2, createSourceLocation(null, @2, @2)), createSourceLocation(null, @1, @2));
        }
    | BREAK error
        {
            $$ = new BreakStatementNode(new IdentifierNode($2, createSourceLocation(null, @2, @2)), createSourceLocation(null, @1, @3));
        }
    | BREAK IDENTIFIER ';'
        {
            $$ = new BreakStatementNode(null, createSourceLocation(null, @1, @1));
        }
    | BREAK IDENTIFIER error
        {
            $$ = new BreakStatementNode(null, createSourceLocation(null, @1, @2));
        }
    ;

return_statement
    : END ';'
        {
            $$ = new ReturnStatementNode(null, createSourceLocation(null, @1, @2));
        }
    | END error
        {
            $$ = new ReturnStatementNode(null, createSourceLocation(null, @1, @1));
        }
    | END IDENTIFIER ';'
        {
            $$ = new ReturnStatementNode($2, createSourceLocation(null, @1, @3));
        }
    | END IDENTIFIER error
        {
            $$ = new ReturnStatementNode($2, createSourceLocation(null, @1, @2));
        }
    ;

iteration_statement
    : WHILE '(' expression ')' statement
        {
            $$ = new WhileStatementNode($3, $5, createSourceLocation(null, @1, @5));
        }
    | FOR '(' expression ';' expression ';' expression* ')' statement
        {
            $$ = new ForStatementNode($3, $5, $7, $9, createSourceLocation(null, @1, @9));
        }
    ;

try_statement
    : TRY compound_statement catch_clause
        {
            $$ = new TryStatementNode($2, $3, null, createSourceLocation(null, @1, @3));
        }
    ;

catch_clause
    : CATCH "(" IDENTIFIER ")" compound_statement
        {
            $$ = new CatchClauseNode(new IdentifierNode($3, createSourceLocation(null, @3, @3)), $5, createSourceLocation(null, @1, @5));
        }
    ;

switch_statement
    : SWITCH '(' expression ')' case_block
        {
            $$ = new SwitchStatementNode($3, $5, createSourceLocation(null, @1, @5));
        }
    ;

case_block
    : '{' case_clauses '}'
        {
            $$ = $2;
        }
    | '{' case_clauses default_clause case_clauses '}'
        {
            $$ = $2.concat($3).concat($4);
        }
    ;

case_clauses
    : case_clauses case_clause
        {
            $$ = $1.concat($2);
        }
    | // empty
        {
            $$ = [];
        }
    ;

case_clause
    : CASE (integer|string_literal|identifier) ':' statement_list
        {
            $$ = new SwitchCaseNode($2, $4, createSourceLocation(null, @1, @4));
        }
    ;

default_clause
    : DEFAULT ':' statement_list
        {
            $$ = new SwitchCaseNode(null, $3, createSourceLocation(null, @1, @3));
        }
    ;

expression_statement
    : expression ';'
        {
            $$ = new ExpressionStatementNode($1, createSourceLocation(null, @1, @2));
        }
    | expression error
        {
            $$ = new ExpressionStatementNode($1, createSourceLocation(null, @1, @1));
        }
    ;

primary_expression
    : builtin_vars
        {
            $$ = new ThisExpressionNode(createSourceLocation(null, @1, @1));
        }
    | identifier
        {
            $$ = new IdentifierNode($1, createSourceLocation(null, @1, @1));
        }
    | literal
    | array_literal_constant // array literals can't contain array literals, so they're out here
    | vector_literal // vectors can't contain vectors, so they're out here
    | '(' expression ')'
        {
            $$ = $2;
        }
    ;

member_expression
    : primary_expression
    | function_expression // ?
    | member_expression '[' expression ']'
        {
            $$ = new MemberExpressionNode($1, $3, true, createSourceLocation(null, @1, @4));
        }
    | member_expression '.' identifier
        {
            $$ = new MemberExpressionNode($1, $3, false, createSourceLocation(null, @1, @3));
        }
    | '$' '(' member_expression ')'
        {
            $$ = new MemberExpressionNode($1, $3, false, createSourceLocation(null, @1, @4));
        }
    ;

assignment_expression
    : conditional_expression
    | lefthandside_expression '=' assignment_expression
        {
            $$ = new AssignmentExpressionNode('=', $1, $3, createSourceLocation(null, @1, @3));
        }
    | lefthandside_expression assignment_operator assignment_expression
        {
            $$ = new AssignmentExpressionNode($2, $1, $3, createSourceLocation(null, @1, @3));
        }
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
        {
            $$ = new LogicalExpressionNode("||", $1, $3, createSourceLocation(null, @1, @3));
        }
    ;

logical_and_expression
    : bitwise_or_expression
    | logical_and_expression '&&' bitwise_or_expression
        {
            $$ = new LogicalExpressionNode("&&", $1, $3, createSourceLocation(null, @1, @3));
        }
    ;

bitwise_or_expression
    : bitwise_xor_expression
    | bitwise_or_expression '|' bitwise_xor_expression
        {
            $$ = new BinaryExpressionNode("|", $1, $3, createSourceLocation(null, @1, @3));
        }
    ;

bitwise_xor_expression
    : bitwise_and_expression
    | bitwise_xor_expression '^' bitwise_and_selection
        {
            $$ = new BinaryExpressionNode("^", $1, $3, createSourceLocation(null, @1, @3));
        }
    ;

bitwise_and_expression
    : equality_expression
    | bitwise_and_expression '&' equality_expression
        {
            $$ = new BinaryExpressionNode("&", $1, $3, createSourceLocation(null, @1, @3));
        }
    ;

equality_expression
    : relational_expression
    | equality_expression '==' relational_expression
        {
            $$ = new BinaryExpressionNode("==", $1, $3, createSourceLocation(null, @1, @3));
        }
    | equality_expression '!=' relational_expression
        {
            $$ = new BinaryExpressionNode("!=", $1, $3, createSourceLocation(null, @1, @3));
        }
    ;

relational_expression
    : shift_expression
    | relational_expression '<' shift_expression
        {
            $$ = new BinaryExpressionNode("<", $1, $3, createSourceLocation(null, @1, @3));
        }
    | relational_expression '>' shift_expression
        {
            $$ = new BinaryExpressionNode(">", $1, $3, createSourceLocation(null, @1, @3));
        }
    | relational_expression '<=' shift_expression
        {
            $$ = new BinaryExpressionNode("<=", $1, $3, createSourceLocation(null, @1, @3));
        }
    | relational_expression '>=' shift_expression
        {
            $$ = new BinaryExpressionNode(">=", $1, $3, createSourceLocation(null, @1, @3));
        }
    ;

// is the shift operator in morpheus?
shift_expression
    : additive_expression
    ;

additive_expression
    : multiplicative_expression
    | additive_expression '+' multiplicative_expression
        {
            $$ = new BinaryExpressionNode("+", $1, $3, createSourceLocation(null, @1, @3));
        }
    | additive_expression '-' multiplicative_expression
        {
            $$ = new BinaryExpressionNode("-", $1, $3, createSourceLocation(null, @1, @3));
        }
    ;

multiplicative_expression
    : unary_expression
    | multiplicative_expression '*' unary_expression
    {
        $$ = new BinaryExpressionNode("*", $1, $3, createSourceLocation(null, @1, @3));
    }
    | multiplicative_expression '/' unary_expression
    {
        $$ = new BinaryExpressionNode("/", $1, $3, createSourceLocation(null, @1, @3));
    }
    | multiplicative_expression '%' unary_expression
    {
        $$ = new BinaryExpressionNode("%", $1, $3, createSourceLocation(null, @1, @3));
    }
    ;

unary_expr
    : '++' unary_expression
        {
            $$ = new UpdateExpressionNode("++", $2, false, createSourceLocation(null, @1, @2));
        }
    | '--' unary_expression
        {
            $$ = new UpdateExpressionNode("--", $2, false, createSourceLocation(null, @1, @2));
        }
    | '+' unary_expression
        {
            $$ = new UnaryExpressionNode("+", true, $2, createSourceLocation(null, @1, @2));
        }
    | '-' unary_expression
        {
            $$ = new UnaryExpressionNode("-", true, $2, createSourceLocation(null, @1, @2));
        }
    | '~' unary_expression
        {
            $$ = new UnaryExpressionNode("~", true, $2, createSourceLocation(null, @1, @2));
        }
    | '!' unary_expression
        {
            $$ = new UnaryExpressionNode("!", true, $2, createSourceLocation(null, @1, @2));
        }
    | ISALIVE unary_expression
        {
            $$ = new UnaryExpressionNode("ISALIVE", true, $2, createSourceLocation(null, @1, @2));
        }
    ;

unary_expression
    : postfix_expression
    | unary_expr
    ;

postfix_expression
    : call_expression
    | call_expression '++'
        {
            $$ = new UpdateExpressionNode("++", $1, false, createSourceLocation(null, @1, @2));
        }
    | call_expression '--'
        {
            $$ = new UpdateExpressionNode("--", $1, false, createSourceLocation(null, @1, @2));
        }
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
    : IDENTIFIER
        {
            $$ = new IdentifierNode($1, createSourceLocation(null, @1, @1));
        }
    ;


literal
    : string_literal
    | numeric_literal
    | null_literal
    | nil_literal
    ;


array_literal_constant
    : literal '::' literal
        {
            $$ = new ConstantArrayLiteralNode([$1, $3], createSourceLocation(null, @1, @3));
        }
    | array_literal_constant '::' literal
        {
            $1.elements.concat($3);
            $1.loc = createSourceLocation(null, @1, @3);
            $$ = $1;
        }
    ;

vector_literal
    : '(' numeric_literal numeric_literal numeric_literal ')'
        {
            $$ = new LiteralNode([$2, $3, $4], createSourceLocation(null, @1, @5));
        }
    ;

null_literal
    : NULL
        {
            $$ = new LiteralNode(null, createSourceLocation(null, @1, @1));
        }
    ;

nil_literal
    : NIL
        {
            $$ = new LiteralNode(undefined, createSourceLocation(null, @1, @1));
        }
    ;

string_literal
    : STRING
        {
            $$ = new LiteralNode($1, createSourceLocation(null, @1, @1));
        }
    ;

numeric_literal
    : integer
        {
            $$ = new LiteralNode(+($1), createSourceLocation(null, @1, @1));
        }
    | float
        {
            $$ = new LiteralNode(+($1), createSourceLocation(null, @1, @1));
        }
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

parser.parseError = function(str, hash) {
    //		alert(JSON.stringify(hash) + "\n\n\n" + parser.newLine + "\n" + parser.wasNewLine + "\n\n" + hash.expected.indexOf("';'"));
    if (!((hash.expected && hash.expected.indexOf("';'") >= 0) && (hash.token === "}" || hash.token === "EOF" || hash.token === "BR++" || hash.token === "BR--" || parser.newLine || parser.wasNewLine))) {
        throw new SyntaxError(str);
    }
};

function createSourceLocation(source, firstToken, lastToken) {
    return new SourceLocation(source, new Position(firstToken.first_line, firstToken.first_column), new Position(lastToken.last_line, lastToken.last_column));
}

/* Begin AST Node Constructors */
function ProgramNode(body, loc) {
    this.type = "Program";
    this.body = body;
    this.loc = loc;
}

function EmptyStatementNode(loc) {
    this.type = "EmptyStatement";
    this.loc = loc;
}

function BlockStatementNode(body, loc) {
    this.type = "BlockStatement";
    this.body = body;
    this.loc = loc;
}

function ExpressionStatementNode(expression, loc) {
    this.type = "ExpressionStatement";
    this.expression = expression;
    this.loc = loc;
}

function IfStatementNode(test, consequent, alternate, loc) {
    this.type = "IfStatement";
    this.test = test;
    this.consequent = consequent;
    this.alternate = alternate;
    this.loc = loc;
}

function LabeledStatementNode(label, body, loc) {
    this.type = "LabeledStatement";
    this.label = label;
    this.body = body;
    this.loc = loc;
}

function BreakStatementNode(label, loc) {
    this.type = "BreakStatement";
    this.label = label;
    this.loc = loc;
}

function ContinueStatementNode(label, loc) {
    this.type = "ContinueStatement";
    this.label = label;
    this.loc = loc;
}

function WithStatementNode(object, body, loc) {
    this.type = "WithStatement";
    this.object = object;
    this.body = body;
    this.loc = loc;
}

function SwitchStatementNode(discriminant, cases, loc) {
    this.type = "SwitchStatement";
    this.discriminant = discriminant;
    this.cases = cases;
    this.loc = loc;
}

function ReturnStatementNode(argument, loc) {
    this.type = "ReturnStatement";
    this.argument = argument;
    this.loc = loc;
}

function ThrowStatementNode(argument, loc) {
    this.type = "ThrowStatement";
    this.argument = argument;
    this.loc = loc;
}

function TryStatementNode(block, handlers, finalizer, loc) {
    this.type = "TryStatement";
    this.block = block;
    this.handlers = handlers; // Multiple catch clauses are SpiderMonkey specific
    this.finalizer = finalizer;
    this.loc = loc;
}

function WhileStatementNode(test, body, loc) {
    this.type = "WhileStatement";
    this.test = test;
    this.body = body;
    this.loc = loc;
}

function DoWhileStatementNode(body, test, loc) {
    this.type = "DoWhileStatement";
    this.body = body;
    this.test = test;
    this.loc = loc;
}

function ForStatementNode(init, test, update, body, loc) {
    this.type = "ForStatement";
    this.init = init;
    this.test = test;
    this.update = update;
    this.body = body;
    this.loc = loc;
}

function ForInStatementNode(left, right, body, loc) {
    this.type = "ForInStatement";
    this.left = left;
    this.right = right;
    this.body = body;
    this.loc = loc;
}

function DebugggerStatementNode(loc) {
    this.type = "DebuggerStatement";
    this.loc = loc;
}

function FunctionDeclarationNode(id, params, body, generator, expression, loc) {
    this.type = "FunctionDeclaration";
    this.id = id;
    this.params = params;
    this.body = body;
    this.generator = generator;
    this.expression = expression;
    this.loc = loc;
}

function VariableDeclarationNode(declarations, kind, loc) {
    this.type = "VariableDeclaration";
    this.declarations = declarations;
    this.kind = kind;
    this.loc = loc;
}

function VariableDeclaratorNode(id, init, loc) {
    this.type = "VariableDeclarator";
    this.id = id;
    this.init = init;
    this.loc = loc;
}

function ThisExpressionNode(loc) {
    this.type = "ThisExpression";
    this.loc = loc;
}

function ArrayExpressionNode(elements, loc) {
    this.type = "ArrayExpression";
    this.elements = elements;
    this.loc = loc;
}

function ConstantArrayLiteralNode(elements, loc) {
    this.type = "ConstantArrayLiteral";
    this.elements = elements;
    this.loc = loc;
}

function ObjectExpressionNode(properties, loc) {
    this.type = "ObjectExpression";
    this.properties = properties;
    this.loc = loc;
}

function FunctionExpressionNode(id, params, body, generator, expression, loc) {
    this.type = "FunctionExpression";
    this.id = id;
    this.params = params;
    this.body = body;
    this.generator = generator;
    this.expression = expression;
    this.loc = loc;
}

function SequenceExpressionNode(expressions, loc) {
    this.type = "SequenceExpression";
    this.expressions = expressions;
    this.loc = loc;
}

function UnaryExpressionNode(operator, prefix, argument, loc) {
    this.type = "UnaryExpression";
    this.operator = operator;
    this.prefix = prefix;
    this.argument = argument;
    this.loc = loc;
}

function BinaryExpressionNode(operator, left, right, loc) {
    this.type = "BinaryExpression";
    this.operator = operator;
    this.left = left;
    this.right = right;
    this.loc = loc;
}

function AssignmentExpressionNode(operator, left, right, loc) {
    this.type = "AssignmentExpression";
    this.operator = operator;
    this.left = left;
    this.right = right;
    this.loc = loc;
}

function UpdateExpressionNode(operator, argument, prefix, loc) {
    this.type = "UpdateExpression";
    this.operator = operator;
    this.argument = argument;
    this.prefix = prefix;
    this.loc = loc;
}

function LogicalExpressionNode(operator, left, right, loc) {
    this.type = "LogicalExpression";
    this.operator = operator;
    this.left = left;
    this.right = right;
    this.loc = loc;
}

function ConditionalExpressionNode(test, consequent, alternate, loc) {
    this.type = "ConditionalExpression";
    this.test = test;
    this.consequent = consequent;
    this.alternate = alternate;
    this.loc = loc;
}

function NewExpressionNode(callee, args, loc) {
    this.type = "NewExpression";
    this.callee = callee;
    this.arguments = args;
    this.loc = loc;
}

function CallExpressionNode(callee, args, loc) {
    this.type = "CallExpression";
    this.callee = callee;
    this.arguments = args;
    this.loc = loc;
}

function MemberExpressionNode(object, property, computed, loc) {
    this.type = "MemberExpression";
    this.object = object;
    this.property = property;
    this.computed = computed;
    this.loc = loc;
}

function SwitchCaseNode(test, consequent, loc) {
    this.type = "SwitchCase";
    this.test = test;
    this.consequent = consequent;
    this.loc = loc;
}

function CatchClauseNode(param, body, loc) {
    this.type = "CatchClause";
    this.param = param;
    this.guard = null; /* Firefox specific */
    this.body = body;
    this.loc = loc;
}

function IdentifierNode(name, loc) {
    this.type = "Identifier";
    this.name = name;
    this.loc = loc;
}

function LiteralNode(value, loc) {
    this.type = "Literal";
    this.value = value;
    this.loc = loc;
}

function SourceLocation(source, start, end) {
    this.source = source;
    this.start = start;
    this.end = end;
}

function Position(line, column) {
    this.line = line;
    this.column = column;
}

/* Expose the AST Node Constructors */
parser.ast = {};
parser.ast.ProgramNode = ProgramNode;
parser.ast.EmptyStatementNode = EmptyStatementNode;
parser.ast.BlockStatementNode = BlockStatementNode;
parser.ast.ExpressionStatementNode = ExpressionStatementNode;
parser.ast.IfStatementNode = IfStatementNode;
parser.ast.LabeledStatementNode = LabeledStatementNode;
parser.ast.BreakStatementNode = BreakStatementNode;
parser.ast.ContinueStatementNode = ContinueStatementNode;
parser.ast.WithStatementNode = WithStatementNode;
parser.ast.SwitchStatementNode = SwitchStatementNode;
parser.ast.ReturnStatementNode = ReturnStatementNode;
parser.ast.ThrowStatementNode = ThrowStatementNode;
parser.ast.TryStatementNode = TryStatementNode;
parser.ast.WhileStatementNode = WhileStatementNode;
parser.ast.DoWhileStatementNode = DoWhileStatementNode;
parser.ast.ForStatementNode = ForStatementNode;
parser.ast.ForInStatementNode = ForInStatementNode;
parser.ast.DebugggerStatementNode = DebugggerStatementNode;
parser.ast.FunctionDeclarationNode = FunctionDeclarationNode;
parser.ast.VariableDeclarationNode = VariableDeclarationNode;
parser.ast.VariableDeclaratorNode = VariableDeclaratorNode;
parser.ast.ThisExpressionNode = ThisExpressionNode;
parser.ast.ArrayExpressionNode = ArrayExpressionNode;
parser.ast.ObjectExpressionNode = ObjectExpressionNode;
parser.ast.FunctionExpressionNode = FunctionExpressionNode;
parser.ast.SequenceExpressionNode = SequenceExpressionNode;
parser.ast.UnaryExpressionNode = UnaryExpressionNode;
parser.ast.BinaryExpressionNode = BinaryExpressionNode;
parser.ast.AssignmentExpressionNode = AssignmentExpressionNode;
parser.ast.UpdateExpressionNode = UpdateExpressionNode;
parser.ast.LogicalExpressionNode = LogicalExpressionNode;
parser.ast.ConditionalExpressionNode = ConditionalExpressionNode;
parser.ast.NewExpressionNode = NewExpressionNode;
parser.ast.CallExpressionNode = CallExpressionNode;
parser.ast.MemberExpressionNode = MemberExpressionNode;
parser.ast.SwitchCaseNode = SwitchCaseNode;
parser.ast.CatchClauseNode = CatchClauseNode;
parser.ast.IdentifierNode = IdentifierNode;
parser.ast.LiteralNode = LiteralNode;
