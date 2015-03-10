start
  = __ program:program __ { return program; }

program
  = body:statement*

statement
    = block_statement
    / expression_statement
    / iteration_statement
    / if_statement
    / continue_statement
    / break_statement
    / return_statement
    / labeled_statement
    / empty_statement

block_statement
    = '{' _ statement* _ '}' _

expression_statement
    = expression:expression EOS
        {
            return { type: 'ExpressionStatement', expression: expression };
        }

if_statement
    = 'if' __ '(' __ test:expression __ ')' __ consequent:statement __ 'else' __ alternate:statement
        {
            return { type: 'IfStatement', test: test, consequent: consequent, alternate: alternate };
        }
    / 'if' __ '(' __ test:expression __ ')' __ consequent:statement
        {
            return { type: 'IfStatement', test: test, consequent: consequent, alternate: null };
        }

iteration_statement
    = 'while' __ '(' __ test:expression __ ')' __ body:statement
        {
            return { type: 'WhileStatement', test: test, body: body };
        }
    / 'for' __ '(' __ init:expression __ ';' __ test:expression __ ';' __ update:expression* __ ')' __ body:statement
        {
            return {
              type: 'ForStatement',
              init: init, //extractOptional(init, 0),
              test: test, //extractOptional(test, 0),
              update: update, //extractOptional(update, 0),
              body: body
            };
        }
        
continue_statement
    = 'continue' EOS
        {
            return { type: 'ContinueStatement', label: null };
        }
    / 'continue' _ label:identifier _ EOS
        {
            return { type: 'ContinueStatement', label: label };
        }

break_statement
    = 'break' EOS
        {
            return { type: 'BreakStatement', label: null };
        }
    / 'break' _ identifier EOS
        {
            return { type: 'BreakStatement', label: label };
        }


return_statement
    = 'end' EOS
    / 'end' _ argument:expression EOS
        {
            return { type: 'EndStatement', argument: argument };
        }

labeled_statement
    = label:identifier __ ':'
        {
            return { type: 'LabeledStatement', arguments: [] };
        }
    / label:identifier (_ argument:identifier)+ __ ':'
        {
            return { type: 'LabeledStatement', arguments: [ 'has args' ] };
        }

empty_statement
    = ';'
        {
            return { type: 'EmptyStatement' };
        }

expression
    = numeric_literal / string_literal / vector_literal

identifier
    = [A-Za-z_$][A-Za-z_$0-9]*
        {
            return { type: 'Identifier', name: text() };
        }

numeric_literal
    = '-'? ([0-9]+) ('.' [0-9]+)?
        {
            return { type: 'Literal', value: parseFloat(text(), 10) };
        }

string_literal
    = q [^"]+ q
        {
            return { type: 'Literal', value: text().slice(1, -1) };
        }
    / q q
        {
            return { type: 'Literal', value: '' };
        }

q
    = '"' / '\''

vector_literal
    = '(' _ one:numeric_literal __ two:numeric_literal __ three:numeric_literal _ ')'
        {
            return { type: 'Literal', value: [ one, two, three ] };
        }

/* Skipped */

__
  = (WhiteSpace / LineTerminatorSequence / Comment)*

_
  = (WhiteSpace / MultiLineCommentNoLineTerminator)*

/* Automatic Semicolon Insertion */

EOS
  = __ ";"
  / _ SingleLineComment? LineTerminatorSequence
  / _ &"}"
  / __ EOF

EOF
  = !.

WhiteSpace "whitespace"
  = "\t"
  / "\v"
  / "\f"
  / " "

LineTerminator
  = [\n\r]

LineTerminatorSequence "end of line"
  = "\n"
  / "\r\n"
  / "\r"

Comment "comment"
  = MultiLineComment
  / SingleLineComment

MultiLineComment
  = "/*" (!"*/" SourceCharacter)* "*/"

MultiLineCommentNoLineTerminator
  = "/*" (!("*/" / LineTerminator) SourceCharacter)* "*/"

SingleLineComment
  = "//" (!LineTerminator SourceCharacter)*

SourceCharacter
  = .
