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
    = expression ';' _
    / expression '\n' _

iteration_statement
    = 'while' _ '(' _ expression _ ')' _ statement
    / 'for' _ '(' _ initializer:expression _ ';' _ comparison:expression _ ';' _ increment:expression* _ ')' _ statement _

if_statement
    = 'if' _ '(' expression ')' _ statement _ 'else' _ statement _
    / 'if' _ '(' expression ')' _ statement _

continue_statement
    = 'continue' _ identifier _ ';'
    / 'continue' _ ';'

break_statement
    = 'break' _ identifier _ ';'
    / 'break' _ ';'

return_statement
    = 'end' _ expression _ ';'
    / 'end' _ ';'

labeled_statement
    = identifier ':'
    / identifier (_ identifier)+ ':'

empty_statement
    = ';' _

identifier
    = [A-Za-z_$][A-Za-z_$0-9]+
        {
            return text();
        }

numeric_literal
    = ([0-9]+)('.' [0-9]+)?
        {
            return parseFloat(text(), 10);
        }

string_literal
    = q [^"]+ q
        {
            return text().slice(1, -1);
        }
    / q q
        {
            return '';
        }

q
    = '"' / '\''

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
