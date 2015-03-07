%x COMMENT

ALPHA       [$A-Za-z_]
NATURAL     ([1-9][0-9]+|[0-9])
INTEGER     \-?{NATURAL}+
FLOAT       {INTEGER}+\.{NATURAL}+
ID          {ALPHA}({ALPHA}|[0-9])*
QUOTE       "'"
DQUOTE      "\""

%options flex

%%

/* morpheus only accepts C++ style double-slash comments.
   it does not accept C-style comments. go figure */

"//"                  this.begin('COMMENT');
<COMMENT>[^\n]*       /* consume */
<COMMENT>\n           this.begin('INITIAL');


\s+                             %{
                                    if (yytext.match(/\r|\n/)) {
                                        parser.newLine = true;
                                    }

                                    if (parser.restricted && parser.newLine) {
                                        this.unput(yytext);
                                        parser.restricted = false;
                                        return ";";
                                    }
                                %}
"/*"(.|\r|\n)*?"*/"             %{
                                    if (yytext.match(/\r|\n/)) {
                                        parser.newLine = true;
                                    }

                                    if (parser.restricted && parser.newLine) {
                                        this.unput(yytext);
                                        parser.restricted = false;
                                        return ";";
                                    }
                                %}
"//".*($|\r\n|\r|\n)            %{
                                    if (yytext.match(/\r|\n/)) {
                                        parser.newLine = true;
                                    }

                                    if (parser.restricted && parser.newLine) {
                                        this.unput(yytext);
                                        parser.restricted = false;
                                        return ";";
                                    }
                                %}


{QUOTE}.*{QUOTE}                parser.restricted = false; return 'STRING';
{DQUOTE}.*{DQUOTE}              parser.restricted = false; return 'STRING';
break                           parser.restricted = true; return 'BREAK'
case                            return 'CASE'
catch                           return 'CATCH'
continue                        parser.restricted = true; return 'CONTINUE'
default                         return 'DEFAULT'
else                            return 'ELSE'
end                             parser.retricted = true; return 'END' // like javascript return
if                              return 'IF'
for                             return 'FOR'
switch                          return 'SWITCH'
try                             return 'TRY'
while                           return 'WHILE'
local                           /*parser.restricted = false;*/ return 'LOCAL'
level                           /*parser.restricted = false;*/ return 'LEVEL'
game                            /*parser.restricted = false;*/ return 'GAME'
parm                            /*parser.restricted = false;*/ return 'PARM'
self                            /*parser.restricted = false;*/ return 'SELF' // like javascript this
group                           /*parser.restricted = false;*/ return 'GROUP'
true                            /*parser.restricted = false;*/ return 'TRUE'
false                           /*parser.restricted = false;*/ return 'FALSE'
NULL                            /*parser.restricted = false;*/ return 'NULL'
NIL                             /*parser.restricted = false;*/ return 'NIL'
thread                          parser.restricted = true; return 'THREAD'
waitthread                      parser.restricted = true; return 'WAITTHREAD'
exec                            parser.restricted = true; return 'EXEC'
waitexec                        parser.restricted = true; return 'WAITEXEC'
waittill                        parser.restricted = true; return 'WAITTILL'
isalive                         parser.restricted = true; return 'ISALIVE'
"::"                            return '::'
"+="                            return '+='
"-="                            return '-='
"*="                            return '*='
"/="                            return '/='
"%="                            return '%='
'++'                            parser.restricted = false; return '++'
'--'                            parser.restricted = false; return '--'
"&&"                            return '&&'
"||"                            return '||'
"^"                             return '^'
"=="                            return '=='
"!="                            return '!='
"<="                            return '<='
">="                            return '>='
"<"                             return '<'
">"                             return '>'
"&"                             return '&'
"|"                             return '|'
"+"                             return '+'
"-"                             return '-'
"*"                             return '*'
"/"                             return '/'
"%"                             return '%'
"$"                             return '$'
"."                             return '.'
"["                             parser.restricted = false; return '['
"]"                             return ']'
"("                             parser.restricted = false; return '('
")"                             return ')'
"{"                             parser.restricted = false; return '{'
"}"                             return '}'
":"                             return ':'
";"                             parser.restricted = false; return ';'
","                             return ','
"="                             return '='
"!"                             parser.restricted = false; return '!'
"~"                             parser.restricted = false; return '~'
{ID}                            /*parser.restricted = false;*/ return 'IDENTIFIER'
{INTEGER}                       /*parser.restricted = false;*/ return 'INTEGER'
{FLOAT}                         /*parser.restricted = false;*/ return 'FLOAT'
<<EOF>>                         return 'EOF'
.                               return 'ERROR'

%%

/* Begin Lexer Customization Methods */
var _originalLexMethod = lexer.lex;

lexer.lex = function() {
    parser.wasNewLine = parser.newLine;
    parser.newLine = false;

    return _originalLexMethod.call(this);
};
/* End Lexer Customization Methods */
