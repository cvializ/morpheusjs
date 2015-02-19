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

\s+       /* ignore */
{INTEGER} return 'INTEGER'
{FLOAT}   return 'FLOAT'
{QUOTE}.*{QUOTE} return 'STRING';
{DQUOTE}.*{DQUOTE} return 'STRING';
if        return 'IF'
end       return 'END'
else      return 'ELSE'
while     return 'WHILE'
for       return 'FOR'
continue  return 'CONTINUE'
case      return 'CASE'
break     return 'BREAK'
switch    return 'SWITCH'
try       return 'TRY'
catch     return 'CATCH'

local     return 'LOCAL'
level     return 'LEVEL'
game     return 'GAME'
parm     return 'PARM'
self     return 'SELF'
group     return 'GROUP'

NULL      return 'NULL'
NIL       return 'NIL'

{ID}      return 'IDENTIFIER'

"::"      return '::'

"+="      return '+='
"-="      return '-='
"*="      return '*='
"/="      return '/='
"%="      return '%='

'++'      return '++'
'--'      return '--'

"&&"      return '&&'
"||"      return '||'
"^"       return '^'
"=="      return '=='
"!="      return '!='
"<="      return '<='
">="      return '>='
"<"       return '<'
">"       return '>'

"&"       return '&'
"|"       return '|'
"+"       return '+'
"-"       return '-'
"*"       return '*'
"/"       return '/'
"%"       return '%'

"$"       return '$'
"."       return '.'
"["       return '['
"]"       return ']'
"("       return '('
")"       return ')'
"{"       return '{'
"}"       return '}'
":"       return ':'
";"       return ';'
","       return ','

"="       return '='
"!"       return '!'
"~"       return '~'

<<EOF>>   return 'EOF'
.         return 'INVALID'

%%
