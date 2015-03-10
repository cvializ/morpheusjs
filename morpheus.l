%x COMMENT

ALPHA       [$A-Za-z_]
NATURAL     ([1-9][0-9]+|[0-9])
INTEGER     \-?{NATURAL}+
FLOAT       {INTEGER}+\.{NATURAL}+
ID          {ALPHA}({ALPHA}|[0-9])*
QUOTE       "'"
DQUOTE      "\""

LineContinuation \\(\r\n|\r|\n)
SingleEscapeCharacter [\'\"\\bfnrtv]
NonEscapeCharacter [^\'\"\\bfnrtv0-9xu]
CharacterEscapeSequence {SingleEscapeCharacter}|{NonEscapeCharacter}
EscapeSequence {CharacterEscapeSequence}
DoubleStringCharacter ([^\"\\\n\r]+)|(\\{EscapeSequence})|{LineContinuation}
SingleStringCharacter ([^\'\\\n\r]+)|(\\{EscapeSequence})|{LineContinuation}
StringLiteral (\"{DoubleStringCharacter}*\")|(\'{SingleStringCharacter}*\')

%options flex

%%

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

{StringLiteral}               parser.restricted = false; return 'STRING';
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
// Listener class
cancelFor       return 'CANCELFOR'
classname       return 'CLASSNAME'
commanddelay        return 'COMMANDDELAY'
delaythrow      return 'DELAYTHROW'
delete      return 'DELETE'
exec        return 'EXEC'
immediateremove     return 'IMMEDIATEREMOVE'
owner       return 'OWNER'
remove      return 'REMOVE'
throw       return 'THROW'
waitexec        return 'WAITEXEC'
unregister      return 'UNREGISTER'

abs     return 'ABS'
addobjective        return 'ADDOBJECTIVE'
//aliascache      return 'ALIASCACHE'
all_ai_off      return 'ALL_AI_OFF'
all_ai_on       return 'ALL_AI_ON'
angles_pointat      return 'ANGLES_POINTAT'
angles_toforward        return 'ANGLES_TOFORWARD'
angles_toleft       return 'ANGLES_TOLEFT'
angles_toup     return 'ANGLES_TOUP'
assert      return 'ASSERT'
bool        return 'BOOL'
bsptransition       return 'BSPTRANSITION'
cache       return 'CACHE'
cam     return 'CAM'
centerprint     return 'CENTERPRINT'
cinematic       return 'CINEMATIC'
clear_objective_pos     return 'CLEAR_OBJECTIVE_POS'
clearfade       return 'CLEARFADE'
clearletterbox      return 'CLEARLETTERBOX'
CreateListener      return 'CREATELISTENER'
cuecamera       return 'CUECAMERA'
cueplayer       return 'CUEPLAYER'
delaythrow      return 'DELAYTHROW'
drawhud     return 'DRAWHUD'
earthquake      return 'EARTHQUAKE'
end     return 'END'
entity      return 'ENTITY'
error       return 'ERROR'
fadein      return 'FADEIN'
fadeout     return 'FADEOUT'
fadesound       return 'FADESOUND'
float       return 'FLOAT_CMD'
forcemusic      return 'FORCEMUSIC'
freezeplayer        return 'FREEZEPLAYER'
getboundkey1        return 'GETBOUNDKEY1'
getboundkey2        return 'GETBOUNDKEY2'
getcvar     return 'GETCVAR'
goto        return 'GOTO'
hidemenu        return 'HIDEMENU'
hidemouse       return 'HIDEMOUSE'
huddraw_alpha       return 'HUDDRAW_ALPHA'
huddraw_align       return 'HUDDRAW_ALIGN'
huddraw_color       return 'HUDDRAW_COLOR'
huddraw_string      return 'HUDDRAW_STRING'
huddraw_font        return 'HUDDRAW_FONT'
huddraw_rect        return 'HUDDRAW_RECT'
huddraw_shader      return 'HUDDRAW_SHADER'
iprintln        return 'IPRINTLN'
huddraw_virtualsize     return 'HUDDRAW_VIRTUALSIZE'
int     return 'INT'
iprintln_noloc      return 'IPRINTLN_NOLOC'
iprintlnbold        return 'IPRINTLNBOLD'
iprintlnbold_noloc      return 'IPRINTLNBOLD_NOLOC'
isalive     return 'ISALIVE'
killclass       return 'KILLCLASS'
killent     return 'KILLENT'
letterbox       return 'LETTERBOX'
leveltransition     return 'LEVELTRANSITION'
loc_convert_string      return 'LOC_CONVERT_STRING'
locprint        return 'LOCPRINT'
map     return 'MAP'
missionfailed       return 'MISSIONFAILED'
missiontransition       return 'MISSIONTRANSITION'
mprint      return 'MPRINT'
mprintln        return 'MPRINTLN'
music       return 'MUSIC'
noncinematic        return 'NONCINEMATIC'
musicvolume     return 'MUSICVOLUME'
pause       return 'PAUSE'
print       return 'PRINT'
popmenu     return 'POPMENU'
print3d     return 'PRINT3D'
pushmenu        return 'PUSHMENU'
println     return 'PRINTLN'
radiusdamage        return 'RADIUSDAMAGE'
randomfloat     return 'RANDOMFLOAT'
randomint       return 'RANDOMINT'
releaseplayer       return 'RELEASEPLAYER'
removeclass     return 'REMOVECLASS'
removeent       return 'REMOVEENT'
restoremusicvolume      return 'RESTOREMUSICVOLUME'
restoresoundtrack       return 'RESTORESOUNDTRACK'
self        return 'SELF'
server      return 'SERVER'
set_objective_pos       return 'SET_OBJECTIVE_POS'
setcurrentobjective     return 'SETCURRENTOBJECTIVE'
setcvar     return 'SETCVAR'
setlightstyle       return 'SETLIGHTSTYLE'
showmenu        return 'SHOWMENU'
sighttrace      return 'SIGHTTRACE'
soundtrack      return 'SOUNDTRACK'
//spawn       return 'SPAWN'
string      return 'STRING_COMMAND'
stuffcmd        return 'STUFFCMD'
teamwin     return 'TEAMWIN'
thread      return 'THREAD'
throw       return 'THROW'
timeout     return 'TIMEOUT'
trigger     return 'TRIGGER'
trace       return 'TRACE'
vector_closer       return 'VECTOR_CLOSER'
vector_add      return 'VECTOR_ADD'
vector_cross        return 'VECTOR_CROSS'
vector_dot      return 'VECTOR_DOT'
vector_length       return 'VECTOR_LENGTH'
vector_normalize        return 'VECTOR_NORMALIZE'
vector_scale        return 'VECTOR_SCALE'
vector_subtract     return 'VECTOR_SUBTRACT'
vector_toangles     return 'VECTOR_TOANGLES'
vector_within       return 'VECTOR_WITHIN'
waitframe       return 'WAITFRAME'
wait        return 'WAIT'
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
