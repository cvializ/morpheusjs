{
  var TYPES_TO_PROPERTY_NAMES = {
    CallExpression:   "callee",
    MemberExpression: "object",
  };

  function filledArray(count, value) {
    var result = new Array(count), i;

    for (i = 0; i < count; i++) {
      result[i] = value;
    }

    return result;
  }

  function extractOptional(optional, index) {
    return optional ? optional[index] : null;
  }

  function extractList(list, index) {
    var result = new Array(list.length), i;

    for (i = 0; i < list.length; i++) {
      result[i] = list[i][index];
    }

    return result;
  }

  function buildList(first, rest, index) {
    return [first].concat(extractList(rest, index));
  }

  function buildTree(first, rest, builder) {
    var result = first, i;

    for (i = 0; i < rest.length; i++) {
      result = builder(result, rest[i]);
    }

    return result;
  }

  function buildBinaryExpression(first, rest) {
    return buildTree(first, rest, function(result, element) {
      return {
        type:     "BinaryExpression",
        operator: element[1],
        left:     result,
        right:    element[3]
      };
    });
  }

  function buildLogicalExpression(first, rest) {
    return buildTree(first, rest, function(result, element) {
      return {
        type:     "LogicalExpression",
        operator: element[1],
        left:     result,
        right:    element[3]
      };
    });
  }

  function optionalList(value) {
    return value !== null ? value : [];
  }
}

Start
  = __ program:Program __ { return program; }

Program
  = body:SourceElements? {
      return {
        type: "Program",
        body: optionalList(body)
      };
    }

SourceElements
  = first:SourceElement rest:(__ SourceElement)* {
      return buildList(first, rest, 1);
    }

SourceElement
  = Statement

Statement
    = Block
    / ExpressionStatement
    / AssignmentEventStatement
    / IterationStatement
    / IfStatement
    / ContinueStatement
    / BreakStatement
    / ReturnStatement
    / LabeledStatement
    / EmptyStatement

Block
  = "{" __ body:(StatementList __)? "}" {
      return {
        type: "BlockStatement",
        body: optionalList(extractOptional(body, 0))
      };
    }

StatementList
  = first:Statement rest:(__ Statement)* { return buildList(first, rest, 1); }

ExpressionStatement
    = expression:Expression EOS
        {
            return { type: 'ExpressionStatement', expression: expression };
        }

IfStatement
    = IfToken __ '(' __ test:Expression __ ')' __ consequent:Statement __ ElseToken __ alternate:Statement
        {
            return { type: 'IfStatement', test: test, consequent: consequent, alternate: alternate };
        }
    / IfToken __ '(' __ test:Expression __ ')' __ consequent:Statement
        {
            return { type: 'IfStatement', test: test, consequent: consequent, alternate: null };
        }

IterationStatement
    = WhileToken __ '(' __ test:Expression __ ')' __ body:Statement
        {
            return { type: 'WhileStatement', test: test, body: body };
        }
    / ForToken __ '(' __ init:Expression __ ';' __ test:Expression __ ';' __ update:Expression* __ ')' __ body:Statement
        {
            return {
              type: 'ForStatement',
              init: init, //extractOptional(init, 0),
              test: test, //extractOptional(test, 0),
              update: update, //extractOptional(update, 0),
              body: body
            };
        }

ContinueStatement
    = ContinueToken EOS
        {
            return { type: 'ContinueStatement', label: null };
        }
    / ContinueToken _ label:Identifier _ EOS
        {
            return { type: 'ContinueStatement', label: label };
        }

BreakStatement
    = BreakToken EOS
        {
            return { type: 'BreakStatement', label: null };
        }
    / BreakToken _ Identifier EOS
        {
            return { type: 'BreakStatement', label: label };
        }


ReturnStatement
    = EndToken EOS
        {
            return { type: 'EndStatement', argument: null };
        }
    / EndToken _ argument:Expression EOS
        {
            return { type: 'EndStatement', argument: argument };
        }

LabeledStatement
    = label:Identifier __ ':'
        {
            return { type: 'LabeledStatement', arguments: [] };
        }
    / label:Identifier args:(_ MemberExpression)+ _ ':'
        {
            return { type: 'LabeledStatement', arguments: args };
        }

EmptyStatement
    = ';'
        {
            return { type: 'EmptyStatement' };
        }

AssignmentEventStatement
    = assignee:MemberExpression _ '=' _ EventStatement
    / EventStatement

EventStatement
    = owner:MemberExpression _ method:Identifier args:(' '+ MemberExpression)+ EOS
        {
            return { type: "EventStatement", owner: owner, method: method, arguments: args };
        }
    / method:Identifier args:( ' '+ MemberExpression)+ EOS
        {
            return { type: "EventStatement", owner: null, method: method, arguments: args };
        }

Expression
  = first:AssignmentExpression rest:(__ "," __ AssignmentExpression)* {
      return rest.length > 0
        ? { type: "SequenceExpression", expressions: buildList(first, rest, 3) }
        : first;
    }

PrimaryExpression
  = builtin:$(BuiltinVariables) { return { type: "ThisExpression", value: builtin }; }
  / Identifier
  / Literal
  / "(" __ expression:Expression __ ")" { return expression; }

BuiltinVariables
  = SelfToken
  / LocalToken
  / LevelToken
  / GameToken
  / ParmToken
  / GroupToken

MemberExpression
  = first:(
        PrimaryExpression
    )
    rest:(
        __ "[" __ property:Expression __ "]" {
          return { property: property, computed: true };
        }
      / __ "." __ property:Identifier {
          return { property: property, computed: false };
        }
    )*
    {
      return buildTree(first, rest, function(result, element) {
        return {
          type:     "MemberExpression",
          object:   result,
          property: element.property,
          computed: element.computed
        };
      });
    }

NewExpression
  = MemberExpression

CallExpression
  = first:(
      callee:MemberExpression __ args:Arguments {
        return { type: "CallExpression", callee: callee, arguments: args };
      }
    )
    rest:(
        __ args:Arguments {
          return { type: "CallExpression", arguments: args };
        }
      / __ "[" __ property:Expression __ "]" {
          return {
            type:     "MemberExpression",
            property: property,
            computed: true
          };
        }
      / __ "." __ property:Identifier {
          return {
            type:     "MemberExpression",
            property: property,
            computed: false
          };
        }
    )*
    {
      return buildTree(first, rest, function(result, element) {
        element[TYPES_TO_PROPERTY_NAMES[element.type]] = result;

        return element;
      });
    }

Arguments
  = "(" __ args:(ArgumentList __)? ")" {
      return optionalList(extractOptional(args, 0));
    }

ArgumentList
  = first:AssignmentExpression rest:(__ "," __ AssignmentExpression)* {
      return buildList(first, rest, 3);
    }

LeftHandSideExpression
  = CallExpression
  / NewExpression

PostfixExpression
  = argument:LeftHandSideExpression _ operator:PostfixOperator {
      return {
        type:     "UpdateExpression",
        operator: operator,
        argument: argument,
        prefix:   false
      };
    }
  / LeftHandSideExpression

PostfixOperator
  = "++"
  / "--"

UnaryExpression
  = PostfixExpression
  / operator:UnaryOperator __ argument:UnaryExpression {
      var type = (operator === "++" || operator === "--")
        ? "UpdateExpression"
        : "UnaryExpression";

      return {
        type:     type,
        operator: operator,
        argument: argument,
        prefix:   true
      };
    }

UnaryOperator
  = "++"
  / "--"
  / $("+" !"=")
  / $("-" !"=")
  / "~"
  / "!"

MultiplicativeExpression
  = first:UnaryExpression
    rest:(__ MultiplicativeOperator __ UnaryExpression)*
    { return buildBinaryExpression(first, rest); }

MultiplicativeOperator
  = $("*" !"=")
  / $("/" !"=")
  / $("%" !"=")

AdditiveExpression
  = first:MultiplicativeExpression
    rest:(__ AdditiveOperator __ MultiplicativeExpression)*
    { return buildBinaryExpression(first, rest); }

AdditiveOperator
  = $("+" ![+=])
  / $("-" ![-=])

ShiftExpression
  = first:AdditiveExpression
    rest:(__ ShiftOperator __ AdditiveExpression)*
    { return buildBinaryExpression(first, rest); }

ShiftOperator
  = $("<<"  !"=")
  / $(">>>" !"=")
  / $(">>"  !"=")

RelationalExpression
  = first:ShiftExpression
    rest:(__ RelationalOperator __ ShiftExpression)*
    { return buildBinaryExpression(first, rest); }

RelationalOperator
  = "<="
  / ">="
  / $("<" !"<")
  / $(">" !">")

EqualityExpression
  = first:RelationalExpression
    rest:(__ EqualityOperator __ RelationalExpression)*
    { return buildBinaryExpression(first, rest); }

EqualityOperator
  = "==="
  / "!=="
  / "=="
  / "!="

BitwiseANDExpression
  = first:EqualityExpression
    rest:(__ BitwiseANDOperator __ EqualityExpression)*
    { return buildBinaryExpression(first, rest); }

BitwiseANDOperator
  = $("&" ![&=])

BitwiseXORExpression
  = first:BitwiseANDExpression
    rest:(__ BitwiseXOROperator __ BitwiseANDExpression)*
    { return buildBinaryExpression(first, rest); }

BitwiseXOROperator
  = $("^" !"=")

BitwiseORExpression
  = first:BitwiseXORExpression
    rest:(__ BitwiseOROperator __ BitwiseXORExpression)*
    { return buildBinaryExpression(first, rest); }

BitwiseOROperator
  = $("|" ![|=])

LogicalANDExpression
  = first:BitwiseORExpression
    rest:(__ LogicalANDOperator __ BitwiseORExpression)*
    { return buildBinaryExpression(first, rest); }

LogicalANDOperator
  = "&&"

LogicalORExpression
  = first:LogicalANDExpression
    rest:(__ LogicalOROperator __ LogicalANDExpression)*
    { return buildBinaryExpression(first, rest); }

LogicalOROperator
  = "||"

ConditionalExpression
  = test:LogicalORExpression __
    "?" __ consequent:AssignmentExpression __
    ":" __ alternate:AssignmentExpression
    {
      return {
        type:       "ConditionalExpression",
        test:       test,
        consequent: consequent,
        alternate:  alternate
      };
    }
  / LogicalORExpression

AssignmentExpression
  = left:LeftHandSideExpression __
    "=" !"=" __
    right:AssignmentExpression
    {
      return {
        type:     "AssignmentExpression",
        operator: "=",
        left:     left,
        right:    right
      };
    }
  / left:LeftHandSideExpression __
    operator:AssignmentOperator __
    right:AssignmentExpression
    {
      return {
        type:     "AssignmentExpression",
        operator: operator,
        left:     left,
        right:    right
      };
    }
  / ConditionalExpression

AssignmentOperator
  = "*="
  / "/="
  / "%="
  / "+="
  / "-="
  / "<<="
  / ">>="
  / ">>>="
  / "&="
  / "^="
  / "|="

Identifier
  = !ReservedWord name:IdentifierName { return name; }

IdentifierName "identifier"
  = first:IdentifierStart rest:IdentifierPart* {
      return {
        type: "Identifier",
        name: first + rest.join("")
      };
    }

IdentifierStart
  = [A-Za-z]
  / "$"
  / "_"

IdentifierPart
  = IdentifierStart
  / [0-9]

NumericLiteral
    = '-'? ([0-9]+) ('.' [0-9]+)?
        {
            return { type: 'Literal', value: parseFloat(text(), 10) };
        }

StringLiteral "string"
  = '"' chars:DoubleStringCharacter* '"' {
      return { type: "Literal", value: chars.join("") };
    }
  / "'" chars:SingleStringCharacter* "'" {
      return { type: "Literal", value: chars.join("") };
    }

DoubleStringCharacter
  = !('"' / "\\" / LineTerminator) SourceCharacter { return text(); }
  / "\\" sequence:EscapeSequence { return sequence; }
  / LineContinuation

SingleStringCharacter
  = !("'" / "\\" / LineTerminator) SourceCharacter { return text(); }
  / "\\" sequence:EscapeSequence { return sequence; }
  / LineContinuation

LineContinuation
  = "\\" LineTerminatorSequence { return ""; }

EscapeSequence
  = CharacterEscapeSequence
  /* / "0" !DecimalDigit { return "\0"; } */

CharacterEscapeSequence
  = SingleEscapeCharacter
  / NonEscapeCharacter

SingleEscapeCharacter
  = "'"
  / '"'
  / "\\"
  / "b" { return "\b"; }
  / "f" { return "\f"; }
  / "n" { return "\n"; }
  / "r" { return "\r"; }
  / "t" { return "\t"; }
  / "v" { return "\v"; }

NonEscapeCharacter
  = !(EscapeCharacter / LineTerminator) SourceCharacter { return text(); }

EscapeCharacter
  = SingleEscapeCharacter
  /* / DecimalDigit */

VectorLiteral
    = '(' _ one:NumericLiteral __ two:NumericLiteral __ three:NumericLiteral _ ')'
        {
            return { type: 'Literal', value: [ one, two, three ] };
        }

NullLiteral
    = NullToken { return null; }

NilLiteral
    = NilToken { return undefined; }

Literal
    = NumericLiteral
    / StringLiteral
    / VectorLiteral
    / NullToken
    / NilToken

Keyword
    = BreakToken
    / CaseToken
    / CatchToken
    / ContinueToken
    / DefaultToken
    / ElseToken
    / ForToken
    / IfToken
    / NullToken
    / NilToken
    / EndToken
    / SwitchToken
    / SelfToken
    / LocalToken
    / LevelToken
    / GameToken
    / ParmToken
    / SelfToken
    / GroupToken
    / ThrowToken
    / TryToken
    / WhileToken

ReservedWord
  = Keyword
  / NullLiteral
  / NilLiteral

/* Tokens */

BreakToken      = "break"      !Identifier
CaseToken       = "case"       !Identifier
CatchToken      = "catch"      !Identifier
ContinueToken   = "continue"   !Identifier
DefaultToken    = "default"    !Identifier
ElseToken       = "else"       !Identifier
ForToken        = "for"        !Identifier
IfToken         = "if"         !Identifier
NullToken       = "null"       !Identifier
NilToken        = "nil"        !Identifier
EndToken        = "end"        !Identifier
SwitchToken     = "switch"     !Identifier
SelfToken       = "self"       !Identifier
LocalToken      = "local"      !Identifier
LevelToken      = "level"      !Identifier
GameToken       = "game"       !Identifier
ParmToken       = "parm"       !Identifier
SelfToken       = "self"       !Identifier
GroupToken      = "group"      !Identifier
ThrowToken      = "throw"      !Identifier
TryToken        = "try"        !Identifier
WhileToken      = "while"      !Identifier

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
