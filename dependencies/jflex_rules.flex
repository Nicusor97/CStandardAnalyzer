import java_cup.runtime.*;
import java.io.*;

%%
%class Lexer

%line
%column

%cup
%implements sym

%{

	private Symbol symbol(int type) {
        return new Symbol(type, yyline, yycolumn);
    }

    private Symbol symbol(int type, Object value) {
        return new Symbol(type, yyline, yycolumn, value);
    }
	
	public String toString() {
          return ("line : " + (this.yyline + 1) + ", column : " + this.yycolumn + ", value :" + this.yytext());
   }

%}


Letter = [a-zA-Z]
Digit = [0-9]
UnderScore = "_"

Identifier = {Letter}({Letter}|{Digit}|{UnderScore})*

DecInteger= 0 | [1-9][0-9]*

HexInteger = 0[xX][0-9A-Fa-f]+

Integer = {DecInteger}|{HexInteger}

/*Double = at least 1 digit ([0-9]+) followed by a period (\.)
 followed by zero or more digits ([0-9]*). The exponent is optional (?).
 Sign of exponent is optional ([\+\-]?). Exponent can be upper or lower
 case ([eE]). One or more digits must follow exponent ([0-9]+)
 */
DoubleConst= [0-9]+\.[0-9]*([eE][\+\-]?[0-9]+)?

// White space can be blank or tabs
WhiteSpace = [ \t]+

// White space can also be new line/end of line
EndOfLine = \r|\n|\r\n

CommentChar = [^\r\n]
SingleLineComment = "//" {CommentChar}* {EndOfLine}?
MultiLineComment = "/*" ~"*/" | "/*" [^"*/"]* "*/"

Comment = {MultiLineComment} | {SingleLineComment}

StringChar = [^\r\n\"\\. ]

%state STRING

%%

<YYINITIAL> {
	^"#".*			{  return symbol(LIBRARY, yytext()); }
	"auto"			{  return symbol(AUTO, yytext()); }
	"break"			{  return symbol(BREAK, yytext()); }
	"case"			{  return symbol(CASE, yytext()); }
	"char"			{  return symbol(CHAR, yytext()); }
	"const"			{  return symbol(CONST, yytext()); }
	"continue"		{  return symbol(CONTINUE, yytext()); }
	"default"		{  return symbol(DEFAULT, yytext()); }
	"do"			{  return symbol(DO, yytext()); }
	"double"		{  return symbol(DOUBLE, yytext()); }
	"else"			{  return symbol(ELSE, yytext()); }
	"enum"			{  return symbol(ENUM, yytext()); }
	"extern"		{  return symbol(EXTERN, yytext()); }
	"float"			{  return symbol(FLOAT, yytext()); }
	"for"			{  return symbol(FOR, yytext()); }
	"goto"			{  return symbol(GOTO, yytext()); }
	"if"			{  return symbol(IF, yytext()); }
	"int"			{  return symbol(INT, yytext()); }
	"long"			{  return symbol(LONG, yytext()); }
	"register"		{  return symbol(REGISTER, yytext()); }
	"return"		{  return symbol(RETURN, yytext()); }
	"short"			{  return symbol(SHORT, yytext()); }
	"signed"		{  return symbol(SIGNED, yytext()); }
	"sizeof"		{  return symbol(SIZEOF, yytext()); }
	"static"		{  return symbol(STATIC, yytext()); }
	"struct"		{  return symbol(STRUCT, yytext()); }
	"switch"		{  return symbol(SWITCH, yytext()); }
	"typedef"		{  return symbol(TYPEDEF, yytext()); }
	"union"			{  return symbol(UNION, yytext()); }
	"unsigned"		{  return symbol(UNSIGNED, yytext()); }
	"void"			{  return symbol(VOID, yytext()); }
	"volatile"		{  return symbol(VOLATILE, yytext()); }
	"while"			{  return symbol(WHILE, yytext()); }
	
	/* IDENTIFIER */
	{Identifier}    { return symbol(IDENTIFIER, yytext()); }
	/* INTEGER CONSTANT*/
    {Integer}        { return symbol(INT_CONST, yytext()); }
	/* DOUBLE CONSTANT */
    {DoubleConst}    { return symbol(DOUBLE_CONST, yytext()); }
	
	/* OPERATORS and PUNCTUATIONS*/
	"..."			{  return symbol(ELLIPSIS, yytext()); }
	">>="			{  return symbol(RIGHT_ASSIGN, yytext()); }
	"<<="			{  return symbol(LEFT_ASSIGN, yytext()); }
	"+="			{  return symbol(ADD_ASSIGN, yytext()); }
	"-="			{  return symbol(SUB_ASSIGN, yytext()); }
	"*="			{  return symbol(MUL_ASSIGN, yytext()); }
	"/="			{  return symbol(DIV_ASSIGN, yytext()); }
	"%="			{  return symbol(MOD_ASSIGN, yytext()); }
	"&="			{  return symbol(AND_ASSIGN, yytext()); }
	"^="			{  return symbol(XOR_ASSIGN, yytext()); }
	"|="			{  return symbol(OR_ASSIGN, yytext()); }
	">>"			{  return symbol(RIGHT_OP, yytext()); }
	"<<"			{  return symbol(LEFT_OP, yytext()); }
	"++"			{  return symbol(INC_OP, yytext()); }
	"--"			{  return symbol(DEC_OP, yytext()); }
	"->"			{  return symbol(PTR_OP, yytext()); }
	"&&"			{  return symbol(AND_OP, yytext()); }
	"||"			{  return symbol(OR_OP, yytext()); }
	"<="			{  return symbol(LE_OP, yytext()); }
	">="			{  return symbol(GE_OP, yytext()); }
	"=="			{  return symbol(EQ_OP, yytext()); }
	"!="			{  return symbol(NE_OP, yytext()); }
	";"			{  return symbol(SEMI, yytext()); }
	("{"|"<%")		{  return symbol(L_BRACE, yytext()); }
	("}"|"%>")		{  return symbol(R_BRACE, yytext()); }
	","			{  return symbol(COMMA, yytext()); }
	":"			{  return symbol(COLON, yytext()); }
	"="			{  return symbol(EQ, yytext()); }
	"("			{  return symbol(LPAREN, yytext()); }
	")"			{  return symbol(LPAREN, yytext()); }
	("["|"<:")		{  return symbol(LBRACK, yytext()); }
	("]"|":>")		{  return symbol(RBRACK, yytext()); }
	"."			{  return symbol(DOT, yytext()); }
	"&"			{  return symbol(AND, yytext()); }
	"!"			{  return symbol(NOT, yytext()); }
	"~"			{  return symbol('~', yytext()); }
	"-"			{  return symbol(MINUS, yytext()); }
	"+"			{  return symbol(PLUS, yytext()); }
	"*"			{  return symbol(TIMES, yytext()); }
	"/"			{  return symbol(DIVIDE, yytext()); }
	"%"			{  return symbol(MOD, yytext()); }
	"<"			{  return symbol(LT, yytext()); }
	">"			{  return symbol(RT, yytext()); }
	"^"			{  return symbol(XOR, yytext()); }
	"|"			{  return symbol(OR, yytext()); }
	"?"			{  return symbol(QUESTION, yytext()); }
	
	/* STRING CONSTANT */
	\"              {yybegin(STRING);}
	
	/* COMMENTS */
	{Comment}       { return symbol(COMMENT, yytext()); }
	{WhiteSpace}    { /* do nothing */}
	\n              { return symbol(NEWLINE, yytext()); }
}

<STRING> {
  \"              { yybegin(YYINITIAL); return symbol(STRING_CONST, yytext()); }
  /* STRING CHARACTERS */
  {StringChar}+   { }
  /* escape sequences */
  "\\b"           { }
  "\\t"           { }
  "\\n"           { }
  "\\f"           { }
  "\\r"           { }
  "\\\""          { }
  "\\'"           { }
  "\\\\"          { }
}

{UnderScore}({Letter}|{Digit}|{UnderScore})+  { throw new RuntimeException("Illegal character \""+yytext()+
                                                              "\" at line "+yyline+", column "+yycolumn);}

{Digit}+{Identifier}*                         {throw new RuntimeException("Illegal character \""+yytext()+
                                                              "\" at line "+yyline+", column "+yycolumn);}
\\.             { throw new RuntimeException("Illegal new line \""+yytext()+"\" in string."); }
{EndOfLine}     {/* do nothing */}
.                                        {throw new RuntimeException("Illegal character \""+yytext()+
                                                              "\" at line "+yyline+", column "+yycolumn);}