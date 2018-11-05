   
/* --------------------------Usercode Section------------------------ */
   
import java_cup.runtime.*;
      
%%

%class Lexer

%line
%column
%implements sym
%cup
   
%{   
	StringBuffer string = new StringBuffer();
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

DIGIT  =  [0-9]
ID     =  [a-zA-Z][a-zA-Z0-9]*

LineTerminator = \r|\n|\r\n

WhiteSpace     = {LineTerminator} | [ \t\f]


     
%%



{DIGIT}+		{return symbol(INT, yytext().trim());}
{DIGIT}+"."{DIGIT}*   {return symbol(FLOAT, yytext().trim());}


{DIGIT}+{ID}*   { throw new Error("Illegal character \""+yytext()+
                                                              "\" at line "+yyline+", column "+yycolumn); }
"([^\r^\n^']|\\0)"  {return symbol(CHAR, yytext().trim());}
\"[^\"]*\"          {return symbol(STRING, yytext().trim());} 

[\r|\n|\r\n]* "/*"			{ return symbol(COMMENT, yytext().trim()); }			
"auto"			{  return symbol(AUTO, yytext().trim()); }
"break"			{  return symbol(BREAK, yytext().trim()); }
"case"			{  return symbol(CASE, yytext().trim()); }
"char"			{  return symbol(CHAR, yytext().trim()); }
"const"			{  return symbol(CONST, yytext().trim()); }
"continue"		{  return symbol(CONTINUE, yytext().trim()); }
"default"		{  return symbol(DEFAULT, yytext().trim()); }
"do"			{  return symbol(DO, yytext().trim()); }
"double"		{  return symbol(DOUBLE, yytext().trim()); }
"else"			{  return symbol(ELSE, yytext().trim()); }
"enum"			{  return symbol(ENUM, yytext().trim()); }
"extern"		{  return symbol(EXTERN, yytext().trim()); }
"float"			{  return symbol(FLOAT, yytext().trim()); }
"for"			{  return symbol(FOR, yytext().trim()); }
"goto"			{  return symbol(GOTO, yytext().trim()); }
"if"			{  return symbol(IF, yytext().trim()); }
"int"			{  return symbol(INT, yytext().trim()); }
"long"			{  return symbol(LONG, yytext().trim()); }
"register"		{  return symbol(REGISTER, yytext().trim()); }
"return"		{  return symbol(RETURN, yytext().trim()); }
"short"			{  return symbol(SHORT, yytext().trim()); }
"signed"		{  return symbol(SIGNED, yytext().trim()); }
"sizeof"		{  return symbol(SIZEOF, yytext().trim()); }
"static"		{  return symbol(STATIC, yytext().trim()); }
"struct"		{  return symbol(STRUCT, yytext().trim()); }
"switch"		{  return symbol(SWITCH, yytext().trim()); }
"typedef"		{  return symbol(TYPEDEF, yytext().trim()); }
"union"			{  return symbol(UNION, yytext().trim()); }
"unsigned"		{  return symbol(UNSIGNED, yytext().trim()); }
"void"			{  return symbol(VOID, yytext().trim()); }
"volatile"		{  return symbol(VOLATILE, yytext().trim()); }
"while"			{  return symbol(WHILE, yytext().trim()); }

"..."			{  return symbol(ELLIPSIS, yytext().trim()); }
">>="			{  return symbol(RIGHT_ASSIGN, yytext().trim()); }
"<<="			{  return symbol(LEFT_ASSIGN, yytext().trim()); }
"+="			{  return symbol(ADD_ASSIGN, yytext().trim()); }
"-="			{  return symbol(SUB_ASSIGN, yytext().trim()); }
"*="			{  return symbol(MUL_ASSIGN, yytext().trim()); }
"/="			{  return symbol(DIV_ASSIGN, yytext().trim()); }
"%="			{  return symbol(MOD_ASSIGN, yytext().trim()); }
"&="			{  return symbol(AND_ASSIGN, yytext().trim()); }
"^="			{  return symbol(XOR_ASSIGN, yytext().trim()); }
"|="			{  return symbol(OR_ASSIGN, yytext().trim()); }
">>"			{  return symbol(RIGHT_OP, yytext().trim()); }
"<<"			{  return symbol(LEFT_OP, yytext().trim()); }
"++"			{  return symbol(INC_OP, yytext().trim()); }
"--"			{  return symbol(DEC_OP, yytext().trim()); }
"->"			{  return symbol(PTR_OP, yytext().trim()); }
"&&"			{  return symbol(AND_OP, yytext().trim()); }
"||"			{  return symbol(OR_OP, yytext().trim()); }
"<="			{  return symbol(LE_OP, yytext().trim()); }
">="			{  return symbol(GE_OP, yytext().trim()); }
"=="			{  return symbol(EQ_OP, yytext().trim()); }
"!="			{  return symbol(NE_OP, yytext().trim()); }
";"			{  return symbol(';', yytext().trim()); }
("{"|"<%")		{  return symbol('{', yytext().trim()); }
("}"|"%>")		{  return symbol('}', yytext().trim()); }
","			{  return symbol(',', yytext().trim()); }
":"			{  return symbol(':', yytext().trim()); }
"="			{  return symbol('=', yytext().trim()); }
"("			{  return symbol('(', yytext().trim()); }
")"			{  return symbol(')', yytext().trim()); }
("["|"<:")		{  return symbol('[', yytext().trim()); }
("]"|":>")		{  return symbol(']', yytext().trim()); }
"."			{  return symbol('.', yytext().trim()); }
"&"			{  return symbol('&', yytext().trim()); }
"!"			{  return symbol('!', yytext().trim()); }
"~"			{  return symbol('~', yytext().trim()); }
"-"			{  return symbol('-', yytext().trim()); }
"+"			{  return symbol('+', yytext().trim()); }
"*"			{  return symbol('*', yytext().trim()); }
"/"			{  return symbol('/', yytext().trim()); }
"%"			{  return symbol('%', yytext().trim()); }
"<"			{  return symbol('<', yytext().trim()); }
">"			{  return symbol('>', yytext().trim()); }
"^"			{  return symbol('^', yytext().trim()); }
"|"			{  return symbol('|', yytext().trim()); }
"?"			{  return symbol('?', yytext().trim()); }
{WhiteSpace}       {  }
.			{ throw new Error("Illegal character \""+yytext()+
                                                              "\" at line "+yyline+", column "+yycolumn); }
