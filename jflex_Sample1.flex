   
/* --------------------------Usercode Section------------------------ */
   
import java_cup.runtime.*;
      
%%
   
/* -----------------Options and Declarations Section----------------- */
   
/* 
   The name of the class JFlex will create will be Lexer.
   Will write the code to the file Lexer.java. 
*/
%class Lexer

/*
  The current line number can be accessed with the variable yyline
  and the current column number with the variable yycolumn.
*/
%line
%column
    
/* 
   Will switch to a CUP compatibility mode to interface with a CUP
   generated parser.
*/
%cup
   
/*
 			  Declarations
   
*/
%{   
    /* To create a new java_cup.runtime.Symbol with information about
       the current token, the token will have no value in this
       case. */
    private Symbol symbol(int type) {
        return new Symbol(type, yyline, yycolumn);
    }
    
    /* Also creates a new java_cup.runtime.Symbol with information
       about the current token, but this object has a value. */
    private Symbol symbol(int type, Object value) {
        return new Symbol(type, yyline, yycolumn, value);
    }
%}
   

/*
  Macro Declarations
  
  These declarations are regular expressions that will be used latter
  in the Lexical Rules Section.  
*/

D = [0-9]
L = [a-zA-Z_]
H = [a-fA-F0-9]
E = [Ee][+-]?{D}+
FS = (f|F|l|L)
IS = (u|U|l|L)*

%%
/* ------------------------Lexical Rules Section---------------------- */
   
/*
   This section contains regular expressions and actions, i.e. Java
   code, that will be executed when the scanner matches the associated
   regular expression. */
   
   /* YYINITIAL is the state at which the lexer begins scanning.  So
   these regular expressions will only be matched if the scanner is in
   the start state YYINITIAL. */
   
<YYINITIAL> {
    
"/*"			{ return symbol(COMMENT); }

"auto"			{  return symbol(AUTO); }
"break"			{  return symbol(BREAK); }
"case"			{  return symbol(CASE); }
"char"			{  return symbol(CHAR); }
"const"			{  return symbol(CONST); }
"continue"		{  return symbol(CONTINUE); }
"default"		{  return symbol(DEFAULT); }
"do"			{  return symbol(DO); }
"double"		{  return symbol(DOUBLE); }
"else"			{  return symbol(ELSE); }
"enum"			{  return symbol(ENUM); }
"extern"		{  return symbol(EXTERN); }
"float"			{  return symbol(FLOAT); }
"for"			{  return symbol(FOR); }
"goto"			{  return symbol(GOTO); }
"if"			{  return symbol(IF); }
"int"			{  return symbol(INT); }
"long"			{  return symbol(LONG); }
"register"		{  return symbol(REGISTER); }
"return"		{  return symbol(RETURN); }
"short"			{  return symbol(SHORT); }
"signed"		{  return symbol(SIGNED); }
"sizeof"		{  return symbol(SIZEOF); }
"static"		{  return symbol(STATIC); }
"struct"		{  return symbol(STRUCT); }
"switch"		{  return symbol(SWITCH); }
"typedef"		{  return symbol(TYPEDEF); }
"union"			{  return symbol(UNION); }
"unsigned"		{  return symbol(UNSIGNED); }
"void"			{  return symbol(VOID); }
"volatile"		{  return symbol(VOLATILE); }
"while"			{  return symbol(WHILE); }


0[xX]{H}+{IS}?		{  return symbol(CONSTANT); }
0{D}+{IS}?		{  return symbol(CONSTANT); }
{D}+{IS}?		{  return symbol(CONSTANT); }
L?'(\\.|[^\\'])+'	{  return symbol(CONSTANT); }

{D}+{E}{FS}?		{  return symbol(CONSTANT); }
{D}*"."{D}+({E})?{FS}?	{  return symbol(CONSTANT); }
{D}+"."{D}*({E})?{FS}?	{  return symbol(CONSTANT); }

"..."			{  return symbol(ELLIPSIS); }
">>="			{  return symbol(RIGHT_ASSIGN); }
"<<="			{  return symbol(LEFT_ASSIGN); }
"+="			{  return symbol(ADD_ASSIGN); }
"-="			{  return symbol(SUB_ASSIGN); }
"*="			{  return symbol(MUL_ASSIGN); }
"/="			{  return symbol(DIV_ASSIGN); }
"%="			{  return symbol(MOD_ASSIGN); }
"&="			{  return symbol(AND_ASSIGN); }
"^="			{  return symbol(XOR_ASSIGN); }
"|="			{  return symbol(OR_ASSIGN); }
">>"			{  return symbol(RIGHT_OP); }
"<<"			{  return symbol(LEFT_OP); }
"++"			{  return symbol(INC_OP); }
"--"			{  return symbol(DEC_OP); }
"->"			{  return symbol(PTR_OP); }
"&&"			{  return symbol(AND_OP); }
"||"			{  return symbol(OR_OP); }
"<="			{  return symbol(LE_OP); }
">="			{  return symbol(GE_OP); }
"=="			{  return symbol(EQ_OP); }
"!="			{  return symbol(NE_OP); }
";"			{  return symbol(';'); }
("{"|"<%")		{  return symbol('{'); }
("}"|"%>")		{  return symbol('}'); }
","			{  return symbol(','); }
":"			{  return symbol(':'); }
"="			{  return symbol('='); }
"("			{  return symbol('('); }
")"			{  return symbol(')'); }
("["|"<:")		{  return symbol('['); }
("]"|":>")		{  return symbol(']'); }
"."			{  return symbol('.'); }
"&"			{  return symbol('&'); }
"!"			{  return symbol('!'); }
"~"			{  return symbol('~'); }
"-"			{  return symbol('-'); }
"+"			{  return symbol('+'); }
"*"			{  return symbol('*'); }
"/"			{  return symbol('/'); }
"%"			{  return symbol('%'); }
"<"			{  return symbol('<'); }
">"			{  return symbol('>'); }
"^"			{  return symbol('^'); }
"|"			{  return symbol('|'); }
"?"			{  return symbol('?'); }

[ \t\v\n\f]		{  }
.			{ /* ignore bad characters */ }
}

.|\n                             { throw new RuntimeException("Illegal character \""+yytext()+
                                                              "\" at line "+yyline+", column "+yycolumn); }


# public static void main(String[] args) {
# 	// the code to use a file and get the symbols
# 	int ID;
# 	File file = new File("E:\\Facultate Anul 4\\Proiectoarea translatoarelor\Labs\\CStandardAnalyzer.git\\Input.txt");
#	BufferedReader br = new BufferedReader(new FileReader(file));
#	String st; 
#	while ((st = br.readLine()) != null)
#	{
#}
#}