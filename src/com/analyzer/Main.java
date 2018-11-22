package com.analyzer;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;

import java_cup.runtime.Symbol;


public class Main {
    static public void main(String args[]) throws FileNotFoundException, UnsupportedEncodingException {

        Lexer lex = null;
//        PrintWriter writer = new PrintWriter("output.txt", "UTF-8");
        try {
            lex = new Lexer(new FileReader("src/input.txt"));
        } catch (Exception e) {
            e.printStackTrace();
        }
        Symbol symbol = null;
        do {
            try {
                symbol = lex.next_token();
//                writer.println("We found the token : "+ lex.yytext() + ": with the ID: " + symbol);
                System.out.println("We found : "+ lex.yytext() + " : with the ID: " + symbol);
            } catch (Exception e) {
                e.printStackTrace();
//                writer.println(e);
            }
        } while (symbol.sym != 0);
//        writer.close();
        
//        try {
//            parser p = new parser(new Lexer(new FileReader("src/input.txt")));
//            Object result = p.parse().value;
//       
//          } catch (Exception e) {
//        	  e.printStackTrace();
//            } finally {
//              /* do close out here */
//              }
//        } 
    }
}