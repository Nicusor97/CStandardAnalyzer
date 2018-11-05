package com.analizor;
import java.io.FileReader;


import java_cup.runtime.Symbol;




public class Main {
    static public void main(String args[]) {

        Lexer lex = null;
        try {
            lex = new Lexer(new FileReader("src/main/resources/input.txt"));
        } catch (Exception e) {
            e.printStackTrace();
        }
        Symbol symbol = null;
        do {
            try {
                symbol = lex.next_token();
                System.out.println(symbol);
            } catch (Exception e) {
                e.printStackTrace();
            }


        } while (symbol.sym != 0);
    }
}
