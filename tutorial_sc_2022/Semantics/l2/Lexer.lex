(* 2002-11-08 -- Time-stamp: <2003-01-06 16:54:41 pes20>    -*-SML-*- *)


{
open Lexing Parser     
   
exception LexicalError of string * int * int (* (message, loc1, loc2) *)
    
fun lexerError lexbuf s = 
   raise LexicalError (s, getLexemeStart lexbuf, getLexemeEnd lexbuf);

exception ThisCannotHappen
}


rule token = parse
    [` ` `\t` `\n` `\r` ]   { token lexbuf }  
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "int"       { INT }
  | "bool"      { BOOL }
  | "unit"      { UNIT }
  | "->"        { ARROW }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "+"         { PLUS }
  | ">="        { GTEQT }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | ":="        { ASSIGN }
  | "!"         { DEREF }
  | "skip"      { SKIP }
  | ";"         { SEQ }
  | "while"     { WHILE }
  | "do"        { DO }
  | "fn"        { FN }
  | ":"         { COLON }
  | "=>"        { DOUBLEARROW }
  | "let"       { LET }
  | "val"       { VAL }
  | "in"        { IN }
  | "end"       { END }
  | "rec"       { REC }
  | [`-``~`]? [`0`-`9`]+     { INTEGER( case Int.fromString(getLexeme lexbuf) of SOME x => x | NONE => raise ThisCannotHappen) }
  | [`A`-`Z` `a`-`z`] ( [`A`-`Z` `a`-`z` `0`-`9` `'`] ) *  { IDENT(getLexeme lexbuf) }
  | ","         { COMMA }
  | "="         { EQUALS }
  | _           { lexerError lexbuf "Illegal symbol in input" }
  | eof         { EOF }


;

