module Syntax

import Prelude;

lexical Id  = ([a-z][a-z0-9]* !>> [a-z0-9]) \ JGKeywords;
lexical Int = [0-9]+ 
            | "-" [0-9]+ ;
            
lexical String = "\"" ![\"]*  "\"";

lexical Float = [1-9][0-9]+ "." [0-9]+ 
              | "-"[1-9][0-9]+ "." [0-9]+
              | [0-9] "." [0-9]+
              | "-"[0-9] "." [0-9]+;

lexical Boolean = [a-zA-Z0-9] !<< "TRUE" !>> [a-zA-Z0-9]
                | [a-zA-Z0-9] !<< "FALSE" !>> [a-zA-Z0-9];
                
lexical Matrix = ("["Float (","Float)* "]")+;

lexical Vector2d = "["Float "," Float "]";

lexical Vector3d = "["Float "," Float "," Float"]";

keyword JGKeywords = "begin" | "end" | 
                       "declare" | 
                       "if" | "then" | "else" | "fi" | 
                       "while" | "do" | "od" | "for" | "in"
                       ;

layout Layout = WhitespaceAndComment* !>> [\ \t\n\r%];

lexical WhitespaceAndComment 
   = [\ \t\n\r]
   | @category="Comment" "/" ![%]+ "/"
   | @category="Comment" "//" ![\n]* $
   ;

start syntax Program 
   = program: "begin" Declarations decls {Statement  ";"}* body "end" ;

syntax Declarations 
   = "declare" {Declaration ","}* decls ";" ;  
 
syntax Declaration = decl: Type tp ":" Id id;


syntax Type 
   = Int:"int" 
   | String:"string" 
   | Boolean:"boolean"
   | Float:"float"
   | Matrix:"Matrix"
   | Vector2d:"Vector2d"
   | Vector3d:"Vector3d"
   ;

syntax Statement 
   = asgStat: Id var ":="  Expression val 
   | ifElseStat: "if" Expression cond "then" {Statement ";"}*  thenPart "else" {Statement ";"}* elsePart "fi"
   | whileStat: "while" Expression cond "do" {Statement ";"}* body "od"
   | forStat: "for" Id var "in" Expression cond "do" {Statement ";"}* forbody "od"
  ;  
     
syntax Expression 
   = id: Id name
   | matrix: Matrix name
   | vector2d: Vector2d v2
   | vector3d: Vector3d v3 
   | strCon: String string
   | intCon: Int int
   | bolCon: Boolean bool
   | floatCon: Float float
   | bracket "(" Expression e ")"
   > not: "not" Expression rhs
   > invers: "inv" Expression rhs
   | transpose: "tran" Expression rhs 
   > left dot: Expression lhs "dot" Expression rhs
   > left (coma: Expression lhs "==" Expression rhs
          | comb: Expression lhs "!=" Expression rhs)
   > left ( ands: Expression lhs "&&"  Expression rhs
          | andc: Expression lhs "and"  Expression rhs
          | ors: Expression lhs "||" Expression rhs
          | orc: Expression lhs "or" Expression rhs
          )
   > left ( mul: Expression lhs "*" Expression rhs
          | div: Expression lhs "/" Expression rhs
          )
   > left ( add: Expression lhs "+" Expression rhs
          | sub: Expression lhs "-" Expression rhs
          )
   ;

public start[Program] program(str s) {
  return parse(#start[Program], s);
}

public start[Program] program(str s, loc l) {
  return parse(#start[Program], s, l);
}