module Compile

import Prelude;
import Abstract;
import Load;
import ToJava;
import StringTemplate;

alias Instrs = list[Instr];                       

// Compile Expressions.

str compileExp(natCon(int N)) = "<N>";  

str compileExp(strCon(str S)) = S;

str compileExp(id(JGId Id)) = "<Id>";

public Instrs compileExp(add(EXP E1, EXP E2)) =    
  [*compileExp(E1), *compileExp(E2), add2()];

  
// Unique label generation

private int nLabel = 0;                            

private str nextLabel() {
  nLabel += 1;
  return "L<nLabel>";
}

// Compile a statement

str compileStat(asgStat(JGId Id, EXP Exp)) =
	compileExp(Id)+ "=" + compileExp(Exp);
	
str compileStat(ifElseStat(EXP Exp,              
                              list[STATEMENT] Stats1,
                              list[STATEMENT] Stats2)){
  
  elseLab = nextLabel();
  endLab = nextLabel();  
  return "if ("+ Exp +") {"+ Stats1+"} else{"+Stats2+"}";
}

Instrs compileStat(whileStat(EXP Exp, 
                             list[STATEMENT] Stats1)) {
  entryLab = nextLabel();
  endLab = nextLabel();
  return [label(entryLab), 
          *compileExp(Exp), 
          gofalse(endLab), 
          *compileStats(Stats1), 
          go(entryLab), 
          label(endLab)];
}

// Compile a list of statements
str compileStats(list[STATEMENT] Stats1) {
	str result = "" ;
	  for(s<-Stats1){
	  	result+="<s>";
	  };
	  return result;
}   
	
  
// Compile declarations

Instrs compileDecls(list[DECL] Decls) =
  [ ((tp == dec()) ? Double (Id) : String (Id))  |       
    decl(JGId Id, TYPE tp) <- Decls
  ];

// Compile a JG program

public str compileProgram(PROGRAM P){
  nLabel = 0;
  if(program(list[DECL] Decls, list[STATEMENT] Series) := P){
  return
    "public class test {
    '  <for (x <- sort([f | f <- compileDecls(Decls)])) {>
    '  private <x>;
       public void main(){
       	<compileStats(Series)>
       }
  <}>
    '}";
  } else
    throw "Cannot happen";
}

public str compileProgram(str txt) = compileProgram(load(txt));