module Compile

import Prelude;
import Abstract;
import Load;
import ToJava;
import StringTemplate;

alias Instrs = list[Instr];                       

// Compile Expressions.

str compileExp(natCon(int N)) = "<N>";  

str compileExp(strCon(str S)) = S.name;

str compileExp(id(JGId Id)) = "<Id<id>>";

public Instrs compileExp(add(EXP E1, EXP E2)) =    
  [*compileExp(E1), *compileExp(E2), add2()];

  

// Compile a statement

str compileStat(asgStat(JGId Id, EXP Exp)) =
	compileExp(Id)+ "=" + compileExp(Exp);
	
str compileStat(ifElseStat(EXP Exp,              
                              list[STATEMENT] Stats1,
                              list[STATEMENT] Stats2)){
  
  return "if (<compileExp(Exp)>) {<compileStates(Stats1)>} else{<compileStates(Stats2)>}";
}


// Compile a list of statements
str compileStats(list[STATEMENT] Stats1) {
	str result = "" ;
	  for(s<-Stats1){
	  	result+="<s.name>";
	  	result+= " = ";
	  	result+="<s.exp>";
	  };
	  return result;
}   
	
  
// Compile declarations

str compileDecls(list[DECL] Decls) {
str java= "";
	
	for(decl <- Decls){
		java += "private "; 
		java += "<decl.tp> ";
		java += " ";
		//Java += " <decl.name>";
	}
	return java;
}


// Compile a JG program

public str compileProgram(PROGRAM P){
  nLabel = 0;
  if(program(list[DECL] Decls, list[STATEMENT] Series) := P){
  return
    "public class test {
    	<compileDecls(Decls)>
       
       public void main(){
       	<compileStats(Series)>
       }
  
    '}";
  } else
    throw "Cannot happen";
}

public str compileProgram(str txt) = compileProgram(load(txt));