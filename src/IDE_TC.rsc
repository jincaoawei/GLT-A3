module IDE_TC

import Prelude;
import util::IDE;
import util::ValueUI;

import vis::Figure;
import vis::Render;

import Syntax;
import Abstract;
import Typecheck;

//  define the language name and extension

private str JG_NAME = "jg";
private str JG_EXT = "jg";

//  Define the connection with the JG parser
Tree parser(str x, loc l) {
    return parse(#Program, x, l);
}

//  Define connection with the JG typechecker
// (includes type checking and uninitialized variables check)

public Program checkJGProgram(Program x) {
	p = implode(#PROGRAM, x);
	env = checkProgram(p);
	errors = { error(v, l) | <loc l, JGId v> <- env.errors };
	
	return x[@messages = errors];
    
}


public void registerJG() {
  registerLanguage(JG_NAME, JG_EXT, parser);
  registerAnnotator(JG_NAME, checkJGProgram);
}