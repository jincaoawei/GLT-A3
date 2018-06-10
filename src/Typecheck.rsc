module Typecheck

import Prelude;
import Abstract;
import Load;

alias TENV = tuple[ map[JGId, TYPE] symbols, list[tuple[loc l, str msg]] errors]; 

TENV addError(TENV env, loc l, str msg) = env[errors = env.errors + <l, msg>];      

str required(TYPE t, str got) = "Required <getName(t)>, got <got>";                 
str required(TYPE t1, TYPE t2) = required(t1, getName(t2));

// compile Expressions.
TENV checkExp(exp:id(JGId Id), TYPE req, TENV env) {                              
  if(!env.symbols[Id]?)
     return addError(env, exp@location, "Undeclared variable <Id>");
  tpid = env.symbols[Id];
  return req == tpid ? env : addError(env, exp@location, required(req, tpid));
}

TENV checkExp(exp:IntCon(int N), TYPE req, TENV env) =                              
  req == Int() ? env : addError(env, exp@location, required(req, "int"));

TENV checkExp(exp:StrCon(str S), TYPE req, TENV env) =
 req == String() ? env : addError(env, exp@location, required(req, "string"));
 
//add: type check for boolean 
TENV checkExp(exp:BolCon(boolean B), TYPE req, TENV env) =                             
  req == Boolean() ? env : addError(env, exp@location, required(req, "boolean"));
  
//add: type check for float 
TENV checkExp(exp:FloCon(float F), TYPE req, TENV env) =
 req == Float() ? env : addError(env, exp@location, required(req, "float"));
 
//add: type check for matrix 
TENV checkExp(exp:MatCon(Matrix M), TYPE req, TENV env) =
 req == Matrix() ? env : addError(env, exp@location, required(req, "matrix"));
 
//add: type check for vector2d 
TENV checkExp(exp:V2Con(Vector2d V), TYPE req, TENV env) =
 req == Vector2d() ? env : addError(env, exp@location, required(req, "Vector2d"));
 
//add: type check for vector3d
TENV checkExp(exp:V3Con(Vector3d K), TYPE req, TENV env) =
 req == Vector3d() ? env : addError(env, exp@location, required(req, "Vector3d"));


//make sure after not the exp is in type boolean
TENV checkExp(exp:not(EXP B), TYPE req, TENV env) =                              
  req == Boolean() ? checkExp(B, Boolean(), env)
                    : addError(env, exp@location, required(req, "boolean"));
                    
//add: type check of inverse/transpose/dot
TENV checkExp(exp:inv(EXP M), TYPE req, TENV env) =                              
  req == Matrix() ? checkExp(B, Matrix(), env)
                    : addError(env, exp@location, required(req, "Matrix"));
                    
TENV checkExp(exp:tran(EXP M), TYPE req, TENV env) =                              
  req == Matrix() ? checkExp(B, Matrix(), env)
                    : addError(env, exp@location, required(req, "Matrix"));                           

TENV checkExp(exp:dot(EXP E1, EXP E2), TYPE req, TENV env){
   if (req == Float()) {
        if (checkExp(E1, Vector2d(), env) == checkExp(E2, Vector2d(), env)){
            return checkExp(E1, Vector2d(), checkExp(E2, Vector2d(), env));
        }
        else if (checkExp(E1, Vector3d(), env) == checkExp(E2, Vector3d(), env)){
            return checkExp(E1, Vector3d(), checkExp(E2, Vector3d(), env));
        }
        else{
            return addError(env, exp@location, required("same type(Vector2d/Vector3d)", "different types"));
        }
    }
    else {
        return addError(env, exp@location, required(req, "float"));
    }

}
//add: logical operator check for and/or(output type is bool);
//make sure both sides of exp should be in boolean type
TENV checkExp(exp:ands(EXP E1, EXP E2), TYPE req, TENV env) =                        
  req == Boolean() ? checkExp(E1, Boolean(), checkExp(E2, Boolean(), env))
                   : addError(env, exp@location, required(req, "boolean"));
                   
TENV checkExp(exp:ors(EXP E1, EXP E2), TYPE req, TENV env) =                        
  req == Boolean() ? checkExp(E1, Boolean(), checkExp(E2, Boolean(), env))
                   : addError(env, exp@location, required(req, "boolean"));
                   
TENV checkExp(exp:andc(EXP E1, EXP E2), TYPE req, TENV env) =                        
  req == Boolean() ? checkExp(E1, Boolean(), checkExp(E2, Boolean(), env))
                   : addError(env, exp@location, required(req, "boolean"));
                   
TENV checkExp(exp:orc(EXP E1, EXP E2), TYPE req, TENV env) =                        
  req == Boolean() ? checkExp(E1, Boolean(), checkExp(E2, Boolean(), env))
                   : addError(env, exp@location, required(req, "boolean"));
                 
//add: logical operator check for ==/!= (output type is bool); 
//make sure both sides of exp should be in same type
                   
TENV checkExp(exp:coma(EXP E1, EXP E2), TYPE req, TENV env) {
    if (req == Boolean()) {
        if (checkExp(E1, String(), env) == checkExp(E2, String(), env)){
            return checkExp(E1, String(), checkExp(E2, String(), env));
        }
        else if (checkExp(E1, Int(), env) == checkExp(E2, Int(), env)){
            return checkExp(E1, Int(), checkExp(E2, Int(), env));
        }
        else if (checkExp(E1, Boolean(), env) == checkExp(E2, Boolean(), env)){
            return checkExp(E1, Boolean(), checkExp(E2, Boolean(), env));
        }
        else {
            return addError(env, exp@location, required("same type", "different types"));
        }
    }
    else {
        return addError(env, exp@location, required(req, "boolean"));
    }   
}

TENV checkExp(exp:comb(EXP E1, EXP E2), TYPE req, TENV env) {
    if (req == Boolean()) {
        if (checkExp(E1, String(), env) == checkExp(E2, String(), env)){
            return checkExp(E1, String(), checkExp(E2, String(), env));
        }
        else if (checkExp(E1, Int(), env) == checkExp(E2, Int(), env)){
            return checkExp(E1, Int(), checkExp(E2, Int(), env));
        }
        else if (checkExp(E1, Boolean(), env) == checkExp(E2, Boolean(), env)){
            return checkExp(E1, Boolean(), checkExp(E2, Boolean(), env));
        }
        else {
            return addError(env, exp@location, required("same type", "different types"));
        }
    }
    else {
        return addError(env, exp@location, required(req, "boolean"));
    }   
}

//add: multiply/divided/add/substract (allow both int and float but type of left side and right side should be same)
TENV checkExp(exp:mul(EXP E1, EXP E2), TYPE req, TENV env) {
    if (req == Int()){
        if (checkExp(E1, Int(), env) == checkExp(E2, Int(), env)){
            return checkExp(E1, Int(), checkExp(E2, Int(), env));
        }
        else{
            addError(env, exp@location, required("same type int", "different types"));
        }
    }
    else if(req == float()){
        if (checkExp(E1, Float(), env) == checkExp(E2, Float(), env)){
            return checkExp(E1, Float(), checkExp(E2, Float(), env));
        }
        else{
            addError(env, exp@location, required("same type float", "different types"));
        }
    }
    else{
        return addError(env, exp@location, required(req, "int or float"));
    }
}      

TENV checkExp(exp:div(EXP E1, EXP E2), TYPE req, TENV env) {
    if (req == Int()){
        if (checkExp(E1, Int(), env) == checkExp(E2, Int(), env)){
            return checkExp(E1, Int(), checkExp(E2, Int(), env));
        }
        else{
            addError(env, exp@location, required("same type int", "different types"));
        }
    }
    else if(req == float()){
        if (checkExp(E1, Float(), env) == checkExp(E2, Float(), env)){
            return checkExp(E1, Float(), checkExp(E2, Float(), env));
        }
        else{
            addError(env, exp@location, required("same type float", "different types"));
        }
    }
    else{
        return addError(env, exp@location, required(req, "int or float"));
    }
}

TENV checkExp(exp:add(EXP E1, EXP E2), TYPE req, TENV env) {
    if (req == Int()){
        if (checkExp(E1, Int(), env) == checkExp(E2, Int(), env)){
            return checkExp(E1, Int(), checkExp(E2, Int(), env));
        }
        else{
            addError(env, exp@location, required("same type int", "different types"));
        }
    }
    else if(req == float()){
        if (checkExp(E1, Float(), env) == checkExp(E2, Float(), env)){
            return checkExp(E1, Float(), checkExp(E2, Float(), env));
        }
        else{
            addError(env, exp@location, required("same type float", "different types"));
        }
    }
    else{
        return addError(env, exp@location, required(req, "int or float"));
    }
}

TENV checkExp(exp:sub(EXP E1, EXP E2), TYPE req, TENV env) {
    if (req == Int()){
        if (checkExp(E1, Int(), env) == checkExp(E2, Int(), env)){
            return checkExp(E1, Int(), checkExp(E2, Int(), env));
        }
        else{
            addError(env, exp@location, required("same type int", "different types"));
        }
    }
    else if(req == float()){
        if (checkExp(E1, Float(), env) == checkExp(E2, Float(), env)){
            return checkExp(E1, Float(), checkExp(E2, Float(), env));
        }
        else{
            addError(env, exp@location, required("same type float", "different types"));
        }
    }
    else{
        return addError(env, exp@location, required(req, "int or float"));
    }
}

//TENV checkExp(exp:mul(EXP E1, EXP E2), TYPE req, TENV env) =                        
//  req == natural() ? checkExp(E1, natural(), checkExp(E2, natural(), env))
//                   : addError(env, exp@location, required(req, "natural"));
  
//TENV checkExp(exp:div(EXP E1, EXP E2), TYPE req, TENV env) =                      
//  req == natural() ? checkExp(E1, natural(), checkExp(E2, natural(), env))
//                   : addError(env, exp@location, required(req, "natural"));
//
//TENV checkExp(exp:add(EXP E1, EXP E2), TYPE req, TENV env) =                        
//  req == natural() ? checkExp(E1, natural(), checkExp(E2, natural(), env))
//                   : addError(env, exp@location, required(req, "natural"));
//  
//TENV checkExp(exp:sub(EXP E1, EXP E2), TYPE req, TENV env) =                      
//  req == natural() ? checkExp(E1, natural(), checkExp(E2, natural(), env))
//                   : addError(env, exp@location, required(req, "natural"));

TENV checkExp(exp:conc(EXP E1, EXP E2), TYPE req, TENV env) =                    
  req == string() ? checkExp(E1, string(), checkExp(E2, string(), env))
                   : addError(env, exp@location, required(req, "string"));


// check a statement

TENV checkStat(stat:asgStat(JGId Id, EXP Exp), TENV env) {                        
  if(!env.symbols[Id]?)
     return addError(env, stat@location, "Undeclared variable <Id>");
  tpid = env.symbols[Id];
  return checkExp(Exp, tpid, env);
}
	
TENV checkStat(stat:ifElseStat(EXP Exp,                                             
                              list[STATEMENT] Stats1,
                              list[STATEMENT] Stats2),
               TENV env){
    env0 = checkExp(Exp, Int(), env);
    env1 = checkStats(Stats1, env0);
    env2 = checkStats(Stats2, env1);
    return env2;
}

TENV checkStat(stat:whileStat(EXP Exp, 
                             list[STATEMENT] Stats1),
                 TENV env) {
    env0 = checkExp(Exp, Int(), env);
    env1 = checkStats(Stats1, env0);
    return env1;
}

/*add: statement check for for (in my case, I allow id can be any types, and expression
should follow the type of id, thus 'for i(string) in ("aaa") {stat1}' is allowed, but 'for
i(int) in ("aaa")' is not allowed)*/
TENV checkStat(stat:forStat(JGId Id, EXP Exp, 
                             list[STATEMENT] Stats1),
                 TENV env) {
    if(!env.symbols[Id]?)
       return addError(env, stat@location, "Undeclared variable <Id>");
    tpid = env.symbols[Id];
    env0 = checkExp(Exp, tpid, env);
    env1 = checkStats(Stats1, env0);
    return env1;
}

// check a list of statements
TENV checkStats(list[STATEMENT] Stats1, TENV env) {                                 
  for(S <- Stats1){
      env = checkStat(S, env);
  }
  return env;
}
  
// check declarations

TENV checkDecls(list[DECL] Decls) =                                                 
    <( Id : tp  | decl(JGId Id, TYPE tp) <- Decls), []>;

// check a JG program

public TENV checkProgram(PROGRAM P){                                                
  if(program(list[DECL] Decls, list[STATEMENT] Series) := P){
     TENV env = checkDecls(Decls);
     return checkStats(Series, env);
  } else
    throw "Cannot happen";
}
                                                                                    
public list[tuple[loc l, str msg]] checkProgram(str txt) = checkProgram(load(txt)).errors;