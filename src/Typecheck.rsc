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

TENV checkExp(exp:decCon(Dec N), TYPE req, TENV env) =                              
  req == dec() ? env : addError(env, exp@location, required(req, "int"));

TENV checkExp(exp:strCon(str S), TYPE req, TENV env) =
 req == string() ? env : addError(env, exp@location, required(req, "string"));
 
//add: type check for boolean 
TENV checkExp(exp:bolCon(bool B), TYPE req, TENV env) =                             
  req == boolean() ? env : addError(env, exp@location, required(req, "boolean"));
  
//add: type check for float 
TENV checkExp(exp:floatCon(Float F), TYPE req, TENV env) =
 req == float() ? env : addError(env, exp@location, required(req, "float"));
 
//add: type check for matrix 
TENV checkExp(exp:matrixCon(Matrix M), TYPE req, TENV env) =
 req == matrix() ? env : addError(env, exp@location, required(req, "Matrix"));
 
//add: type check for vector2d 
TENV checkExp(exp:vecaCon(Veca V), TYPE req, TENV env) =
 req == veca() ? env : addError(env, exp@location, required(req, "Vec2d"));
 
//add: type check for vector3d
TENV checkExp(exp:vecbCon(Vecb K), TYPE req, TENV env) =
 req == vecb() ? env : addError(env, exp@location, required(req, "Vector3d"));


//make sure after not the exp is in type boolean
TENV checkExp(exp:not(EXP B), TYPE req, TENV env) =                              
  req == boolean() ? checkExp(B, boolean(), env)
                    : addError(env, exp@location, required(req, "boolean"));
                    
//add: type check of inverse/transpose/dot
TENV checkExp(exp:invers(EXP M), TYPE req, TENV env) =                              
  req == matrix() ? checkExp(M, matrix(), env)
                    : addError(env, exp@location, required(req, "Matrix"));
                    
TENV checkExp(exp:transpose(EXP T), TYPE req, TENV env) =                              
  req == matrix() ? checkExp(T, matrix(), env)
                    : addError(env, exp@location, required(req, "Matrix"));                           

TENV checkExp(exp:dot(EXP E1, EXP E2), TYPE req, TENV env){
   if (req == float()) {
        if (checkExp(E1, veca(), env) == checkExp(E2, veca(), env)){
            return checkExp(E1, veca(), checkExp(E2, veca(), env));
        }
        else if (checkExp(E1, vecb(), env) == checkExp(E2, vecb(), env)){
            return checkExp(E1, vecb(), checkExp(E2, vecb(), env));
        }
        else{
            return addError(env, exp@location, required("same type(Vec2d/Vec3d)", "different types"));
        }
    }
    else {
        return addError(env, exp@location, required(req, "float"));
    }

}

//add: logical operator check for and/or(output type is bool);make sure both sides of exp should be in boolean type
TENV checkExp(exp:ands(EXP E1, EXP E2), TYPE req, TENV env) =                        
  req == boolean() ? checkExp(E1, boolean(), checkExp(E2, boolean(), env))
                   : addError(env, exp@location, required(req, "boolean"));
                   
TENV checkExp(exp:andc(EXP E1, EXP E2), TYPE req, TENV env) =                        
  req == boolean() ? checkExp(E1, boolean(), checkExp(E2, boolean(), env))
                   : addError(env, exp@location, required(req, "boolean"));
                   
TENV checkExp(exp:ors(EXP E1, EXP E2), TYPE req, TENV env) =                        
  req == boolean() ? checkExp(E1, boolean(), checkExp(E2, boolean(), env))
                   : addError(env, exp@location, required(req, "boolean"));
                   
TENV checkExp(exp:orc(EXP E1, EXP E2), TYPE req, TENV env) =                        
  req == boolean() ? checkExp(E1, boolean(), checkExp(E2, boolean(), env))
                   : addError(env, exp@location, required(req, "boolean"));
                 
//add: logical operator check for ==/!= (output type is bool); make sure both sides of exp should be in same type
                   
TENV checkExp(exp:coma(EXP E1, EXP E2), TYPE req, TENV env) {
    if (req == boolean()) {
        if (checkExp(E1, string(), env) == checkExp(E2, string(), env)){
            return checkExp(E1, string(), checkExp(E2, string(), env));
        }
        else if (checkExp(E1, dec(), env) == checkExp(E2, dec(), env)){
            return checkExp(E1, dec(), checkExp(E2, dec(), env));
        }
        else if (checkExp(E1, boolean(), env) == checkExp(E2, boolean(), env)){
            return checkExp(E1, boolean(), checkExp(E2, boolean(), env));
        }
        else if (checkExp(E1, float(), env) == checkExp(E2, float(), env)){
            return checkExp(E1, float(), checkExp(E2, float(), env));
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
    if (req == boolean()) {
        if (checkExp(E1, string(), env) == checkExp(E2, string(), env)){
            return checkExp(E1, string(), checkExp(E2, string(), env));
        }
        else if (checkExp(E1, dec(), env) == checkExp(E2, dec(), env)){
            return checkExp(E1, dec(), checkExp(E2, dec(), env));
        }
        else if (checkExp(E1, boolean(), env) == checkExp(E2, boolean(), env)){
            return checkExp(E1, boolean(), checkExp(E2, boolean(), env));
        }
        else if (checkExp(E1, float(), env) == checkExp(E2, float(), env)){
            return checkExp(E1, float(), checkExp(E2, float(), env));
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
    if (req == dec()){
        if (checkExp(E1, dec(), env) == checkExp(E2, dec(), env)){
            return checkExp(E1, dec(), checkExp(E2, dec(), env));
        }
        else{
            addError(env, exp@location, required("same type int", "different types"));
        }
    }
    else if(req == float()){
        if (checkExp(E1, float(), env) == checkExp(E2, float(), env)){
            return checkExp(E1, float(), checkExp(E2, float(), env));
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
    if (req == dec()){
        if (checkExp(E1, dec(), env) == checkExp(E2, dec(), env)){
            return checkExp(E1, dec(), checkExp(E2, dec(), env));
        }
        else{
            addError(env, exp@location, required("same type int", "different types"));
        }
    }
    else if(req == float()){
        if (checkExp(E1, float(), env) == checkExp(E2, float(), env)){
            return checkExp(E1, float(), checkExp(E2, float(), env));
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
    if (req == dec()){
        if (checkExp(E1, dec(), env) == checkExp(E2, dec(), env)){
            return checkExp(E1, dec(), checkExp(E2, dec(), env));
        }
        else{
            addError(env, exp@location, required("same type int", "different types"));
        }
    }
    else if(req == float()){
        if (checkExp(E1, float(), env) == checkExp(E2, float(), env)){
            return checkExp(E1, float(), checkExp(E2, float(), env));
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
    if (req == dec()){
        if (checkExp(E1, dec(), env) == checkExp(E2, dec(), env)){
            return checkExp(E1, dec(), checkExp(E2, dec(), env));
        }
        else{
            addError(env, exp@location, required("same type int", "different types"));
        }
    }
    else if(req == float()){
        if (checkExp(E1, float(), env) == checkExp(E2, float(), env)){
            return checkExp(E1, float(), checkExp(E2, float(), env));
        }
        else{
            addError(env, exp@location, required("same type float", "different types"));
        }
    }
    else{
        return addError(env, exp@location, required(req, "int or float"));
    }
}

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
    env0 = checkExp(Exp, dec(), env);
    env1 = checkStats(Stats1, env0);
    env2 = checkStats(Stats2, env1);
    return env2;
}

TENV checkStat(stat:whileStat(EXP Exp, 
                             list[STATEMENT] Stats1),
                 TENV env) {
    env0 = checkExp(Exp, dec(), env);
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