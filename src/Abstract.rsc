module Abstract

import Syntax;

public data TYPE = Int() | String() | Boolean() | Float() | Matrix() | Vector2d() | Vector3d();    
	  
public alias JGId = str;                  
	  
public data PROGRAM =                       
  program(list[DECL] decls, list[STATEMENT] stats);

public data DECL =
  decl(JGId name, TYPE tp);

public data EXP = 
       id(JGId name)
     | IntCon(int iVal)
     | StrCon(str sVal)
     | BolCon(boolean bVal)
     | FloCon(float fVal)
     | MatCon(Matrix mVal)
     | V2Con(Vector2d v2Val)
     | V3Con(Vector3d v3Val)
     | notCon(EXP bVal)
     | invCon(EXP mVal)
     | tranCon(EXP mVal)
     | dot(EXP left, EXP right)
     | coma(EXP left, EXP right)
     | comb(EXP left, EXP right)
     | ands(EXP left, EXP right)
     | ors(EXP left, EXP right)
     | andc(EXP left, EXP right)
     | orc(EXP left, EXP right)
     | mul(EXP left, EXP right)
     | div(EXP left, EXP right)
     | add(EXP left, EXP right)
     | sub(EXP left, EXP right)
     | conc(EXP left, EXP right)
     ;
    
public data STATEMENT =
       asgStat(JGId name, EXP exp)
     | ifElseStat(EXP exp, list[STATEMENT] thenpart, list[STATEMENT] elsepart)
     | whileStat(EXP exp, list[STATEMENT] body)
     | forStat(JGId name, EXP exp, list[STATEMENT] forbody)
     ;

anno loc TYPE@location;                   
anno loc PROGRAM@location;
anno loc DECL@location;
anno loc EXP@location;
anno loc STATEMENT@location;

public alias Occurrence = tuple[loc location, JGId name, STATEMENT stat];