module Abstract

import Syntax;

public data TYPE = dec() | string() | boolean() | float() | matrix() | veca() | vecb();    

//public data TYPE = Int() | String() | Boolean();
	  
public alias JGId = str; 

public data PROGRAM =                       
  program(list[DECL] decls, list[STATEMENT] stats); 

public data DECL =
  decl(JGId name, TYPE tp);                

public data EXP = 
       id(JGId name)
     | decCon(Dec iVal)
     | strCon(str sVal)
     | bolCon(bool bVal)
     | floatCon(Float fVal)
     | matrixCon(Matrix mVal)
     | vecaCon(Veca xVal)
     | vecbCon(Vecb yVal)
     | not(EXP nVal)
     | invers(EXP kVal)
     | transpose(EXP tVal)
     | dot(EXP left, EXP right)
     | coma(EXP left, EXP right)
     | comb(EXP left, EXP right)
     | ands(EXP left, EXP right)
     | andc(EXP left, EXP right)
     | ors(EXP left, EXP right)
     | orc(EXP left, EXP right)
     | mul(EXP left, EXP right)
     | div(EXP left, EXP right)
     | add(EXP left, EXP right)
     | sub(EXP left, EXP right)
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