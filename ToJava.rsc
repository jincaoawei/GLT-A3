module ToJava

import Abstract;

public data Instr =
       Double (str Id)    // Reserve a memory location for a natural variable
     | String (str Id)    // Reserve a memory location for a string variable
     ;