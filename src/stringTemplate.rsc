module StringTemplate

import String;
import IO;
import Set;
import List;

// Capitalize the first character of a string

public str capitalize(str s) {  
  return toUpperCase(substring(s, 0, 1)) + substring(s, 1);
}


private map[str,str] genFields(List[DECL] descs) {
	map[str,str] fields = ();
	for(f<-descs){
		
	}
}


// Helper function to generate a setter
private str genSetter(map[str,str] fields, str x) {
  return "public void set<capitalize(x)>(<fields[x]> <x>) {
         '  this.<x> = <x>;
         '}";
}

// Helper function to generate a getter
private str genGetter(map[str,str] fields, str x) {
  return "public <fields[x]> get<capitalize(x)>() {
         '  return <x>;
         '}";
}

// Generate a class with given name and fields.
// The field names are processed in sorted order.
public str genClass(str name, map[str,str] fields) { 
  return 
    "public class <name> {
    '  <for (x <- sort([f | f <- fields])) {>
    '  private <fields[x]> <x>;
    '  <genSetter(fields, x)>
    '  <genGetter(fields, x)><}>
    '}";
}