/*
	Author: Hong Zhang, Pieter Derks
	2IMP25 A2
*/


module Main

import IO;
import ListRelation;
import vis::Figure; 
import vis::Render;
import Set;
import List;
import Type;
import Map;
import Relation;
import String;
import util::ValueUI;
import util::Math;
import vis::Figure;
//import Visualisation;

import lang::java::flow::JavaToObjectFlow;
import analysis::flow::ObjectFlow;
import lang::java::jdt::m3::Core;
import lang::java::jdt::m3::AST;

//import ClassDiagram;
import FlowGraphsAndClassDiagrams;


alias OFG = rel[loc from, loc to];

//private loc project_loc = |project://eLib|;
private M3 m3Model; //M3 model from project
private FlowProgram prog; 
private OFG ofg;
private OFG proOfg; //propagate ofg


private list[Edge] tyDepen; //type dependency
private list[Edge] tyDepenInter = []; //type dependency with interface
private list[Edge] decl; //declartion with no field
private list[Edge] meInvoc; //method invocation
 
private lrel[str superClass, set[str] subClass] extendClasses = []; //extend classes



void main(){
	loc project_loc = |project://eLib|;
	setUpModel(project_loc);

    proEdges = Ofg2Edges(proOfg);
    list[Edge] edgeTypeMiss = [];
    for(e <- proEdges){
    	for(tD <- tyDepen){
	    	if (contains(e.from, "field") && contains(e.to, "class")
	    		&& e.from  == tD.from && contains(tD.to, "interface")) 
	    		edgeTypeMiss += e;
	    }
    }
    
    
	    
    
    list[Edge] declMiss = [];
    for(d <- decl){
    	for(e <- edgeTypeMiss){
    		if (e.from == d.from){
    			declMiss += d;
    		}
    	}
    }
    	//text(declMiss);
    	//text(tyDepen);
    	//text(edgeLinkMiss);
    	//text(tyDepenInter);
    
    
    lrel [str field, str class, str interface] suggestionRel = [];
    for(e <- edgeTypeMiss){
    	str fieldEdge = e.from;
    	str class = getEdgeClass(e.to);
    	str interface = "";
    	for(i <- tyDepenInter){
    		if(e.from == i.from){
    			interface = (getEdgeClass(i.to));
    		}
    	}
    	suggestionRel += <fieldEdge, class, interface>;
    }
    //text(suggestionRel);
    lrel[str field, set[str] classSet, str interface] newSuggestionRel = [];
    for(sr <- suggestionRel){
    	set[str] setCls = {};
    	for (r <- suggestionRel){
    		if(sr.field == r.field)
    			setCls += r.class;
    	}
    	newSuggestionRel += <sr.field, setCls, sr.interface>;
    }
    newSuggestionRel = dup (newSuggestionRel); 
    
    
    for(s <- newSuggestionRel){
		switch(s.interface){
			case "Map":
				//for (m <- meInvoc){
				for (d <- decl){
					for(mI <- meInvoc){
						if(s.field == d.from && s.field ==mI.from)
							println("Modify code at" + d.to + "\t===\> " 
									+ s.interface+"\<" + getType(mI.to) + ", "+ getSupClass(s.classSet) +"\> " 
									+ getEdgeClass(s.field));
					};
				}
			case "Collection":
				for (d <- decl){
					if(s.field == d.from)
						println("Modify code at" + d.to + "\t===\> " 
								+ s.interface+"\<"+ getSupClass(s.classSet) +"\> " 
								+ getEdgeClass(s.field));
				}
			
		}    
    }
    
}

private void setUpModel(loc project_loc){
	/*ofg and propagate ofg part*/
	m3Model = createM3FromEclipseProject(project_loc);
	ast = createAstsFromEclipseProject(project_loc, true);
	prog = createOFG(ast);
	ofg = buildFlowGraph(prog) 
	 + { <cs + "this", x> | newAssign(x, _, cs, _) <- prog.statements } ;
	 ;
	proOfg = propagateOFG(prog, ofg); 
	
	/*get relevant data from modelM3*/
	getExtendClasses(); //get ExtendClasses
		
	tyDepen = getTypeDependency(m3Model);  
		
	for(e <- tyDepen){
		if(contains(e.to, "interface"))
			tyDepenInter += e;
	}
	decl = getDeclarations(m3Model); 	//decliarations, filtering "field"
    meInvoc = getMethodInvocations(m3Model);
   		
}


private str getEdgeClass(str edge) {
    int length = size(edge);
    int startIndex;
    int endIndex;
    str newString = "";
    for (int i <- [length - 1..0]) {
        if (stringChar(charAt(edge, i)) == "/") {
            startIndex = i + 1;
            break;
        }
    }
    for (int i <- [length - 1..0]) {
        if ((stringChar(charAt(edge, i)) == "|")) {
            endIndex = i;
            break;
        }
    }
    newString = edge[startIndex..endIndex];
    return newString;
}

private str getType(str edge) {
    int length = size(edge);
    int startIndex = 0;
    int endIndex = size(edge);
    str newString = "";
    for (int i <- [length - 1..0]) {
        if (stringChar(charAt(edge, i)) == "/") {
            startIndex = i + 1;
            break;
        }
    }
  	for (int i <- [length - 1..0]) {
        if ((stringChar(charAt(edge, i)) == "|")) {
            endIndex = i;
            break;
        }
    }
    for (int i <- [length - 1..0]) {
        if ((stringChar(charAt(edge, i)) == "(")) {
            endIndex = i;
            break;
        }
    }
    
    return edge[startIndex..endIndex];
}


private OFG propagateOFG(FlowProgram p, OFG ofg){
    genFor = { <constr + "this", class> | newAssign(_, class, constr, _) <- p.statements, constructor(constr, _) <- p.decls };
    genBac = { <x, caller> | assign(y, caller, x) <- p.statements, caller != emptyId}
            + { <m + "return", caller> | call(caller, _, _, m, _) <- p.statements, caller != emptyId};
    
        
	OFG IN = { };
	OFG OUTFor = genFor;
	OFG OUTBac = genBac;
	
	//iOfg = {<to, from> | <from, to> <- ofg};	//inversed ofg
	iOfg = ofg<to, from>;
	
	set[loc] pred(loc n) = iOfg[n]; 
	set[loc] succ(loc n) = ofg[n];
	
	solve (IN, OUTFor) {
        IN = { <n,\o> | n <- carrier(ofg), x <- (pred(n)), \o <- OUTFor[x] };
        OUTFor = genFor + IN;
    }
    solve (IN, OUTBac) {
        IN = { <n,\o> | n <- carrier(ofg), x <- (succ(n)), \o <- OUTBac[x] };
        OUTBac = genBac + IN;
    }
	
	//println(size(OUTFor));
	//println(size(OUTBac));
	
	return OUTFor + OUTBac;
}

private list[Edge] getTypeDependency(M3 m){
	list[Edge] dependency = [edge("<from>", "<to>") | <from, to> <- m.typeDependency ];
    
    for (e <- dependency) {
        if (! contains(e.from, "field")) {	//filtering ...field
            dependency -= e;
        }
    }
    //text(dependency);
    return dependency;
}

private list[Edge] getDeclarations(M3 m){
	list[Edge] decl = [edge("<from>", "<to>") | <from, to> <- m.declarations ];
	for(d <- decl){
		if(!contains(d.from, "field"))
			decl -= d;
	}
	return decl;
}
private list[Edge] Ofg2Edges(OFG ofg) {
    return [edge("<from>", "<to>") | <from, to> <- ofg ];
}


private void getExtendClasses() {
    list[Edge] extends = [edge("<to>", "<from>") | <from, to> <- m3Model.extends ];
    for (e <- extends) {
        if (! containsClass(e.from)) {
            extendClasses += <e.from, {e.to}>;
        } else {
            for (ec <- extendClasses) {
                if (e.from == ec.superClass) {
                    set[str] tmp = ec.subClass;
                    extendClasses -= <e.from, tmp>;
                    tmp += e.to;
                    extendClasses += <e.from, tmp>;
                }
            }
        }        
    }        
}

private bool containsClass(str class) {
    for (e <- extendClasses) {
        if (e.superClass == class) {
            return true;
        }
    }
    return false;
}

private str getSupClass(set[str] classSet) {
    if (size(classSet) == 1) {
    	//println(classSet);
        return (toList(classSet)[0]);
    } else {
        for (c <- classSet) {
            for (e <- extendClasses) {
                if (c == getEdgeClass(e.superClass)) {
                    return getEdgeClass(e.superClass);
                } else {
                    for (s <- e.subClass) {
                        if (c == getEdgeClass(s)) {
                            return getEdgeClass(e.superClass);
                        }
                    }
                }
            }
        }
        return "Object";
    }
}

private list[Edge] getMethodInvocations(M3 m){

	// We look at the method invocations of the M3 model to see what methods Map.get and Map.remove are being called from
	OFG foundMatchesGet = {pair | pair <- m.methodInvocation, pair[1] == |java+method:///java/util/Map/get(java.lang.Object)|};
	OFG foundMatchesRemove = {pair | pair <- m.methodInvocation, pair[1] == |java+method:///java/util/Map/remove(java.lang.Object)|};
	OFG foundMatches = foundMatchesGet + foundMatchesRemove;
	set[loc] foundMethods = foundMatches.from;
	
	// For every found method, we check if there exists a constructor of type java/lang, and if so, return the method name and the constructor
	OFG methodTypes = {invoc | methodName <- foundMethods, invoc <- m.methodInvocation, invoc[0] == methodName && contains(invoc[1].path, "java/lang")};
	
	// Get the fields that the method being called accesses;
	OFG derivedTypes = {<methodField[1], methodType[1]> | methodField <- m.fieldAccess, methodType <- methodTypes, methodField[0] == methodType[0]};

	
	list[Edge] edgeDerivedTypes = [edge("<from>", "<to>") | <from, to> <- derivedTypes ];
	return edgeDerivedTypes;
}