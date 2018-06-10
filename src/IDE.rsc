module IDE

import Prelude;
import util::IDE;
import util::ValueUI;

import vis::Figure;
import vis::Render;

import Syntax;



private str JG_NAME = "jg";
private str JG_EXT = "jg";


Tree parser(str x, loc l) {
    return parse(#Program, x, l);
}

public void registerJG() {
  registerLanguage(JG_NAME, JG_EXT, parser);
}