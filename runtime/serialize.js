var serialize = e => {
    switch(true) {
    case null$Qu(e): return "()";
    case boolean$Qu(e): return e ? "#t" : "#f";
    case number$Qu(e): return number$Mn$Gtstring(e);
    case char$Qu(e): return "#\\"+charName(e);
    case symbol$Qu(e): return symbol$Mn$Gtstring(e);
    case string$Qu(e): return JSON.stringify(e);
    case vector$Qu(e): return "#("+e.vector.map(serialize).join(" ")+")";
    case pair$Qu(e):
	if(Array.isArray(e))
	    return "("+e.map(serialize).join(" ")+")";
	if (improper$Qu(e))
	    return "("+e.improper.map(serialize).join(" ")
	    + " . " + serialize(e.tail) + ")";
	return "(" + serialize(e.car) + " . "
            + serialize(e.cdr) + ")";
    case procedure$Qu(e): return "#<"+e.toString()+">";
    case eof$Mnobject$Qu(e): return "#<eof-object>";
    default:
	if (typeof(e) == 'object') {
	    if (e.constructor.name == "Object") {
		return "#<"+JSON.stringify(e)+">";
	    }
	    return "#<"+e.constructor.name+JSON.stringify(e)+">";
	}
	return "#<"+typeof(e)+">";
    }
};

var equal$Qu = mk_seq_rel((x,y) => serialize(x) == serialize(y)); /// i nikomu nie wolno się z tego śmiać

var writeln = e => { console.log(serialize(e)) ; return e };

let stringify = (e) => {
    if(string$Qu(e)) {
	return e;
    }
    return serialize(e);
}

var error = (...msg) => { throw new Error(msg.map(stringify).join('')); };

var assert = c => { if(!c) {error("Assertion failed");} };

var invalid$Mnexample =
    make$Mnparameter((expression, actual, expected) =>
	(typeof(expected) == 'undefined')
	    ? error("expected ",expression," to be non-#false")
	    : error("while evaluating\n\n  ",
		    expression,
		    "\n\nexpected:\n\n  ",
		    expected,
		    "\n\ngot:\n\n  ",
		    actual, "\n"))

var valid$Mnexample =
    make$Mnparameter((expression, actual, expected) => actual)

var slot$Mnref = (x, prop) => x[symbol$Mn$Gtstring(prop)];
var slot$Mnset$Ex = (x, prop, v) => x[symbol$Mn$Gtstring(prop)] = v;
