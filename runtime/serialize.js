/////////////////////////////////////////////////////////////////////
// SERIALIZE

const __char_name = c => {
    let i = c.char.codePointAt(0);
    if (i < 16) {
	return "x0"+i.toString(16);
    }
    if (i <= 32) {
	return "x"+i.toString(16);
    }
    return c.char;
};

const __serialize = e => {
    switch(true) {
    case __is_nil(e): return "()";
    case __is_boolean(e): return e ? "#t" : "#f";
    case __is_number(e): return __number2string(e);
    case __is_char(e): return "#\\"+__char_name(e);
    case __is_symbol(e): return __symbol2string(e);
    case __is_string(e): return JSON.stringify(e); /// :)
    case __is_vector(e): return "#("+e.map(__serialize).join(" ")+")";
    case __is_pair(e):
        var str = "(";
        while(true) {
            str += __serialize(e.car);
            if(e.cdr===__nil) { return str + ")"; }
            str += " ";
            if(__is_pair(e.cdr)) { e = e.cdr; }
            else { return str += ". " + __serialize(e.cdr) + ")"; }
        }
    case __is_procedure(e): return "#<"+e.toString()+">";
    case __is_eof_object(e): return "#<eof-object>";
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

const __arrays_equal = (a, b) => {
    if (a.length != b.length) return false;
    for (var i = 0; i < a.length; ++i) {
	    if (!__equal2(a[i], b[i])) {
	        return false;
	    }
    }
    return true;
};

const __objects_equal = (a, b) => {
    var a_keys = Object.keys(a);
    var b_keys = Object.keys(b);
    if (a_keys.length != b_keys.length) return false;
    for (var x of a_keys) {
	    if (!(x in b) || !__equal2(a[x], b[x])) return false;
    }
    return true;
};

const __equal2 = (x,y) => {
    if(x===undefined) return y===undefined;
    if(x.constructor===y.constructor) {
        return (__eqv2(x,y) ||                           
                (x.constructor==Array && __arrays_equal(x,y)) ||
                (x.constructor==Object &&
                 (('car' in x && 'cdr' in x &&
                   'car' in y && 'cdr' in y &&
                   __equal2(x.car, y.car) &&
                   __equal2(x.cdr, y.cdr))
                  || __objects_equal(x,y))))
    }
    return false;
};

/////////////////////////////////////////////////////////////////////

var serialize = __serialize;
let arrays$Mnequal$Qu = __arrays_equal;

let objects$Mnequal$Qu = __objects_equal;

var equal$Qu = mk_seq_rel(__equal2);

let __stringify = (e) => __is_string(e)?e:__serialize(e); /// hmm.

var writeln = (...args) => {
    console.log(args.map(__stringify).join(''));
};

var error = (...msg) => { throw new Error(msg.map(__stringify).join('')); };

var assert = c => { if(!c) {error("Assertion failed");} };

var invalid$Mnexample =
    __make_parameter((expression, actual, expected) =>
	(typeof(expected) == 'undefined')
	    ? error("expected ",expression," to be non-#false")
	    : error("while evaluating\n\n  ",
		    expression,
		    "\n\nexpected:\n\n  ",
		    expected,
		    "\n\ngot:\n\n  ",
		    actual, "\n"))

var valid$Mnexample =
    __make_parameter((expression, actual, expected) => actual)

var slot$Mnref = (x, prop) => x[__symbol2string(prop)];

var slot$Mnset$Ex = (x, prop, v) => x[__symbol2string(prop)] = v;
