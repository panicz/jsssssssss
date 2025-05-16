var improper$Qu = x => typeof(x) == 'object'
    && typeof(x.improper) != 'undefined'
    && Array.isArray(x.improper)
    && x.improper.length>0
    && typeof(x.tail) != 'undefined';

var pair$Qu = x => typeof(x) == 'object'
    && ((Array.isArray(x) && x.length>0)
	|| improper$Qu(x));

var cons = (h,t) => Array.isArray(t)
    ? [h].concat(t)
    : improper$Qu(t)
    ? {improper: [h].concat(t.improper),
       tail: t.tail}
    : {improper: [h], tail: t};

var car = p => Array.isArray(p)
    ? p[0]
    : p.improper[0];

var cdr = p => Array.isArray(p)
    ? p.slice(1)
    : (p.improper.length > 0
       ? {improper: p.improper.slice(1), tail: p.tail}
       : p.tail);

var null$Qu = x => Array.isArray(x) && !x.length;

var append = (...xs) => xs.length == 0
    ? []
    : xs[0].concat(...xs.slice(1));

var append$Ex = (...xs) => {
    if (xs.length == 0) {
	return xs;
    }
    
    for (var x of xs.slice(1)) {
	if (Array.isArray(x)) {
	    xs[0].push(...x);
	} else {
	    return {improper: xs[0], tail: x};
	}
    }
    return xs[0];
}

var list = (...xs) => xs;

var for$Mneach = (f, l) => { for (var x of l) { f(x); } };

var map = (f, ...ls) => {
    switch (ls.length) {
    case 0: return [];
    case 1: return ls[0].map(f);
    default: break;
    }
    var result = [];
    for (var i = 0; i < ls[0].length; ++i) {
	var args = [];
	for (var arglist of ls) {
	    if (arglist.length < i) {
		return result;
	    }
	    args.push(arglist[i]);
	}
	result.push(f.apply(null, args));
    }
    return result;
};
