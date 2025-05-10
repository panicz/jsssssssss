var improper$Qu = x => typeof(x) == 'object'
    && typeof(x.improper) != 'undefined'
    && Array.isArray(x.improper)
    && x.improper.length>0
    && typeof(x.tail) != 'undefined';

var pair$Qu = x => typeof(x) == 'object'
    && ((Array.isArray(x) && x.length>0)
	|| (typeof(x.car) != 'undefined'
	    && typeof(x.cdr) != 'undefined')
	|| improper$Qu(x));

var cons = (h,t) => Array.isArray(t)
    ? [h].concat(t)
    : (improper$Qu(t)
       ? {improper: [h].concat(t.improper), tail: t.tail}
       : {car: h, cdr: t});

var car = p => Array.isArray(p)
    ? p[0]
    : (improper$Qu(p)
       ? p.improper[0]
       : p.car);

var cdr = p => Array.isArray(p)
    ? p.slice(1)
    : (improper$Qu(p)
       ? (p.improper.length > 0
	  ? {improper: p.improper.slice(1), tail: p.tail}
	  : p.tail)
       : p.cdr);

var null$Qu = x => Array.isArray(x) && !x.length;

var append = (...xs) => xs.length == 0 ? [] : xs[0].concat(...xs.slice(1));

var append$Ex = (...xs) => {
    if (xs.length == 0) {
	return xs;
    }
    for (var x of xs.slice(1)) {
	xs[0].push(...x);
    }
    return xs[0];
}

var list = (...xs) => xs;

var for$Mneach = (f, l) => { for (var x of l) { f(x); } };
