/////////////////////////////////////////////////////////////////////
// LISTS

const __is_list = x => {
    while(__is_pair(x)) x=x.cdr;
    return __is_nil(x);
};

/////////////////////////////////////////////////////////////////////

var pair$Qu = __is_pair;
var cons = __cons;

var car = p => p.car;
var cdr = p => p.cdr;
var cadr = p => p.cdr.car;

var null$Qu = __is_nil;

var list$Qu = __is_list;

var append = (...xs) => {
    xs = xs.filter(x=>x!==__nil);
    if(xs.length<1) return __nil;
    var res = xs[xs.length-1];
    for(var i=xs.length-2;i>=0;i--) {
        var tmp = __list2vector(xs[i]);
        for(var j=tmp.length-1;j>=0;j--) {
            res = __cons(tmp[j],res);
        }
    }
    return res
};

var append$Ex = (...xs) => {
    xs = xs.filter(x=>x!==__nil);
    if(xs.length<1) return __nil;
    for(var i=0;i<xs.length-1;i++) {
        var p = xs[i];
        while(__is_pair(p)) {
            if(__is_nil(p.cdr)) {
                p.cdr = xs[i+1];
                break;
            }
            p = p.cdr;
        }
    }
    return xs[0];
};

var length = xs => {
    var len = 0;
    while(__is_pair(xs)) { xs = xs.cdr ; len += 1; }
    if(__is_nil(xs)) return len;
    else throw new Error("length wrong type argument");
};

var list = (...xs) => __vector2list(xs);

var member = (e, l) => {
    while(__is_pair(l)) {
        if(__equal2(e, l.car)) return l;
        l = l.cdr;
    }
    return false;
};

var drop = (n, l) => {
    while(n>0 && __is_pair(l)) { l = l.cdr; n -= 1; }
    return l;
};

var take = (n, l) => {
    var q = [];
    while(n>0 && __is_pair(l)) {q.push(l.car); l = l.cdr; n -= 1;}
    return __vector2list(q);
};

var any = (pred, l) => {
    while(__is_pair(l)) {
        var res = pred(l.car);
        if(res!==false) return res;
        l = l.cdr;
    }
    return false;
};

var every = (pred, l) => {
    while(__is_pair(l)) {
        if(pred(l.car)===false) return false;
        l = l.cdr;
    }
    return true;
};

var assoc = (key, alist) => {
    while(__is_pair(alist)) {
	    if(__equal2(alist.car.car, key)) return alist.car;
	    alist = alist.cdr;
    }
    return false;
};

var for$Mneach = (f, l) => {
    while(__is_pair(l)) { f(l.car); l=l.cdr; }
};

var map = (f, ...ls) => {
    var res = [];
    while(__is_pair(ls[0])) {
        var args = ls.map(x => x.car);
        res.push(f.apply(null,args));
        ls = ls.map(x => x.cdr);        
    }
    return __vector2list(res);
}

var only = (pred, l) => __vector2list(__list2vector(l).filter(pred));

var fold$Mnleft = (op, init, l) => {
    var acc = init;
    while(__is_pair(l)) {
        acc = op(acc, l.car);
        l = l.cdr;
    }
    return acc;
};


var union = (...sets) => {
    switch (sets.length) {
    case 0: return __nil;
    case 1: return sets[0];
    default: break;
    }
    var set = {};
    for (var s of sets) {
	    for (var x of __list2vector(s)) {
	        set[__serialize(x)] = x;
	    }
    }
    var result = [];
    for (var k in set) {
	result.push(set[k]);
    }
    return __vector2list(result);
};

var difference = (a, b) => {
    var bset = {};
    for (var x of __list2vector(b)) {
	bset[__serialize(x)] = 1;
    }
    return __vector2list(__list2vector(a).filter(x=> !(__serialize(x) in bset)));
};
