var pair$Qu = x => (typeof(x) == 'object' && 'car' in x && 'cdr' in x);

var cons = (h,t) => {return {car: h, cdr: t}; };
var car = p => p.car;
var cdr = p => p.cdr;
var cadr = p => p.cdr.car;

var null$Qu = x => Array.isArray(x) && !x.length;

var list$Qu = x => (null$Qu(x) || pair$Qu(x) && list$Qu(x.cdr));

var append = (...xs) => {
    xs = xs.filter(x=>!null$Qu(x));
    if(xs.length<1) return [];
    var res = xs[xs.length-1];
    for(var i=xs.length-2;i>=0;i--) {
        var tmp = __unlist(xs[i]);
        for(var j=tmp.length-1;j>=0;j--) {
            res = cons(tmp[j],res);
        }
    }
    return res
};

var append$Ex = (...xs) => {
    xs = xs.filter(x=>!null$Qu(x));
    if(xs.length<1) return [];
    for(var i=0;i<xs.length-1;i++) {
        var p = xs[i];
        while(pair$Qu(p)) {
            if(null$Qu(p.cdr)) {
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
    while(pair$Qu(xs)) { xs = xs.cdr ; len += 1; }
    if(null$Qu(xs)) return len;
    else throw "length wrong type argument";
};

var __list = (...xs) => {
    var res = [];
    for(var i=xs.length-1; i>=0; i--) { res = cons(xs[i],res); }
    return res;
};

var list = __list;

var __unlist = xs => {
    var arr = [];
    while(pair$Qu(xs)) { arr.push(xs.car); xs = xs.cdr; }
    if(!null$Qu(xs)) throw "__unlist wrong type argument"
    return arr;
};

var member = (e, l) => {
    while(pair$Qu(l)) {
        if(equal$Qu(e, l.car)) return l;
        l = l.cdr;
    }
    return false;
};

var drop = (n, l) => {
    while(n>0 && pair$Qu(l)) { l = l.cdr; n -= 1; }
    return l;
};

var take = (n, l) => {
    var q = [];
    while(n>0 && pair$Qu(l)) {q.push(l.car); l = l.cdr; n -= 1;}
    return __list(...q);
};

var any = (pred, l) => {
    while(pair$Qu(l)) {
        var res = pred(l.car);
        if(res!==false) return res;
        l = l.cdr;
    }
    return false;
};

var every = (pred, l) => {
    while(pair$Qu(l)) {
        if(pred(l.car)===false) return false;
        l = l.cdr;
    }
    return true;
};

var assoc = (key, alist) => {
    while(pair$Qu(alist)) {
	    if(equal$Qu(alist.car.car, key)) return alist.car;
	    alist = alist.cdr;
    }
    return false;
};

var for$Mneach = (f, l) => {
    while(pair$Qu(l)) { f(l.car); l=l.cdr; }
};

var map = (f, ...ls) => {
    var res = [];
    while(pair$Qu(ls[0])) {
        var args = ls.map(x => car(x));
        res.push(f.apply(null,args));
        ls = ls.map(x => cdr(x));        
    }
    return __list(...res);
}

var only = (pred, l) => __list(...__unlist(l).filter(pred));

//var fold$Mnleft = (op, init, l) =>  __unlist(l).reduce(op, init);
var fold$Mnleft = (op, init, l) => {
    var acc = init;
    while(pair$Qu(l)) {
        acc = op(acc, l.car);
        l = l.cdr;
    }
    return acc;
};


var union = (...sets) => {
    switch (sets.length) {
    case 0: return [];
    case 1: return sets[0];
    default: break;
    }
    var set = {};
    for (var s of sets) {
	    for (var x of __unlist(s)) {
	        set[serialize(x)] = x;
	    }
    }
    var result = [];
    for (var k in set) {
	result.push(set[k]);
    }
    return __list(...result);
};

var difference = (a, b) => {
    var bset = {};
    for (var x of __unlist(b)) {
	bset[stringify(x)] = 1;
    }
    return list(...__unlist(a).filter(x=> !(stringify(x) in bset)));
};
