var pair$Qu = x => (typeof(x) == 'object' && 'car' in x && 'cdr' in x);

var cons = (h,t) => {return {car: h, cdr: t}; };
var car = p => p.car;
var cdr = p => p.cdr;
var cadr = p => p.cdr.car;

var null$Qu = x => Array.isArray(x) && !x.length;

var list$Qu = x => (null$Qu(x) || pair$Qu(x) && list$Qu(x.cdr))

var append = (...xs) => {
    var res = xs[xs.length-1];
    for(var i=xs.length-2;i>=0;i--) {
        var tmp = __unlist(xs[i]);
        for(var j=tmp.length-1;j>=0;j--) {
            res = cons(tmp[j],res);
        }
    }
    return res
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
        if(equal$Qu(e, l.car)) return true;
        l = l.cdr;
    }
    return false;
};

var drop = (n, l) => {
    while(n>0) { l = l.cdr; n -= 1; }
    return l;
};

var take = (n, l) => {
    var q = [];
    while(n>0) {q.push(l.car); l = l.cdr; n -= 1;}
    return __list(...q);
};

var any = (pred, l) => {
    while(pair$Qu(l)) {
        if(pred(l.car)) return true;
        l = l.cdr;
    }
    return false;
};

var every = (pred, l) => {
    while(pair$Qu(l)) {
        if(!pred(l.car)) return false;
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

/// wot bootstrapping c'nie
var map1 = ((f,xs)=>{return ((null$Qu(xs))===false?(cons(f(car(xs)),map1(f,cdr(xs)))):([]));});
var map = ((f,...xs)=>{var xs=__list(...xs);return ((pair$Qu(car(xs)))===false?([]):(cons(apply(f,map1(car,xs)),apply(map,f,map1(cdr,xs)))));});
/// no sorki
var only = (pred, l) => __list(...__unlist(l).filter(pred));
var fold$Mnleft = (op, init, l) => __unlist(l).reduce(op, init);

