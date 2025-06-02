/*
var pair$Qu = x => (typeof(x) == 'object'
	                && typeof(x.car) != 'undefined'
                    && typeof(x.cdr) != 'undefined');
*/

var pair$Qu = x => (typeof(x) == 'object' && 'car' in x && 'cdr' in x);

var cons = (h,t) => {return {car: h, cdr: t}; };
var car = p => {return p.car} ;
var cdr = p => {return p.cdr} ;
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
