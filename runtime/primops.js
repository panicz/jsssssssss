/////////////////////////////////////////////////////////////////////
// PRIMOPS

const __nil = Symbol();
const __is_nil = x => x===__nil;

const __is_number = x => typeof(x)=='number';
const __is_boolean = x => typeof(x)=='boolean';
const __is_procedure = x => typeof(x) == 'function';
const __is_char = x => typeof(x) == 'object' && typeof(x.char) == 'string' && x.char.length == 1;
const __is_string = x => typeof(x) == 'string';
const __is_symbol = x => typeof(x) == 'object' && typeof(x.symbol) == 'string';
const __is_vector = x => typeof(x) == 'object' && x.constructor == Array;
const __is_pair = x => typeof(x) == 'object' && 'car' in x && 'cdr' in x;

const __eqv2 = (a,b) => {
    if(a===b) return true;
    switch(true) {
    case __is_nil(a): return __is_nil(b);
    case __is_string(a) && __is_string(b):
    case __is_number(a) && __is_number(b):
    case __is_boolean(a) && __is_boolean(b):
        return a===b;
    case __is_symbol(a) && __is_symbol(b):
        return a.symbol===b.symbol;
    case __is_char(a) && __is_char(b):
        return a.char===b.char;
    default:
        return false;
    }
};

const __cons = (h,t) => {return {car: h, cdr: t}; };

const __apply = (f, ...args) => {
    var collected = args.slice(0, args.length-1)
        .concat(__list2vector(args[args.length-1]));
    return f.apply(null, collected);
};


var mk_seq_rel = rel => (...xs) => {
    for(var i = 1; i < xs.length; ++i) {
	if(!rel(xs[i-1], xs[i])) return false;
    }
    return true;
};

/////////////////////////////////////////////////////////////////////

var boolean$Qu = __is_boolean;

var eqv$Qu = mk_seq_rel(__eqv2);
var eq$Qu = eqv$Qu;
var $Eq = eq$Qu;

var $Pl = (...xs) => xs.reduce((n,m) => n+m, 0);

var $Mn = (...xs) => xs.length>1 ? xs.reduce((n,m) => n-m) : -xs[0];

var $St = (...xs) => xs.reduce((n,m) => n*m, 1);

var $Sl = (...xs) => xs.length>1 ? xs.reduce((n,m) => n/m) : 1/xs[0];

var not = x => !x;

var $Gt = mk_seq_rel((n,m) => n > m);

var $Ls = mk_seq_rel((n,m) => n < m);

var $Gt$Eq = mk_seq_rel((n,m) => n >= m);

var $Ls$Eq = mk_seq_rel((n,m) => n <= m);

var apply = __apply;

var procedure$Qu = __is_procedure;
