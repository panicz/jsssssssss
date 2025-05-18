var mk_seq_rel = rel => (...xs) => {
    for(var i = 1; i < xs.length; ++i) {
	if(!rel(xs[i-1], xs[i])) return false;
    }
    return true;
};


var boolean$Qu = x => typeof(x) == 'boolean';

var eqv$Qu = mk_seq_rel(
    (a, b) =>
    (null$Qu(a) && null$Qu(b))
	|| (symbol$Qu(a) && symbol$Qu(b)
	    && a.symbol == b.symbol)
	|| (boolean$Qu(a) && boolean$Qu(b) && a == b)
	|| (number$Qu(a) && number$Qu(b) && a == b)
	|| (char$Qu(a) && char$Qu(b) && a.char == b.char)
	|| a === b
);

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

var noop = ()=>{};

var apply = (f, ...args) => {
    var collected = args.slice(0, args.length-1)
        .concat(args[args.length-1]);
    return f.apply(null, collected);
};

var procedure$Qu = x => typeof(x) == 'function';
