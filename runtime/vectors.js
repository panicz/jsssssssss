var vector$Qu = x => typeof(x) == 'object' && Array.isArray(x);

var make$Mnvector = (k,f) => { return Array(k).fill(f); };
var vector = (...xs) => {return xs; };

var list$Mn$Gtvector = l => {
    var v = Array(length(l));
    for(var i=0;i<v.length;i++) { v[i] = l.car; l = l.cdr; }    
    return v;
};

var vector$Mn$Gtlist = v => {
    var l = __nil;
    for(var i=v.length-1; i>=0; i--) { l = cons(v[i],l); }
    return l;
};

var vector$Mnlength = v => v.length;

var vector$Mnref = (v,k) => v[k];

var vector$Mnset$Ex = (v,k,o) => v[k]=o;

var vector$Mnfill$Ex = (v,f) => v.fill(f);
