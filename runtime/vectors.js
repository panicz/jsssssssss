/////////////////////////////////////////////////////////////////////
// VECTORS

const __list2vector = l => {
    if(!__is_list(l)) throw "list->vector wrong type argument";
    var ll = length(l);
    var v = Array(ll);
    for(var i=0;i<ll;i++) { v[i] = l.car; l = l.cdr; }
    return v;
};

const __vector2list = v => {
    var l = __nil;
    for(var i=v.length-1; i>=0; i--) l = {car: v[i], cdr: l};
    return l;
};

/////////////////////////////////////////////////////////////////////

var vector$Qu = __is_vector;

var make$Mnvector = (k,f) => { return Array(k).fill(f); };
var vector = (...xs) => {return xs; };

var list$Mn$Gtvector = __list2vector;
var vector$Mn$Gtlist = __vector2list;

var vector$Mnlength = v => v.length;

var vector$Mnref = (v,k) => v[k];

var vector$Mnset$Ex = (v,k,o) => v[k]=o;

var vector$Mnfill$Ex = (v,f) => v.fill(f);
