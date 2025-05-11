var vector$Qu = x => typeof(x) == 'object'
    && typeof(x.vector) == 'object'
    && Array.isArray(x.vector);

var make$Mnvector = (k,f) => { return {vector: Array(k).fill(f)} }

var vector = (...xs) => {return {vector: xs} }

var list$Mn$Gtvector = l => {
    if(Array.isArray(l)) return {vector: l};
    else throw "list->vector wrong type argument"
}
var vector$Mn$Gtlist = v => v.vector;

var vector$Mnlength = v => v.vector.length;

var vector$Mnref = (v,k) => v.vector[k];

var vector$Mnset$Ex = (v,k,o) => v.vector[k]=o;

var vector$Mnfill$Ex = (v,f) => v.vector.fill(f);
