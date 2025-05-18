var number$Qu = x => typeof(x) == 'number';

var number$Mn$Gtstring = (e, radix=10) => {
    if (isFinite(e)) return e.toString(radix);
    if (e > 0) return "+inf.0";
    if (e < 0) return "-inf.0";
    return "+nan.0";
};

var string$Mn$Gtnumber = (s, radix=10) => {
    if (s == "+inf.0") return Infinity;
    if (s == "-inf.0") return -Infinity;
    if (s == "+nan.0") return NaN;
    if (s.indexOf('.') >= 0) {
	assert(radix == 10);
	return parseFloat(s);
    }
    return parseInt(s, radix);
};

var finite$Qu = x => x!=Infinity && x!=-Infinity && x!=NaN;

