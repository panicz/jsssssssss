/////////////////////////////////////////////////////////////////////
// NUMBERS

const __number2string = (e, radix=10) => {
    if (isFinite(e)) return e.toString(radix);
    if (e > 0) return "+inf.0";
    if (e < 0) return "-inf.0";
    return "+nan.0";
};

const __string2number = (s, radix=10) => {
    if (s == "+inf.0") return Infinity;
    if (s == "-inf.0") return -Infinity;
    if (s == "+nan.0") return NaN;
    if (s.indexOf('.') >= 0) {
	assert(radix == 10);
	return parseFloat(s);
    }
    return parseInt(s, radix);
};

/////////////////////////////////////////////////////////////////////

var number$Qu = __is_number;

var finite$Qu = isFinite;
var nan$Qu = isNaN;

var even$Qu = n => n%2==0;
var odd$Qu = n => n%2==1;

var number$Mn$Gtstring = __number2string;
var string$Mn$Gtnumber = __string2number;
