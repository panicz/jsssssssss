/////////////////////////////////////////////////////////////////////
// STRINGS

const __EOF = {char: false};
const __is_eof_object = x => (x === __EOF);

const __symbol2string = s => {
    if (typeof(s.symbol) == 'undefined') {
	throw new Error("not a symbol: "+s);
    }
    return s.symbol
	.replace(/^[$]Kw/, "")
	.replace(/^[$]([0-9])/, "$1")
	.replace(/[$]Pl/g, "+")
	.replace(/[$]Mn/g, "-")
	.replace(/[$]St/g, "*")
	.replace(/[$]Sl/g, "/")
	.replace(/[$]Ls/g, "<")
	.replace(/[$]Gt/g, ">")
	.replace(/[$]Eq/g, "=")
	.replace(/[$]Ex/g, "!")
	.replace(/[$]Pc/g, "%")
	.replace(/[$]Qu/g, "?")
	.replace(/[$]At/g, "@")
	.replace(/[$]Tl/g, "~")
	.replace(/[$]Dt/g, ".")
	.replace(/[$]Am/g, "&")
	.replace(/[$]Nm/g, "#");
};

const __string2symbol = s => {
    var name = (s == "for")
	? "$Kwfor"
	: (s.replace(/^([0-9])/, "$$N$1")
	   .replace(/[+]/g, "$Pl")
	   .replace(/[-]/g, "$Mn")
	   .replace(/[*]/g, "$St")
	   .replace(/[/]/g, "$Sl")
	   .replace(/[<]/g, "$Ls")
	   .replace(/[>]/g, "$Gt")
	   .replace(/[=]/g, "$Eq")
	   .replace(/[!]/g, "$Ex")
	   .replace(/[%]/g, "$Pc")
	   .replace(/[?]/g, "$Qu")
	   .replace(/[@]/g, "$At")
	   .replace(/[~]/g, "$Tl")
	   .replace(/[.]/g, "$Dt")
	   .replace(/[&]/g, "$Am")
	   .replace(/[#]/g, "$Nm"));
    return { symbol: name };
};

/////////////////////////////////////////////////////////////////////

var char$Qu = __is_char;
var string$Qu = __is_string;
var symbol$Qu = __is_symbol;
var eof$Mnobject$Qu = __is_eof_object;

var symbol$Mn$Gtstring = __symbol2string;
var string$Mn$Gtsymbol = __string2symbol;

var list$Mn$Gtstring = s => __list2vector(s).map(c => c.char).join('');
var string$Mn$Gtlist = s => __vector2list(s.split('').map(c =>{ return {char: c} }));

var string$Mnappend = (...args) => args.join('');

var string$Mnjoin = (strings, joint='') => {
    res = "";
    while(__is_pair(strings)) {
        if(!__is_string(strings.car)) throw new Error("string-join wrong type argument");
        res += strings.car;
        strings = strings.cdr;
        if(__is_nil(strings)) return res;
        if(!__is_pair(strings)) throw new Error("string-join wrong type argument");
        res += joint;
    }
};

var string$Mnref = (s, i) => { return {char: s[i]}; };

var string$Mnlength = s => s.length;

var char$Mn$Gtinteger = c => c.char.codePointAt(0);

var integer$Mn$Gtchar = i => {
    return {char: String.fromCodePoint(i)};
};

var string$Mntake = (s, n) => s.slice(0, n);

var string$Mndrop = (s, n) => s.slice(n);

var string$Mnmatch = (pat, s) => s.match(new RegExp(pat))||false;

var string$Mnreplace$Mnsubstring = (s, p, r) =>
    s.replace(new RegExp(p.replace(/[-\/\\^$.*+?()[\]{}|]/g, '\\$&'),
			 'g'), r);


var char$Mnnumeric$Qu = x => __is_char(x) && /[0-9]/.test(x.char);

var string$Eq$Qu = mk_seq_rel((x,y) => __is_string(x) && __is_string(y) && x===y);
