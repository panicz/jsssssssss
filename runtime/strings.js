var char$Qu = x => typeof(x) == 'object'
    && typeof(x.char) == 'string'
    && x.char.length == 1;

var string$Qu = x => typeof(x) == 'string';

var symbol$Qu = x => typeof(x) == 'object'
    && typeof(x.symbol) == 'string';

const __EOF = {char: false};

var eof$Mnobject$Qu = x => x === __EOF;

var symbol$Mn$Gtstring = s => s.symbol.replace(/^[$]N([0-9])/, "$1")
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
    .replace(/[$]Nm/g, "#");

var string$Mn$Gtsymbol = s => { return {
    symbol: s.replace(/^([0-9])/, "$$N$1")
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
	.replace(/[#]/g, "$Nm")}; };

var list$Mn$Gtstring = s => s.map(c => c.char).join('');

var string$Mn$Gtlist = s => s.split('').map(c =>{return {char: c}});

var string$Mnappend = (...args) => args.join('');

var string$Mnjoin = (strings, joint='') => strings.join(joint);

let charName = c => {
    let i = c.char.codePointAt(0);
    if (i < 16) {
	return "x0"+i.toString(16);
    }
    if (i <= 32) {
	return "x"+i.toString(16);
    }
    return c.char;
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
