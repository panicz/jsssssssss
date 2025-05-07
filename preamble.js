var mk_seq_rel = rel => (...xs) => {
    for(var i = 1; i < xs.length; ++i) {
	if(!rel(xs[i-1], xs[i])) return false;
    }
    return true;
};

var cons = (h,t) => Array.isArray(t) ? [h].concat(t) : {car: h, cdr: t};

var car = p => Array.isArray(p) ? p[0] : p.car;

var cdr = p => Array.isArray(p) ? p.slice(1) : p.cdr;

var null$Qu = x => Array.isArray(x) && !x.length;

var boolean$Qu = x => typeof(x) == 'boolean';

var number$Qu = x => typeof(x) == 'number';

var char$Qu = x => typeof(x) == 'object'
    && typeof(x.char) == 'string'
    && x.char.length == 1;

var string$Qu = x => typeof(x) == 'string';

var symbol$Qu = x => typeof(x) == 'object'
    && typeof(x.symbol) == 'string';

var pair$Qu = x => typeof(x) == 'object'
    && ((Array.isArray(x) && x.length>0)
	|| (typeof(x.car) != 'undefined'
	    && typeof(x.cdr) != 'undefined'));

var procedure$Qu = x => typeof(x) == 'function';

var input$Mnport$Qu = x => typeof(x) == 'object'
    && typeof(x.readChar) == 'procedure'
    && typeof(x.peekChar) == 'procedure'
    && typeof(x.charReady) == 'procedure';

var output$Mnport$Qu = x => typeof(x) == 'object'
    && typeof(x.writeChar) == 'procedure'
    && typeof(x.writeString) == 'procedure';

const __EOF = {char: false};

var eof$Mnobject$Qu = x => x === __EOF;

var eqv$Qu = mk_seq_rel(
    (a, b) =>
    (null$Qu(a) && null$Qu(b))
	|| (symbol$Qu(a) && symbol$Qu(b)
	    && a.symbol == b.symbol)
	|| (boolean$Qu(a) && boolean$Qu(b) && a == b)
	|| (number$Qu(a) && number$Qu(b) && a == b)
	|| a === b
);

var eq$Qu = eqv$Qu;

var $Eq = eq$Qu;

var $Pl = (...xs) => xs.reduce((n,m) => n+m, 0);

var $Mn = (...xs) => xs.length>1 ? xs.reduce((n,m) => n-m) : -xs[0];

var $St = (...xs) => xs.reduce((n,m) => n*m, 1);

var $Sl = (...xs) => xs.length>1 ? xs.reduce((n,m) => n/m, 1) : 1/xs[0];

var not = x => !x;

var $Gt = mk_seq_rel((n,m) => n > m);

var $Ls = mk_seq_rel((n,m) => n < m);

var $Gt$Eq = mk_seq_rel((n,m) => n >= m);

var $Ls$Eq = mk_seq_rel((n,m) => n <= m);

var apply = (f, ...args) => {
    var collected = args.slice(0, args.length-1)
        .concat(args[args.length-1]);
    return f.apply(null, collected);
};

var append = (...xs) => xs.length == 0 ? [] : xs[0].concat(...xs.slice(1));

var list = (...xs) => xs;

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

var list$Mn$Gtstring = s => s.map(c => c.char).join('');

var string$Mn$Gtlist = s => s.map((c) => {char: c});

var string$Mnappend = (...args) => args.join('');

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

var serialize = e => {
    switch(true) {
    case null$Qu(e): return "()";
    case boolean$Qu(e): return e ? "#t" : "#f";
    case number$Qu(e): return "" + e;
    case char$Qu(e): return "#\\"+charName(e);
    case symbol$Qu(e): return symbol$Mn$Gtstring(e);
    case pair$Qu(e):
	if(Array.isArray(e))
	    return "("+e.map(serialize).join(" ")+")";
	return "(" + serialize(e.car) + " . "
            + serialize(e.cdr) + ")";
    case procedure$Qu(e): return "#<procedure>";
    case input$Mnport$Qu(e): return "#<input-port>";
    case output$Mnport$Qu(e): return "#<output-port>";
    default: return "#<"+typeof(e)+">";
    }
};

var writeln = e => { console.log(serialize(e)) ; return e };

var make$Mnparameter = (init) => {
    var stack = [init];
    var accessor = (...args) => {
	if (args.length == 0) {
            return stack[stack.length-1];
	} else {
            stack[stack.length-1] = args[0];
	}
    };
    accessor.stack = stack;
    return accessor;
};

var push$Mnparameter = (parameter, value) => parameter.stack.push(value);

var pop$Mnparameter = (parameter) => parameter.stack.pop();

class InputStringPort {
    constructor(string) {
	this.string = string;
	this.tip = 0;
    }

    readChar() {
	if (this.tip >= this.string.length) {
	    return __EOF;
	}
	return {char: this.string[this.tip++]};
    }
    
    peekChar() {
	return {char: string[tip]};
    }
    
    charReady() {
	return tip < string.length;
    }
};

class OutputStringPort {
    constructor() {
	this.string = "";
    }
    writeChar(c) {
	this.string += c.char;
    }
    writeString(s) {
	this.string += s;
    }
};

let fs = (typeof(require) == 'function')
    ? require('fs')
    : {
	readSync: (fd, buffer) => { buffer[0]='?'; },
	writeSync: (fd, string) => {console.log(string);},
	closeSync: (fd) => {},
    };

class InputFilePort {
    constructor(fd) {
	this.fd = fd;
	this.buffer = (typeof(Buffer) == 'undefined')
	    ? []
	    : Buffer.alloc(1);
	this.charsUnread = [];
    }
    
    readChar() {
	if (this.charsUnread.length > 0) {
	    return this.buffer.pop();
	}
	
	let bytesRead = fs.readSync(this.fd, this.buffer);
	if (bytesRead < 1) {
	    return __EOF;
	}
	return {char: this.buffer.toString('utf8')};
    }

    unreadChar(c) {
	this.buffer.push(c);
    }
    
    peekChar() {
	let c = this.readChar()
	this.unreadChar(c);
	return c;
    }
    
    charReady() {
	if (this.charsUnread.length > 0) {
	    return true;
	}
	return; // TODO przemyslenia
    }

    close() {
	return fs.closeSync(this.fd);
    }

};

class OutputFilePort {
    constructor(fd) {
	this.fd = fd;
    }
    writeChar(c) {
	return fs.writeSync(this.fd, c.char);
    }
    writeString(s) {
	return fs.writeSync(this.fd, s);
    }
    close() {
	return fs.closeSync(this.fd);
    }
};

let stdin = new InputFilePort(0);

let stdout = new OutputFilePort(1);

let stderr = new OutputFilePort(2);

var current$Mninput$Mnport = make$Mnparameter(stdin);

var current$Mnoutput$Mnport = make$Mnparameter(stdout);

var current$Mnerror$Mnport = make$Mnparameter(stderr);

var call$Mnwith$Mninput$Mnstring = (string, f) => {
    return f(new InputStringPort(string));
};

var call$Mnwith$Mnoutput$Mnstring = (f) => {
    let p = new OutputStringPort();
    f(p);
    return p.string;
};

var with$Mninput$Mnfrom$Mnstring = (string, f) => {
    let p = new InputStringPort(string);
    push$Mnparameter(current$Mninput$Mnport, p);
    try {
	var result = f();
    }
    finally {
	pop$Mnparameter(current$Mninput$Mnport);
    }
    return result;
};

var with$Mnoutput$Mnto$Mnstring = (f) => {
    let p = new OutputStringPort();
    push$Mnparameter(current$Mnoutput$Mnport, p);
    try {
	f();
    }
    finally {
	pop$Mnparameter(current$Mnoutput$Mnport);
    }
    return p.string;
};

var read$Mnchar = (p = current$Mninput$Mnport()) => p.readChar();

var peek$Mnchar = (p = current$Mninput$Mnport()) => p.peekChar();

var char$Mnready$Qu = (p = current$Mninput$Mnport()) => p.charReady();

var close$Mninput$Mnport = p => p.close();

var write$Mnchar = (c, p = current$Mnoutput$Mnport()) => p.writeChar(c);

var write$Mnstring = (s, p = current$Mnoutput$Mnport()) => p.writeString(s);

var newline = (p = current$Mnoutput$Mnport()) => p.writeChar({char: '\n'});

var close$Mnoutput$Mnport = p => p.close();
