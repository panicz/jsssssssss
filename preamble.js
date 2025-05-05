var mk_seq_rel = rel => (...xs) => {
  for(var i = 1; i < xs.length; ++i) {
      if(!rel(xs[i-1], xs[i])) return false;
  }
  return true;
}

var cons = (h,t) => Array.isArray(t) ? [h].concat(t) : {car: h, cdr: t};
var car = p => Array.isArray(p) ? p[0] : p.car;
var cdr = p => Array.isArray(p) ? p.slice(1) : p.cdr;
var null$Qu = x => Array.isArray(x) && !x.length;
var boolean$Qu = x => typeof(x) == 'boolean';
var number$Qu = x => typeof(x) == 'number';
var string$Qu = x => typeof(x) == 'string';
var symbol$Qu = x => typeof(x) == 'object' && typeof(x.symbol) == 'string';
var pair$Qu = x => typeof(x) == 'object' &&
    ((Array.isArray(x) && x.length>0)
     || (typeof(x.car) != 'undefined'
	 && typeof(x.cdr) != 'undefined'));
var procedure$Qu = x => typeof(x) == 'function';

var input$Mnport$Qu = x => typeof(x) == 'object'
    && typeof(x.readChar) == 'procedure'
    && typeof(x.peekChar) == 'procedure'
    && typeof(x.charReady) == 'procedure'
    && typeof(x.close) == 'procedure';

var output$Mnport$Qu = x => typeof(x) == 'object'
    && typeof(x.writeChar) == 'procedure'
    && typeof(x.writeString) == 'procedure'
    && typeof(x.close) == 'procedure';

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

var serialize = e => {
  switch(true) {
  case null$Qu(e): return "()";
  case boolean$Qu(e): return e ? "#t" : "#f";
  case number$Qu(e): return "" + e;
  case symbol$Qu(e): return symbol$Mn$Gtstring(e);
  case pair$Qu(e):
      if(Array.isArray(e)) return "(" + e.map(serialize).join(" ") + ")";
      return "(" + serialize(e.car) + " . "
          + serialize(e.cdr) + ")";
  case procedure$Qu(e): return "#<procedure>";
  default: return "#<something strange>";
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

var current$Mninput$Mnport = make$Mnparameter("not implemented yet");
var current$Mnoutput$Mnport = make$Mnparameter("not implemented yet");

var read$Mnchar = (p = current$Mninput$Mnport()) => p.readChar();
var peek$Mnchar = (p = current$Mninput$Mnport()) => p.peekChar();
var char$Mnready$Qu = (p = current$Mninput$Mnport()) => p.charReady();
var close$Mninput$Mnport = p => p.close();

var write$Mnchar = (c, p = current$Mnoutput$Mnport()) => p.writeChar(c)
var close$Mnoutput$Mnport = p => p.close();
