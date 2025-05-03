var mk_seq_rel = rel => (...xs) => {
  for(var i = 1; i < xs.length; ++i) {
    if(!rel(xs[i-1], xs[i])) return false
  }
  return true
}

var internal = {
  cons: (h,t) => Array.isArray(t) ? [h].concat(t) : {car: h, cdr: t},
  car: p => Array.isArray(p) ? p[0] : p.car,
  cdr: p => Array.isArray(p) ? p.slice(1) : p.cdr,
  is_null: x => Array.isArray(x) && !x.length,
  is_boolean: x => typeof(x) == 'boolean',
  is_number: x => typeof(x) == 'number',
  is_symbol: x => typeof(x) == 'object' && typeof(x.symbol) == 'string',
  is_pair: x => typeof(x) == 'object' &&
                ((Array.isArray(x) && x.length>0) ||
                 (typeof(x.car) != 'undefined' &&
                  typeof(x.cdr) != 'undefined')),
  is_procedure: x => typeof(x) == 'function',
  append: (...xs) => xs.length == 0 ? [] : xs[0].concat(...xs.slice(1)),
  list: (...xs) => xs,
  add: (...xs) => xs.reduce((n,m) => n+m, 0),
  sub: (...xs) => xs.length>1 ? xs.reduce((n,m) => n-m) : -xs[0],
  mul: (...xs) => xs.reduce((n,m) => n*m, 1),
  div: (...xs) => xs.length>1 ? xs.reduce((n,m) => n/m, 1) : 1/xs[0],
  not: x => !x,
  gt: mk_seq_rel((n,m) => n > m),
  lt: mk_seq_rel((n,m) => n < m),
  gteq: mk_seq_rel((n,m) => n >= m),
  lteq: mk_seq_rel((n,m) => n <= m),
  app: (f, ...args) => {
    var collected = args.slice(0, args.length-1)
                        .concat(args[args.length-1]);
    return f.apply(null, collected);
  }
}

internal.eqv = mk_seq_rel(
  (a, b) =>
    (internal.is_null(a) && internal.is_null(b)) ||
    (internal.is_symbol(a) && internal.is_symbol(b)
     && a.symbol == b.symbol) ||
    (internal.is_boolean(a) && internal.is_boolean(b) && a == b) ||
    (internal.is_number(a) && internal.is_number(b) && a == b) ||
    a === b
)

internal.sym2str = s => s.symbol.slice(3) /// a Provisional Solution (tm)
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
                                .replace(/[$]Nm/g, "#")
                                .replace(/[$]Dt/g, ".")

internal.serialize = e => {
  switch(true) {
    case internal.is_null(e): return "()"
    case internal.is_boolean(e): return e ? "#t" : "#f"
    case internal.is_number(e): return "" + e
    case internal.is_symbol(e): return internal.sym2str(e)
    case internal.is_pair(e):
      if(Array.isArray(e)) return "(" + e.map(internal.serialize).join(" ") + ")"
      return "(" + internal.serialize(e.car) + " . "
                 + internal.serialize(e.cdr) + ")"
    case internal.is_procedure(e): return "#<procedure>"
    default: return "#<something strange>"
  }
}

internal.console_log = e => { console.log(internal.serialize(e)) ; return e }

var s__cons = internal.cons
var s__car = internal.car
var s__cdr = internal.cdr
var s__null$Qu = internal.is_null
var s__boolean$Qu = internal.is_boolean
var s__number$Qu = internal.is_number
var s__symbol$Qu = internal.is_symbol
var s__pair$Qu = internal.is_pair
var s__procedure$Qu = internal.is_procedure
var s__eqv$Qu = internal.eqv
var s__eq$Qu = internal.eqv /// sic!
var s__$Eq = s__eq$Qu
var s__$Pl = internal.add
var s__$Mn = internal.sub
var s__$St = internal.mul
var s__$Sl = internal.div
var s__not = internal.not
var s__$Gt = internal.gt
var s__$Ls = internal.lt
var s__$Gt$Eq = internal.gteq
var s__$Ls$Eq = internal.lteq
var s__apply = internal.app
var s__append = internal.append
var s__list = internal.list
var s__console$Dtlog = internal.console_log
