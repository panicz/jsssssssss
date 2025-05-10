var input$Mnport$Qu = x => typeof(x) == 'object'
    && typeof(x.readChar) == 'procedure'
    && typeof(x.peekChar) == 'procedure'
    && typeof(x.charReady) == 'procedure';

var output$Mnport$Qu = x => typeof(x) == 'object'
    && typeof(x.writeChar) == 'procedure'
    && typeof(x.writeString) == 'procedure';

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
	if (this.tip >= this.string.length) {
	    return __EOF;
	}
	return {char: this.string[this.tip]};
    }

    charReady() {
	return this.tip < this.string.length;
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
	return f();
    }
    finally {
	pop$Mnparameter(current$Mninput$Mnport);
    }
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
