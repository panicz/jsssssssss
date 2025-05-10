var make$Mnparameter = (init) => {
    var s = [init];
    var axs=(...a)=>(a.length==0)?s[s.length-1]:s[s.length-1]=a[0];
    axs.stack = s;
    return axs;
};

var push$Mnparameter = (param, value) => param.stack.push(value);

var pop$Mnparameter = (param) => param.stack.pop();
