/////////////////////////////////////////////////////////////////////
// PARAMETERS

const __make_parameter = (init) => {
    var s = [init];
    var axs=(...a)=>(a.length==0)?s[s.length-1]:s[s.length-1]=a[0];
    axs.stack = s;
    return axs;
};

const __push_parameter = (param, value) => param.stack.push(value);

const __pop_parameter = (param) => param.stack.pop();

/////////////////////////////////////////////////////////////////////

var make$Mnparameter = __make_parameter;
var push$Mnparameter = __push_parameter;
var pop$Mnparameter = __pop_parameter;
