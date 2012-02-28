function UnboundVariable(name) {
    this.toString = function() {
        return 'unbound variable ' + name;
    };
}

function TooFewArgs(name, minNumArgs, actualNumArgs) {
    this.toString = function() {
        return 'The procedure '
            + name
            + ' has been called with '
            + actualNumArgs
            + ' argument'
            + (actualNumArgs === 1 ? '' : 's')
            + '; it requires at least '
            + minNumArgs
            + ' argument'
            + (minNumArgs === 1 ? '' : 's');
    };
}

function TooManyArgs(name, maxNumArgs, actualNumArgs) {
    this.toString = function() {
        return 'The procedure '
            + name
            + ' has been called with '
            + actualNumArgs
            + ' argument'
            + (actualNumArgs === 1 ? '' : 's')
            + '; it requires at most '
            + maxNumArgs
            + ' argument'
            + (maxNumArgs === 1 ? '' : 's');
    };
}

function IncorrectNumArgs(name, expectedNumArgs, actualNumArgs) {
    this.toString = function() {
        return 'The procedure '
            + name
            + ' has been called with '
            + actualNumArgs
            + ' argument'
            + (actualNumArgs === 1 ? '' : 's')
            + '; it requires exactly '
            + expectedNumArgs
            + ' argument'
            + (expectedNumArgs === 1 ? '' : 's');
    };
}

function InternalInterpreterError(msg) {
    this.toString = function() {
        return msg;
    };
}

function PrimitiveProcedureError(message) {
    this.toString = function() { return message; };
}

function ArgumentTypeError(argument, which, procName, expectedType) {
    this.toString = function() {
        return 'The object '
            + argument.toString()
            + ', passed as argument '
            + which
            + ' to '
            + procName
            + ', is not of the correct type '
            + expectedType;
    };
}

function MacroError(keyword, msg) {
    this.toString = function() {
        return 'Error in macro '
        + keyword + ': ' + msg;
    };
}

function UnimplementedOptionError(what) {
    this.toString = function() {
        return 'Sorry, '
            + what
            + ' is optional according to R5RS and unimplemented';
    }
}

function GeneralSyntaxError(what) {
    this.toString = function() {
        return 'bad syntax in ' + what;
    };
}

function QuasiquoteError(what) {
    this.toString = function() {
        return 'quasiquote error: ' + what;
    };
}

function IllegalEmptyApplication(where) {
    this.toString = function() {
        return 'illegal empty application in ' + where;
    };
}

function ParseError(what) {
    this.toString = function() {
        return 'parse error on ' + what;
    };
}

function EvalError(what) {
    this.toString = function() {
        return 'evaluation error: ' + what;
    };
}