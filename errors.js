function SemanticError(msg) {
    this.msg = msg;
}

function TooFewArgs(name, minNumArgs, actualNumArgs) {
    this.msg = 'The procedure '
        + name
        + ' has been called with '
        + actualNumArgs
        + ' argument'
        + (actualNumArgs === 1 ? '' : 's')
        + '; it requires at least '
        + minNumArgs
        + ' argument'
        + (minNumArgs === 1 ? '' : 's');
}

function TooManyArgs(name, maxNumArgs, actualNumArgs) {
    this.msg = 'The procedure '
        + name
        + ' has been called with '
        + actualNumArgs
        + ' argument'
        + (actualNumArgs === 1 ? '' : 's')
        + '; it requires at most '
        + maxNumArgs
        + ' argument'
        + (maxNumArgs === 1 ? '' : 's');
}

function IncorrectNumArgs(name, expectedNumArgs, actualNumArgs) {
    this.msg = 'The procedure '
        + name
        + ' has been called with '
        + actualNumArgs
        + ' argument'
        + (actualNumArgs === 1 ? '' : 's')
        + '; it requires exactly '
        + expectedNumArgs
        + ' argument'
        + (expectedNumArgs === 1 ? '' : 's');
}

function InternalInterpreterError(msg) {
    this.msg = msg;
}

function ArgumentTypeError(argument, which, procName, expectedType) {
    this.msg = 'The object '
        + arguments[i].toString()
        + ', passed as argument '
        + which
        + ' to '
        + procName
        + ', is not of the correct type '
        + expectedType;
}