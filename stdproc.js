function requireArgTypes(primitiveName, args, type) {

    var ordinalNames = {
        0: 'first',
        1: 'second',
        2: 'third',
        3: 'fourth',
        4: 'fifth',
        5: 'sixth',
        6: 'seventh',
        7: 'eighth',
        8: 'ninth',
        9: 'tenth'
    };

    for (var i = 0; i < args.length; ++i)
        if (!builtins[type + '?'](args[i]))
            throw new SemanticError('The object '
                + args[i]
                + ', passed as the '
                + (ordinalNames[i] || (i + 1) + 'th')
                + ' argument to '
                + primitiveName
                + ', is not the correct type '
                + type);
}

function requireNumArgs(primitiveName, actual, atLeast, isExact) {
    if (actual.length < atLeast) {
        throw new SemanticError('The procedure '
            + primitiveName
            + ' has been called with '
            + actual.length
            + 'arguments; it requires at least '
            + atLeast
            + ' arguments.')
    }

    else if (isExact && actual.length !== atLeast) {
        throw new SemanticError('The procedure '
            + primitiveName
            + ' has been called with '
            + actual.length
            + 'arguments; it requires exactly '
            + atLeast
            + ' arguments.')
    }
}


builtins = {};

builtins['boolean?'] = function(x) {
    requireNumArgs('boolean?', arguments, 1, true);
    return typeof x === 'boolean';
};
builtins['string?'] = function(x) {
    requireNumArgs('string?', arguments, 1, true);
    return typeof x === 'string';
};
builtins['number?'] = function(x) {
    requireNumArgs('number?', arguments, 1, true);
    return typeof x === 'number';
};
builtins['vector?'] = function(x) {
    requireNumArgs('vector?', arguments, 1, true);
    return x instanceof Array;
};
// JavaScript doesn't distinguish between chars and length-one strings, but Scheme does
builtins['char?'] = function(x) {
    requireNumArgs('char?', arguments, 1, true);
    return x.type === 'char';
};

builtins['symbol?'] = function(x) {
    requireNumArgs('symbol?', arguments, 1, true);
    return x.type === 'symbol';
};
/* Primitive procedures are represented by JavaScript functions;
 other procedures are represented by JavaScript hashes with type 'procedure'. */
builtins['procedure?'] = function(x) {
    requireNumArgs('procedure?', arguments, 1, true);
    return typeof x === 'function' || x.type === 'procedure';
};
builtins['pair?'] = function(x) {
    requireNumArgs('pair?', arguments, 1, true);
    return x.type === 'pair';
};
builtins['port?'] = function(x) {
    requireNumArgs('port?', arguments, 1, true);
    return x.type === 'port';
};

builtins['complex?'] = function(x) {
    requireNumArgs('complex?', arguments, 1, true);
    return false;
}; // Not required to support this. See 6.2.3.
builtins['real?'] = function(x) {
    requireNumArgs('real?', arguments, 1, true);
    return typeof x === 'number';
};
builtins['rational?'] = function(x) {
    requireNumArgs('rational?', arguments, 1, true);
    return typeof x === 'number';
};
builtins['integer?'] = function(x) {
    requireNumArgs('integer?', arguments, 1, true);
    return typeof x === 'number' && Math.round(x) === x;
};
builtins['exact?'] = function(x) {
    requireNumArgs('exact?', arguments, 1, true);
    return false;
}; // In JavaScript every number is a float.
builtins['inexact?'] = function(x) {
    requireNumArgs('inexact?', arguments, 1, true);
    return typeof x === 'number';
};

builtins['='] = function() {
    requireArgTypes('=', arguments, 'number');

    for (var i = 0; i < arguments.length - 1; ++i)
        if (arguments[i] !== arguments[i + 1])
            return false;
    return true;
};

builtins['<'] = function() {
    requireArgTypes('<', arguments, 'number');

    for (var i = 0; i < arguments.length - 1; ++i)
        if (arguments[i] >= arguments[i + 1])
            return false;
    return true;

};

builtins['<='] = function() {
    requireArgTypes('<=', arguments, 'number');

    for (var i = 0; i < arguments.length - 1; ++i)
        if (arguments[i] > arguments[i + 1])
            return false;
    return true;

};

builtins['>'] = function() {
    requireArgTypes('>', arguments, 'number');

    for (var i = 0; i < arguments.length - 1; ++i)
        if (arguments[i] <= arguments[i + 1])
            return false;
    return true;

};

builtins['>='] = function() {
    requireArgTypes('>=', arguments, 'number');

    for (var i = 0; i < arguments.length - 1; ++i)
        if (arguments[i] < arguments[i + 1])
            return false;
    return true;
};

builtins['+'] = function() {
    requireArgTypes('+', arguments, 'number');

    var sum = 0;
    for (var i = 0; i < arguments.length; ++i)
        sum += arguments[i];
    return sum;
};

builtins['*'] = function() {
    requireArgTypes('*', arguments, 'number');

    var product = 1;
    for (var i = 0; i < arguments.length; ++i)
        product *= arguments[i];
    return product;
};

builtins['-'] = function() {
    requireArgTypes('-', arguments, 'number');
    requireNumArgs('-', arguments, 1);

    // unary
    if (arguments.length === 1)
        return -1 * arguments[0];

    // varargs: (x1 - x2) - x3 etc
    else {
        var ans = arguments[0];
        for (var i = 1; i < arguments.length; ++i)
            ans -= arguments[i];
        return ans;
    }
};

builtins['/'] = function() {
    requireArgTypes('/', arguments, 'number');
    requireNumArgs('/', arguments, 1);

    // unary
    if (arguments.length === 1)
        return 1 / arguments[0];

    // varargs: (x1 / x2) / x3 etc
    else {
        var ans = arguments[0];
        for (var i = 1; i < arguments.length; ++i)
            ans /= arguments[i];
        return ans;
    }
};

builtins['remainder'] = function(p, q) {
    requireArgTypes('remainder', arguments, 'number');
    requireNumArgs('remainder', arguments, 2, true);
    return p % q;
};

builtins['modulo'] = builtins['remainder']; // todo bl this isn't quite right

builtins['quotient'] = function(p, q) {
    requireArgTypes('remainder', arguments, 'number');
    requireNumArgs('remainder', arguments, 2, true);

    return Math.round(p / q);
}; // todo bl not sure this is correct





