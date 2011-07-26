function SemanticError(msg) {
    this.msg = msg;
}

function onlyChild(tree) {
    // bl: could break depending on enumerable/nonenumerable hash props
    for (var kid in tree)
        return tree[kid];
}


var _eval = {};



_eval['expression'] = function(tree, env) {
    var child = onlyChild(tree);
    return _eval[child.type](child, env);
};

/* 4.1.1: The value of the variable reference is the value stored in the location
    to which the variable is bound. It is an error to reference an unbound variable. */
_eval['variable'] = function(tree, env) {
    var maybeAns = env[tree['identifier']];
    if (maybeAns !== undefined)
        return maybeAns;
    else throw new SemanticError('unbound variable ' + tree['identifier']);
};