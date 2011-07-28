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

/* 4.1.2: (quote <datum>) evaluates to <datum>.
 (quote <datum>) may be abbreviated as '<datum>.
 The two notations are equivalent in all respects. */
_eval['quotation'] = function(tree, env) {
    return externalRepresentation(tree['datum']);
};

// todo bl investigate doing this in the parser
function externalRepresentation(datum) {
    var node;
    var ans = [];
    if (node = datum['simple-datum']) {
        return node['text'];
    } else if (node = datum['compound-datum']) {

        if (node = node['list']) {

            if (node['abbreviation']) {

                console.log(node);

                throw new Error("todo bl!");

            } else {

                for (var i = 0; i < node['datum'].length; ++i)
                    ans.push(externalRepresentation(node['datum'][i]));
                if (node['.datum']) {
                    ans.push('.');
                    ans.push(externalRepresentation(node['.datum']));
                }
                return '(' + ans.join(' ') + ')';
            }


        } else if (node = datum['compound-datum']['vector']) {
            for (var i = 0; i < node['datum'].length; ++i)
                ans.push(externalRepresentation(node['datum'][i]));
            return '#(' + ans.join(' ') + ')';
        }

        else throw new Error('compound-datum that is neither list nor vector, ' +
                'should never happen -- indicates logical bug in parser', datum['compound-datum']);

    } else throw new Error('datum that is neither simple-datum nor compound-datum, '
        + 'should never happen -- indicates logical bug in parser', datum);
}