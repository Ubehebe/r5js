function SchemeMacro(keyword, transformerSpec) {
    this.keyword = keyword;
    this.literals = transformerSpec['transformer-spec-identifier'].map(function(x) {
        return x.identifier;
    });
    this.rules = [];

    var syntaxRules = transformerSpec['syntax-rule'];
    var syntaxRule;
    var pattern;
    for (var i = 0; i < syntaxRules.length; ++i) {
        syntaxRule = syntaxRules[i];
        pattern = syntaxRule.pattern;
        if (!(pattern instanceof Array)
            || !pattern[0]['pattern-identifier']
            || pattern[0]['pattern-identifier'].identifier !== keyword)
            throw new MacroError(keyword, 'each rule pattern must have the form ('
                + keyword + '...)');
        else
            this.rules.push();
    }
}

function SyntaxPattern(tree) {
    this.tree = tree;
}

/* 4.3.2: An input form F matches a pattern P if and only if:
 (1) P is a non-literal identifier; or
 (2) P is a literal identifier and F is an identifier with the same binding; or
 (3) P is a list (P1 ... Pn) and F is a list of n forms that match P1 through Pn, respectively; or
 (4) P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or improper list of n or more forms
 that match P1 through Pn, respectively, and whose nth “cdr” matches Pn+1; or
 (5) P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is the identifier ...
 and F is a proper list of at least n forms, the first n of which match P1 through Pn,
 respectively, and each remaining element of F matches Pn+1; or
 (6) P is a vector of the form #(P1 ...Pn) and F is a vector of n forms that match P1 through Pn; or
 (7) P is of the form #(P1 . . . Pn Pn+1 <ellipsis>) where <ellipsis> is the identifier ...
 and F is a vector of n or more forms the first n of which match P1 through Pn,
 respectively, and each remaining element of F matches Pn+1; or
 (8) P is a datum and F is equal to P in the sense of the equal? procedure.
 */
SyntaxPattern.prototype.matches = function(datum) {

    // rule 1
    if (this.tree['pattern-identifier'] /* todo bl: && is literal */)
        return true;

    // rule 2
    else if (this.tree['pattern-identifier'] /* todo bl: && is not literal */
        && datum['simple-datum']
        && datum['simple-datum'].text === this.tree['pattern-identifier'].identifier)
        return true;

    // rule 3
    else if (this.tree['pattern'] instanceof Array
        && datum.list
        && datum.list.datum.length == this.tree['pattern'].length) {
        var patterns = this.tree['pattern'];
        var forms = datum.list.datum;
        for (var i=0; i<patterns.length; ++i)
            if (!patterns[i].matches(forms[i]))
                return false;
        return true;
    }




    // ...
    else return false;

};
