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

function contains(array, x) {
    for (var i = 0; i < array.length; ++i)
        if (array[i] === x)
            return true;
    return false;
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
function syntaxPatternMatches(pattern, datum, literals, definitionEnv, useEnv) {

    var patternIsId = pattern['pattern-identifier'];
    var patternIsLiteralId = patternIsId && contains(literals, patternIsId.identifier);
    var datumIsId = datum['simple-datum'];

    var patternIsList = pattern['pattern'] instanceof Array;
    var patternProperList = patternIsList && !pattern['.pattern'] && pattern['pattern'];
    var datumIsList = datum['compound-datum'] && datum['compound-datum'].list;
    var datumProperList = datumIsList && !datum['compound-datum'].list['.datum']
        && datum['compound-datum'].list.datum;

    // rule 1
    if (patternIsId && !patternIsLiteralId)
        return true;

    // rule 2
    /* 4.3.2: A subform in the input matches a literal identifier if and only if
     it is an identifier and either both its occurrence in the macro expression
     and its occurrence in the macro definition have the same lexical binding,
     or the two identifiers are equal and both have no lexical binding. */
    else if (patternIsLiteralId && datumIsId) {
        var definitionBinding = definitionEnv[patternIsId.identifier];
        var useBinding = useEnv[datumIsId.text];
        if (definitionBinding === undefined
            && useBinding === undefined
            && patternIsId.identifier === datumIsId.text)
            return true;
        else if (definitionBinding !== undefined
            && useBinding !== undefined
            && definitionBinding === useBinding)
            return true;
        else return false;
    }

    // rule 3
    else if (patternProperList && datumProperList) {
        if (patternProperList.length !== datumProperList.length)
            return false;
        for (var i = 0; i < patternProperList.length; ++i)
            if (!syntaxPatternMatches(patternProperList[i], datumProperList[i], literals, definitionEnv, useEnv))
                return false;
        return true;
    }

    else if (patternIsList && !patternProperList && datumIsList) {
        if (datum['compound-datum'].list.datum.length < pattern.pattern.length+1)
            return false;
        else if (datumProperList) {
            var patternImproperList = pattern['pattern'];
            for (var i=0; i<patternImproperList.length; ++i)
                if (!syntaxPatternMatches(patternImproperList[i], datumProperList[i], literals, definitionEnv, useEnv))
                    return false;
            // Ugh, artificially construct the nth cdr since our structures aren't recursive
            var datumNthCdr = 
        }
    }




    // ...
    else return false;

}
;
