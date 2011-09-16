function SchemeMacro(keyword, transformerSpec, definitionEnv) {
    this.keyword = keyword;
    this.literals = transformerSpec['transformer-spec-identifier'].map(function(x) {
        return x.identifier;
    });
    this.definitionEnv = definitionEnv;
    this.patterns = [];
    this.templates = [];

    var syntaxRules = transformerSpec['syntax-rule'];
    var syntaxRule;
    var pattern;
    for (var i = 0; i < syntaxRules.length; ++i) {
        syntaxRule = syntaxRules[i];
        pattern = syntaxRule.pattern && syntaxRule.pattern.pattern;
        if (!(pattern instanceof Array)
            || !pattern[0]['pattern-identifier']
            || pattern[0]['pattern-identifier'].identifier !== keyword)
            throw new MacroError(keyword, 'each rule pattern must have the form ('
                + keyword + '...)');
        else {

            /* 4.3.2: The keyword at the beginning of the pattern in a <syntax rule>
             is not involved in the matching and is not considered a pattern variable
             or literal identifier. */
            pattern.shift();

            this.patterns.push(pattern);
            this.templates.push(syntaxRule.template);
        }
    }
}

function PatternMatch(replacementDatums, template) {
    this.replacementDatums = replacementDatums;
    this.template = template;
}

PatternMatch.prototype.hygienicTranscription = function() {
    return transformTemplate(this.template, this.replacementDatums);
};

SchemeMacro.prototype.matchInput = function(datum, useEnv) {

    var replacementDatums = {};

    for (var i = 0; i < this.patterns.length; ++i)
        if (findReplacementDatums(datum, this.patterns[i], this.literals, this.definitionEnv, useEnv, replacementDatums)) {
            console.log("found replacement datums: " + replacementDatums);
            return new PatternMatch(replacementDatums, deepCopy(this.templates[i]));
        }
    return null;

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
    function findReplacementDatums(datum, pattern, literals, definitionEnv, useEnv, ansDict) {

        console.log('findReplacementDatums: inspecting');
        console.log(datum);

        var patternIsId = pattern['pattern-identifier'];
        var patternIsLiteralId = patternIsId && contains(literals, patternIsId.identifier);
        var patternList = pattern.pattern instanceof Array && pattern.pattern;
        var datumList = datum.list && datum.list.datum;

        /* (1) P is a non-literal identifier
         Example: (let-syntax ((foo (syntax-rules () ((foo x) "aha!")))) (foo (1 2 3))) => "aha!"
         ((1 2 3) would be a procedure-call error if evaluated, but it is never evaluated.) */
        if (patternIsId && !patternIsLiteralId) {
            ansDict[patternIsId.identifier] = datum;
            return true;
        }

        /* (2) P is a literal identifier and F is an identifier with the same binding
         4.3.2: A subform in the input matches a literal identifier if and only if
         it is an identifier and either both its occurrence in the macro expression
         and its occurrence in the macro definition have the same lexical binding,
         or the two identifiers are equal and both have no lexical binding.

         Example:
         (let ((x 1)) (let-syntax ((foo (syntax-rules (x) ((foo x) "aha!")))) (foo x))) => "aha!"
         x has the same lexical binding in the macro expression and the macro definition

         (let-syntax ((foo (syntax-rules (x) ((foo x) "aha!")))) (foo x)) => "aha!"
         x has no lexical binding in either the macro expression or the macro definition

         todo bl: this part of the spec is really hard to understand. In particular,
         I do not understand how two non-equal identifiers can have the same binding.
         For example, in

         (let* ((x 1) (y x)) (foo x y))

         x and y do not have the same lexical binding in the procedure call, since this is rewritten as

         ((lambda (x) ((lambda (y) (foo x y)) x)) 1) */

        else if (patternIsLiteralId) {
            if (datum.identifier) {
                if ((datum.identifier === patternIsId.identifier
                    && definitionEnv[datum.identifier] === undefined
                    && useEnv[datum.identifier] === undefined)
                    || (definitionEnv[datum.identifier] === useEnv[datum.identifier])) {
                    ansDict[patternIsId.identifier] = datum;
                    return true;
                }
            }
            return false;
        }

        /* (3) P is a list (P1 ... Pn) and F is a list of n forms that match P1 through Pn, respectively
            Example: (let-syntax ((foo (syntax-rules () ((foo (x y z)) y)))) (foo (1 2 3))) => 2 */
        else if (patternList && !pattern['.pattern'] && datumList && !datum.list['.datum']) {
            console.log('patternList ' + patternList.length + ' datumList ' + datumList.length);
            if (patternList.length !== datumList.length)
                return false;
            for (var i = 0; i < patternList.length; ++i) {
                if (!findReplacementDatums(datumList[i], patternList[i], literals, definitionEnv, useEnv, ansDict))
                    return false;
            }
            return true;
        }

        // todo bl implementation inconsistencies!
        // (let-syntax ((foo (syntax-rules () ((foo (x y . z)) z)))) (foo (3 4)))
        // MIT Scheme => ()
        // Chez Scheme, PLT Scheme => error
         /* (4) P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or improper list
            of n or more forms that match P1 through Pn, respectively, and whose
            nth “cdr” matches Pn+1
            Example: 

        /*else if (patternList && !patternIsProperList && datumIsList) {
         if (datum['compound-datum'].list.datum.length < pattern.pattern.length + 1)
         return false;
         else if (datumProperList) {
         var patternImproperList = pattern['pattern'];
         for (var i = 0; i < patternImproperList.length; ++i)
         if (!transformDatumByPattern(patternImproperList[i], datumProperList[i], literals, definitionEnv, useEnv))
         return false;
         // Ugh, artificially construct the nth cdr since our structures aren't recursive
         }
         }

         // ...
         */
        else return false;
    }
};

function deepCopy(node) {
    var ans = {};
    for (var k in node)
        ans[k] = typeof node[k] === 'object' ? deepCopy(node[k]) : node[k];
    return ans;
}

/* 4.3.2: When a macro use is transcribed according to the template of
 the matching <syntax rule>, pattern variables that occur in the template
 are replaced by the subforms they match in the input. */
function transformTemplate(template, replacementDatums) {
    if (template['pattern-identifier']) {

        template['pattern-identifier'].identifier = replacementDatums[template['pattern-identifier'].identifier];
    } else if (template['template-element']) {
        var elements = template['template-element'];
        for (var i = 0; i < elements.length; ++i)
            transformTemplate(elements[i].template, replacementDatums);
        if (template['.template'])
            transformTemplate(template['.template'])
    }
}

// todo bl handle "..."
function templateToString(template) {

    if (template['pattern-identifier']) {
        // should already have been replaced by the input form
        return template['pattern-identifier'].identifier;
    } else if (template['template-datum']) {
        return template['template-datum']['string']
            || template['template-datum']['character']
            || template['template-datum']['boolean']
            || template['template-datum']['number'];
    }
    // todo bl distinguish between lists and vectors!! we already erased the #(
    else if (template['template-element']) {
        var ans = '(';
        var elements = template['template-element'];
        for (var i = 0; i < elements.length; ++i)
            ans += templateToString(elements[i].template) + ' '
        if (template['.template'])
            ans += ' . ' + templateToString(template['.template']);
        return ans + ")";
    }

}
