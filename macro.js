function SchemeMacro(literalIdentifiers, rules, definitionEnv) {
    this.literalIdentifiers = literalIdentifiers;
    this.rules = rules;
    this.definitionEnv = definitionEnv;
}

SchemeMacro.prototype.allPatternsBeginWith = function(keyword) {

    for (var rule = this.rules; rule; rule = rule.nextSibling) {
        var pattern = rule.at('pattern');
        if (!pattern.isList() || pattern.firstChild.payload !== keyword)
            return false;
    }
    return true;

};
/* todo bl start here

 */
/* 4.3.2: The keyword at the beginning of the pattern in a <syntax rule>
 is not involved in the matching and is not considered a pattern variable
 or literal identifier. */
/*
 pattern.shift();

 this.patterns.push(pattern);
 this.templates.push(syntaxRule.template);
 }
 }

 function PatternMatch(replacementDatums, template) {
 this.replacementDatums = replacementDatums;
 this.template = template;
 }

 PatternMatch.prototype.hygienicTranscription = function() {
 return transformTemplate(this.template, this.replacementDatums);
 };*/

SchemeMacro.prototype.selectTemplate = function(datum, useEnv) {
    for (var rule = this.rules; rule; rule = rule.nextSibling) {
        var bindings = {};
        var pattern = rule.at('pattern');
        console.log('selectTemplate: considering pattern');
        console.log(pattern);
        if (this.patternMatch(pattern, datum, useEnv, bindings, true))
            return new Template(rule.at('template'), bindings);
    }
    return null;
};

function Template(datum, bindings) {
    this.datum = datum;
    this.bindings = bindings;
}

Template.prototype.hygienicTranscription = function() {
    
};

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
SchemeMacro.prototype.patternMatch = function(patternDatum, inputDatum, useEnv, ansDict, ignoreLeadingKeyword) {

    console.log('patternMatch: patternDatum:');
    console.log(patternDatum);
    console.log('inputDatum:');
    console.log(inputDatum);

    var isIdentifier = patternDatum.payload;
    var isLiteralIdentifier = isIdentifier && contains(this.literalIdentifiers, patternDatum.payload);

    /* (1) P is a non-literal identifier
     Example: (let-syntax ((foo (syntax-rules () ((foo x) "aha!")))) (foo (1 2 3))) => "aha!"
     ((1 2 3) would be a procedure-call error if evaluated, but it is never evaluated.) */
    if (isIdentifier && !isLiteralIdentifier) {
        ansDict[isIdentifier] = inputDatum;
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

    else if (isIdentifier && isLiteralIdentifier) {
        if (inputDatum.payload) {
            if ((inputDatum.payload === isIdentifier
                && this.definitionEnv[isIdentifier] === undefined
                && useEnv[isIdentifier] === undefined)
                || (this.definitionEnv[isIdentifier] === useEnv[isIdentifier])) {
                ansDict[isIdentifier] = inputDatum;
                return true;
            }
        }
        return false;
    }

    /* (3) P is a list (P1 ... Pn) and F is a list of n forms that match P1 through Pn, respectively
     Example: (let-syntax ((foo (syntax-rules () ((foo (x y z)) y)))) (foo (1 2 3))) => 2 */
    else if (patternDatum.isList() && inputDatum.isList()) {
        var patternElement = patternDatum.firstChild;
        var inputElement = inputDatum.firstChild;
        if (ignoreLeadingKeyword) {
            patternElement = patternElement.nextSibling;
            inputElement = inputElement.nextSibling;
        }

        for (/* already initialized */;
            patternElement && inputElement;
            patternElement = patternElement.nextSibling,
                inputElement = inputElement.nextSibling) {
            if (!this.patternMatch(patternElement, inputElement, useEnv, ansDict))
                return false;
        }
        return !(patternElement || inputElement);
    }
};

function contains(array, x) {
    for (var i = 0; i < array.length; ++i)
        if (array[i] === x)
            return true;
    return false;
}

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
