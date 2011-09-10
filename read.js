function Datum() {
    /*this.firstChild = null;
     this.nextSibling = null;
     this.type = null;
     this.payload = null;
     this.nonterminals = [];*/
}

Datum.prototype.setParse = function(type) {
    if (!this.nonterminals)
        this.nonterminals = [];
    this.nonterminals.push(type);
};

Datum.prototype.unsetParse = function() {
    this.nonterminals = [];
    for (var child in this)
        if (child instanceof Datum)
            child.unsetParse();
};

Datum.prototype.appendSibling = function(sibling) {
    if (!this.nextSibling) {
        if (this.parent) {
            // Propagate the parent field
            sibling.parent = this.parent;
            this.parent = null;
        }
        this.nextSibling = sibling;
    }
    else
        this.nextSibling.appendSibling(sibling);
};

Datum.prototype.appendChild = function(child) {
    if (!this.firstChild)
        this.firstChild = child;
    else this.firstChild.appendSibling(child);
};

function Reader(text) {
    this.scanner = new Scanner(text);
    this.readyTokens = [];
    this.nextTokenToReturn = 0;
    this.errorToken = null;
    this.errorMsg = '';
}

Reader.prototype.nextToken = function() {
    while (this.nextTokenToReturn >= this.readyTokens.length) {
        var token = this.scanner.nextToken();
        if (!token)
            return null;
        this.readyTokens.push(token);
    }
    return this.readyTokens[this.nextTokenToReturn++];
};

Reader.prototype.assertNextTokenType = function(type) {
    var token = this.nextToken();
    if (!token) {
        this.errorMsg = 'eof';
        return null;
    }
    if (token.type === type) {
        return token;
    } else {
        this.errorToken = token;
        this.errorMsg = 'expected ' + type;
        return null;
    }
};

Reader.prototype.rhs = function() {
    var ansDatum = new Datum();
    var parseFunction;
    var tokenStreamStart = this.nextTokenToReturn;

    for (var i = 0; i < arguments.length; ++i) {
        var element = arguments[i];
        var cur = (parseFunction = this[element.type])
            ? this.onNonterminal(ansDatum, element, parseFunction)
            : this.onTerminal(ansDatum, element);
        if (!cur) {
            this.nextTokenToReturn = tokenStreamStart;
            return null;
        }
    }

    return ansDatum;
};

Reader.prototype.onNonterminal = function(ansDatum, element, parseFunction) {

    // Handle * and +
    if (element.atLeast !== undefined) { // explicit undefined since atLeast 0 should be valid
        var prev, cur, firstChild;
        var num = 0;
        while (cur = parseFunction.apply(this)) {
            ++num;
            if (!firstChild)
                firstChild = cur;
            if (prev)
                prev.nextSibling = cur;
            prev = cur;
        }

        if (num >= element.atLeast) {
            ansDatum.type = element.name || element.type;
            ansDatum.appendChild(firstChild);
            if (prev)
                prev.parent = ansDatum;
            return ansDatum;
        } else {
            this.nextTokenToReturn -= num;
            this.errorMsg = 'expected at least '
                + element.atLeast + ' ' + element.nodeName + ', got ' + num;
            return null;
        }
    }

    // The normal case is exactly one of element.
    else {
        var parsed = parseFunction.apply(this);
        if (!parsed)
            return parsed;
        else {
            ansDatum.type = element.name || element.type;
            ansDatum.appendChild(parsed);
            return ansDatum;
        }
    }
};

Reader.prototype.onTerminal = function(ansDatum, element) {
    var token = this.assertNextTokenType(element.type);
    if (token) {
        if (token.payload) {
            ansDatum.payload = token.payload;
            ansDatum.type = token.type;
        }
        return ansDatum;
    } else return null;
};

Reader.prototype.alternation = function() {
    var possibleRhs;
    // The most informative error is probably the failed parse
    // that got furthest through the input.
    var mostInformativeErrorToken = null;
    var mostInformationErrorMsg = null;
    for (var i = 0; i < arguments.length; ++i) {
        possibleRhs = this.rhs.apply(this, arguments[i]);
        if (possibleRhs)
            return possibleRhs;
        else if (!mostInformativeErrorToken
            || (this.errorToken && this.errorToken.stop > mostInformativeErrorToken.stop)) {
            mostInformativeErrorToken = this.errorToken;
            mostInformationErrorMsg = this.errorMsg;
        }
    }

    this.errorToken = mostInformativeErrorToken;
    this.errorMsg = mostInformationErrorMsg;
    return null;
};

// <datum> -> <simple datum> | <compound datum>
// <simple datum> -> <boolean> | <number> | <character> | <string> | <symbol>
// <compound datum> -> <list> | <vector>
// <symbol> -> <identifier>
// <list> -> (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
// <vector> -> #(<datum>*)
// <abbreviation> -> <abbrev prefix> <datum>
// <abbrev prefix> -> ' | ` | , | ,@
Reader.prototype['datum'] = function() {
    return this.alternation(
        [
            {type: 'identifier'}
        ],
        [
            {type: 'boolean'}
        ],
        [
            {type: 'number'}
        ],
        [
            {type: 'character'}
        ],
        [
            {type: 'string'}
        ],
        [
            {type: '('},
            {type: 'datum', atLeast: 0, name: 'list'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'datum', atLeast: 1, name: 'improper-list'},
            {type: '.'},
            {type: 'datum', name: 'improper-list'},
            {type: ')'}
        ],
        [
            {type: '#('},
            {type: 'datum', atLeast: 0, name: 'vector'},
            {type: ')'}
        ],
        [
            {type: "'"},
            {type: 'datum', name: "'"}
        ],
        [
            {type: '`'},
            {type: 'datum', name: '`'}
        ],
        [
            {type: ','},
            {type: 'datum', name: ','}
        ],
        [
            {type: ',@'},
            {type: 'datum', name: ',@'}
        ]);

};

Reader.prototype.read = function() {
    var ans = this['datum']();
    return ans ? ans : {token: this.errorToken, msg: this.errorMsg};
};