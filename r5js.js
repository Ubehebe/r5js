function nextToken(text, offset) {

    var maybeToken = text.charAt(offset);

    // ignore whitespace
    if (' \r\n\t'.indexOf(maybeToken) !== -1)
        return nextToken(text, offset + 1);

    // one-character primitive tokens
    else if ("()'`,.".indexOf(maybeToken) !== -1)
        return {success: true, token: {type: maybeToken}, offset: offset + 1};

    // two-character primitive tokens
    else if ((maybeToken = text.substr(offset, 2)) === '#(' || maybeToken === ',@')
        return {success: true, token: {type: maybeToken}, offset: offset + 2};

    else {

        var ans;
        var toTry = [nextIdentifier, nextBoolean, nextCharacter, nextString, nextNumber];

        for (var i = 0; i < toTry.length; ++i)
            if ((ans = toTry[i](text, offset)).success)
                return ans;

        return ans;
    }
}

function nextNumber(text, offset) {
    var bases = [10,16,8,2]; // order by common case
    var ans;
    for (var i = 0; i < bases.length; ++i)
        if ((ans = nextNumberInBase(bases[i], text, offset).success))
            return ans;
    return {success: false, offset: offset, msg: 'invalid number'};
}

function nextNumberInBase(base, text, offset) {

    var prefix = parseNumberPrefix(base, text, offset);
    if (!prefix.success)
        return prefix;
    parseComplex(base, text, prefix.offset);
}

function parseComplex(base, text, offset) {
    parseReal(base, text, offset);
}

function parseReal(base, text, offset) {

    var sign = text.charAt(0);

    if (sign === '+' || sign === '-')
        return parseUreal(base, text, offset + 1, {negate: true});
    else return parseUreal(base, text, offset);

}

function parseUreal(base, text, offset) {
    var maybeNumerator = parseUinteger(base, text, offset);
    if (maybeNumerator.success) {
        if (text.charAt(maybeNumerator.offset) === '/')
            var maybeDenominator = parseUinteger(base, text, maybeNumerator.offset+1);
    } else {
        parseDecimal(text, offset);
    }
}

function parseDecimal(text, offset) {
    var beforeDot = parseUinteger(10, text, offset);
    var decimalSuffix = parseDecimalSuffix(text, beforeDot.offset);
    

}

function parseUinteger(base, text, offset) {
    var digits = {2: '01', 8: '01234567', 10: '0123456789', 16: '0123456789abcdef'};
    var len = 0;
    var cur;

    while ((cur = text.charAt(offset + len)).length === 1
        && digits[base].indexOf(cur) !== -1)
        ++len;

    if (len === 0)
        return {success: false, offset: offset, msg: 'parse error in number'};

    while (cur = text.charAt(offset + len).length === 1
        && cur === '#')
        ++len;

    // todo bl: could turn it into an int right here
    return {success: true, offset: offset+len, token: {type: 'number', value: text.substr(offset, len)}};
}

function parseNumberPrefix(text, offset) {

    var allowedInPrefix = '#iexodb';
    var maxPrefixLen = text.substr(offset, 4).length; // at most 4
    var prefixLen = 0;

    for (var i = 0; i < maxPrefixLen; ++i) {
        if (allowedInPrefix.indexOf(text.charAt(offset + i)) === -1) {
            prefixLen = i;
            break;
        }
    }

    var attrs = {};

    switch (prefixLen) {
        // common-case ordering
        case 0:
            return {success: true, token: {type: 'number', attrs: {base: 'd', exact: false}}, offset: offset + prefixLen};
        // prefix must have even length
        case 1:
        case 3:
            return {success: false, offset: offset, msg: 'parse error in number prefix'};
        case 2:
            addNumberPrefix(text, offset, attrs);
            return attrs
                ? {success: true, token: {type: 'number', attrs: attrs}}
                : {success: false, offset: offset, msg: 'parse error in number prefix'};
        case 4:
            addNumberPrefix(text, offset, attrs);
            addNumberPrefix(text, offset + 2, attrs);
            return attrs
                ? {success: true, token: {type: 'number', attrs: attrs}, offset: offset + prefixLen}
                : {success: false, offset: offset, msg: 'parse error in number prefix'};
        default:
            return {success: false, offset: offset, msg: 'invalid number prefix length '
                + prefixLen + ' (should never happen)'};
    }

    function addNumberPrefix(text, offset, existingPrefixAttrs) {

        var poundSign = text.charAt(offset);
        var what = text.charAt(offset + 1);

        if (poundSign !== '#' || what.length !== 1)
            return null;

        switch (what) {
            case 'i':
            case 'e':
                if (existingPrefixAttrs['exact'])
                    return null;
                else {
                    existingPrefixAttrs['exact'] = what === 'e';
                    return existingPrefixAttrs
                }
            case 'x':
            case 'o':
            case 'b':
            case 'd':
                if (existingPrefixAttrs['base'])
                    return null;
                else {
                    existingPrefixAttrs['base'] = what;
                    return existingPrefixAttrs
                }
            default:
                return null;
        }
    }

}

function nextCharacter(text, offset) {
    if (text.substr(offset, 2) !== '#\\')
        return {success: false, offset: offset, msg: "invalid character literal"};
    else if (text.substr(offset, 7) === "#\\space")
        return {success: true, token: {type: 'character', value: ' '}, offset: offset + 7};
    else if (text.substr(offset, 9) === "#\\newline")
        return {success: true, token: {type: 'character', value: '\n'}, offset: offset + 9};
    else
        return {success: true, token: {type: 'character', value: text[offset + 2]}, offset: offset + 3};
}

function nextString(text, offset) {
    if (text.charAt(offset) !== '"')
        return {success: false, offset: offset, msg: 'expected "'};

    var len = 0;
    while (validStringElement(text, offset + ++len))
        ;

    return (text.charAt(offset + len) === '"')
        ? {success: true, token: {type: 'string', value: text.substr(offset + 1, offset + len - 1)}, offset: offset + len}
        : {success: false, offset: offset, msg: 'unterminated string literal'};


    function validStringElement(text, offset) {
        var cur = text.charAt(offset);
        if (cur.length === 1 && cur !== '"' && cur !== '\\')
            return true;
        else if ((cur = text.substr(offset, 2)) === '\\"'
            || cur === '\\\\')
            return true;
        else return false;
    }
}

function nextBoolean(text, offset) {
    var maybeBool = text.substr(offset, 2);
    switch (maybeBool) {
        case '#t':
        case '#f':
            return {success: true, token: {type: 'boolean', value: maybeBool}, offset: offset + 2};
        default:
            return {success: false, offset: offset, msg: 'expected #t or #f'};
    }
}

function nextIdentifier(text, offset) {
    var initial = text.charAt(offset);

    /* if the first character is invalid, either the token is one of the peculiar
     identifiers or is invalid. */
    if (!validInitial(initial))
        return peculiarIdentifier(text, offset);

    var len = 0;

    while (validSubsequent(text.charAt(offset + ++len)))
        ;

    return {success: true, token: {type: 'identifier', value: text.substr(offset, len)}, offset: offset + len};

    function peculiarIdentifier(text, offset) {
        var maybePeculiar = text.charAt(offset);
        if (maybePeculiar === '+' || maybePeculiar === '-')
            return {success: true, token: {type: 'identifier', value: maybePeculiar}, offset: offset + 1};
        /* todo bl: unreachable. we will always parse '...' as type '.' first.
         the grammar is ambiguous: is '...' an actual peculiar identfier or
         an ellipsis denoting an open class of peculiar identifiers? */
        else if (text.substr(offset, 3) === '...')
            return {success: true, token: {type: 'identifier', value: '...'}, offset: offset + 3};
        else
            return {success: false, offset: offset, msg: 'invalid identifier'};
    }

    function validInitial(c) {
        return c.length === 1
            && ((c >= 'a' && c <= 'z') || '!$%&*/:<=>?^_~'.indexOf(c) !== -1);
    }

    function validSubsequent(c) {
        return c.length === 1
            && (validInitial(c) || (c >= '0' && c <= '9') || '+-.@'.indexOf(c) !== -1);
    }
}

function assertValidToken(text, type) {
    var ans = nextToken(text, 0);
    if (!ans.success) {
        console.error("parse error on valid token " + text);
    } else if (type && type !== ans.token.type) {
        console.error("parse error on " + text + ": expected type " + type
            + ", actual type " + ans.token.type);
    }
}

var validTokens = {
    'identifier': ['h', '+', '-', '...', '!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '~', '_', '^', '&+', 'h+...@@@-.'],
    'character': ['#\\c', '#\\space', '#\\newline', '#\\\\'],
    'string': ['""', '"hello, world"', '" \\" "', '"\\\\"'],
    'boolean': ['#t', '#f'],
    'number': []
};

for (var type in validTokens)
    validTokens[type].forEach(function(text) {
        assertValidToken(text, type);
    });

console.log(parseUinteger(10, "3875", 0));

var invalidIdentifiers = ['0', '\\', 'iden/tifier'];
