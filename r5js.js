var lex = {};

lex['token'] = function(text, offset) {

    var maybeToken = text.charAt(offset);

    // ignore whitespace
    if (' \r\n\t'.indexOf(maybeToken) !== -1)
        return lex['token'](text, offset + 1);

    // one-character primitive tokens
    else if ("()'`,.".indexOf(maybeToken) !== -1)
        return parseOk(maybeToken, maybeToken, offset + 1);

    // two-character primitive tokens
    else if ((maybeToken = text.substr(offset, 2)) === '#(' || maybeToken === ',@')
        return parseOk(maybeToken, maybeToken, offset + 2);

    else {

        var ans;
        var toTry = ['identifier', 'boolean', 'character', 'string', 'num'];

        for (var i = 0; i < toTry.length; ++i)
            if ((ans = lex[toTry[i]](text, offset)).success)
                return ans;

        return ans;
    }
};

lex['num'] = function(text, offset) {
    var bases = [10,16,8,2]; // order by common case
    var ans;
    for (var i = 0; i < bases.length; ++i)
        if ((ans = lex['num-in-base'](bases[i], text, offset)).success)
            return parseOk('num', ans.value, ans.offset);
    return ans;
};

lex['num-in-base'] = function(base, text, offset) {
    var prefix = lex['num-prefix'](base, text, offset);
    return (!prefix.success) ? prefix : lex['complex'](base, text, prefix.offset);
};

/*
 <complex R> -> <real R>
 | <real R> @ <real R>
 | <real R> + <ureal R> i
 | <real R> - <ureal R> i
 | <real R> + i
 | <real R> - i
 | + <ureal R> i
 | - <ureal R> i
 | + i
 | - i
 */
lex['complex'] = function(base, text, offset) {


    var sign = lex['sign'](text, offset);
    if (!sign.success)
        return sign;

    // All the rules beginning with <real R> on the RHS
    else if (sign.value === '') {
        var real1 = lex['real'](base, text, offset);
        if (!real1.success)
            return real1;
        var maybeOp = text.charAt(real1.offset);
        if (maybeOp.length === 1 && '+-@'.indexOf(maybeOp) !== -1) {

            // <complex R> -> <real R> @ <real R>
            if (maybeOp === '@') {
                var real2 = lex['real'](base, text, real1.offset + 1);
                return real2.success
                    ? parseOk('real', real1.value + '@' + real2.value)
                    : real2;
            }

            // <complex R> -> <real R> + i | <real R> - i
            else if (text.charAt(real1.offset + 1) === 'i' || text.charAt(real1.offset + 1) === 'I') {
                return parseOk('complex', real1.value + maybeOp + 'i', real1.offset + 2);
            }

            // <complex R> -> <real R> + <ureal R> i | <real R> - <ureal R> i
            else {
                var mustBeUrealAfterOp = lex['ureal'](base, text, real1.offset + 1);
                if (!mustBeUrealAfterOp.success)
                    return mustBeUrealAfterOp;
                var mustBeIAfterUreal = text.charAt(mustBeUrealAfterOp.offset);
                return mustBeIAfterUreal === 'i' || mustBeIAfterUreal === 'I'
                    ? parseOk('complex', real1.value + maybeOp + mustBeUrealAfterOp.value + 'i', mustBeUrealAfterOp.offset + 1)
                    : parseError('complex', mustBeUrealAfterOp.offset, 'expected i');
            }
        } else return parseOk('complex', real1.value, real1.offset); // <complex R> -> <real R>
    }

    //  <complex R> -> + i | - i
    else if (text.charAt(sign.offset) === 'i' || text.charAt(offset) === 'I') {
        return parseOk('complex', sign.value + 'i', sign.offset + 1);
    }

    // <complex R> -> + <ureal R> i | - <ureal R> i
    else {
        var mustBeUreal = lex['ureal'](base, text, sign.offset);
        if (!mustBeUreal.success)
            return mustBeUreal;
        var mustBeI = text.charAt(text, mustBeUreal.offset)
        return (mustBeI === 'i' || mustBeI === 'I')
            ? parseOk('complex', sign.value + mustBeUreal.value + 'i', mustBeUreal.offset + 1)
            : parseError('complex', mustBeUreal.offset, 'expected i');
    }
};

// <real R> -> <sign> <ureal R>
lex['real'] = function(base, text, offset) {

    var sign = lex['sign'](text, offset);
    if (!sign.success)
        return sign;

    var ureal = lex['ureal'](base, text, sign.offset);

    return ureal.success
        ? parseOk('real', sign.value + ureal.value, ureal.offset)
        : ureal;
};

lex['sign'] = function(text, offset) {
    var maybeSign = text.charAt(offset);
    if (maybeSign.length !== 1)
        return eofError('sign', offset);
    else if ('+-'.indexOf(maybeSign) !== -1)
        return parseOk('sign', maybeSign, offset + 1);
    else return parseOk('sign', '', offset);
};

/*
 <ureal R> -> <uinteger R>
 | <uinteger R> / <uinteger R>
 | <decimal 10>
 */
lex['ureal'] = function(base, text, offset) {
    var uint1 = lex['uinteger'](base, text, offset);
    if (uint1.success) {
        if (text.charAt(uint1.offset) === '/') {
            var uint2 = lex['uinteger'](base, text, uint1.offset + 1);
            return uint2.success
                ? parseOk('ureal', uint1.value + '/' + uint2.value, uint2.offset)
                : uint2;
        } else return uint1;
    } else return lex['decimal'](text, offset);
};

/* <decimal 10> -> <uinteger 10> <suffix>
 | . <digit 10>+ #* <suffix>
 | <digit 10>+ . <digit 10>+ #* <suffix>
 | <digit 10>+ #+ . #* <suffix>
 */
lex['decimal'] = function(text, offset) {
    var maybeUinteger10 = lex['uinteger'](10, text, offset);
    if (maybeUinteger10.success) {
        lex['decimal-suffix'](text, maybeUinteger10.offset);
    } else {
        var digitsBeforeDot = 0;
        var cur;
        while ((cur = text.charAt(offset + digitsBeforeDot)) >= '0' && cur <= '9')
            ++digitsBeforeDot;
    }
};

// <suffix> -> <empty> | <exponent marker> <sign> <digit 10>+
lex['decimal-suffix'] = function(text, offset) {

    var marker = lex['exponent-marker'](text, offset);
    if (!marker.success)
        return parseOk('decimal-suffix', '', offset);

    var sign = lex['sign'](text, marker.offset);
    if (!sign.success)
        return sign;

    var digits = lex['digits'](10, text, sign.offset);
    if (!digits.success)
        return digits;

    return parseOk('decimal-suffix', marker.value + sign.value + digits.value, digits.offset);
};

lex['exponent-marker'] = function(text, offset) {
    var allowedMarkers = 'esfdl';
    var marker = text.charAt(offset);
    if (marker.length !== 1) {
        return eofError('exponent-marker', offset);
    } else if (allowedMarkers.indexOf(marker) === -1) {
        return parseError('exponent-marker', offset, 'invalid exponent-marker ' + marker);
    } else return parseOk('exponent-marker', marker, offset + 1);
};

function parseError(type, offset, msg) {
    return {success: false, tokenType: type, offset: offset, msg: msg || 'parse error'};
}

function eofError(type, offset) {
    return parseError(type, offset, 'unexpected EOF while parsing ' + type);
}

function parseOk(type, value, nextOffset) {
    return {success: true, tokenType: type, value: value, offset: nextOffset};
}

// <uinteger R> -> <digit R>+ #*
lex['uinteger'] = function(base, text, offset) {

    var digits = lex['digits'](base, text, offset);
    if (!digits.success)
        return digits;

    var i = digits.offset;
    var cur;

    while ((cur = text.charAt(i)).length === 1 && cur === '#')
        ++i;

    return parseOk('uinteger', digits.value, i);
};

// <prefix R> -> <radix R> <exactness> | <exactness> <radix R>
lex['num-prefix'] = function(base, text, offset) {
    var radix1 = lex['radix'](base, text, offset);
    if (radix1.success) {
        var exactness2 = lex['exactness'](text, radix1.offset);
        return exactness2.success
            ? parseOk('num-prefix', radix1.value + exactness2.value, exactness2.offset)
            : exactness2;
    } else {
        var exactness1 = lex['exactness'](text, offset);
        if (!exactness1.success)
            return exactness1;
        var radix2 = lex['radix'](base, text, offset);
        return radix2.success
            ? parseOk('num-prefix', exactness1.value + radix2.value, radix2.offset)
            : radix2;
    }
};

/* <radix 2> -> #b
 <radix 8> -> #o
 <radix 10> -> <empty> | #d
 <radix 16> -> #x
 */
lex['radix'] = function(base, text, offset) {

    var expected = {2: 'b', 8: 'o', 10: 'd', 16: 'x'};

    var maybeHash = text.charAt(offset);
    if (maybeHash !== '#')
        return base === 10
            ? parseOk('radix', base, offset)
            : parseError('radix', offset, 'expected #');

    var maybeBase = text.charAt(offset + 1);
    if (maybeBase.length !== 1)
        return eofError('radix', offset + 1);
    else if (maybeBase !== expected[base])
        return parseError('radix', offset + 1, 'invalid radix ' + maybeBase);
    else
        return parseOk('radix', base, offset + 2);
};

// <exactness> -> <empty> | #i | #e
lex['exactness'] = function(text, offset) {
    var maybeExactness = text.substr(offset, 2);
    if (maybeExactness === '#i'
        || maybeExactness === '#e'
        || maybeExactness === "#I"
        || maybeExactness === "#E")
        return parseOk('exactness', maybeExactness, offset + 2);
    else return parseOk('exactness', '', offset);
};


/* <digit 2> -> 0 | 1
 <digit 8> -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
 <digit 10> -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
 <digit 16> -> 0 | 1 | 2 | 3 |4 | 5| 6 | 7 | 8 | 9 | a | b | c | d | e | f
 */
lex['digits'] = function(base, text, offset) {

    var acceptable = {2: '01',
        8: '01234567',
        10: '0123456789',
        16: '0123456789abcdefABCDEF'};

    if (text.charAt(offset).length !== 1)
        return eofError('digits-' + base, offset);

    var numDigits = 0;
    var cur;
    while ((cur = text.charAt(offset + numDigits)).length === 1
        && acceptable[base].indexOf(cur) !== -1)
        ++numDigits;

    if (numDigits === 0)
        return parseError('digits-' + base, offset, 'expected digits-' + base);

    return parseOk('digits-' + base, text.substr(offset, numDigits), offset + numDigits);
};

lex['character'] = function(text, offset) {
    var requiredPrefix = text.substr(offset, 2);
    if (requiredPrefix !== '#\\')
        return parseError('character', offset, "invalid character literal prefix " + requiredPrefix);
    else if (text.substr(offset, 7) === "#\\space")
        return parseOk('character', ' ', offset + 7);
    else if (text.substr(offset, 9) === "#\\newline")
        return parseOk('character', '\n', offset + 9);
    else
        return parseOk('character', text.charAt(offset + 2), offset + 3);
};

lex['string'] = function(text, offset) {
    if (text.charAt(offset) !== '"')
        return parseError('string', offset, 'expected "');

    var len = 0;
    while (validStringElement(text, offset + ++len))
        ;

    return (text.charAt(offset + len) === '"')
        ? parseOk('string', text.substr(offset + 1, offset + len - 1), offset + len)
        : parseError('string', offset, 'unterminated string literal');

    function validStringElement(text, offset) {
        var cur = text.charAt(offset);
        if (cur.length === 1 && cur !== '"' && cur !== '\\')
            return true;
        else if ((cur = text.substr(offset, 2)) === '\\"'
            || cur === '\\\\')
            return true;
        else return false;
    }
};

lex['boolean'] = function(text, offset) {
    var maybeBool = text.substr(offset, 2);
    switch (maybeBool) {
        case '#t':
            return parseOk('boolean', true, offset + 2);
        case '#f':
            return parseOk('boolean', false, offset + 2);
        default:
            return parseError('boolean', offset);
    }
};

lex['identifier'] = function(text, offset) {
    var initial = text.charAt(offset);

    /* if the first character is invalid, either the token is one of the peculiar
     identifiers or is invalid. */
    if (!validInitial(initial))
        return lex['peculiar-identifier'](text, offset);

    var len = 0;

    while (validSubsequent(text.charAt(offset + ++len)))
        ;

    return parseOk('identifier', text.substr(offset, len), offset + len);

    function validInitial(c) {
        return c.length === 1
            && ((c >= 'a' && c <= 'z') || '!$%&*/:<=>?^_~'.indexOf(c) !== -1);
    }

    function validSubsequent(c) {
        return c.length === 1
            && (validInitial(c) || (c >= '0' && c <= '9') || '+-.@'.indexOf(c) !== -1);
    }
};

lex['peculiar-identifier'] = function(text, offset) {
    var maybePeculiar = text.charAt(offset);

    // there seems to be no benefit to returning a type 'peculiar-identifier'
    if (maybePeculiar === '+' || maybePeculiar === '-')
        return parseOk('identifier', maybePeculiar, offset + 1);
    /* todo bl: unreachable. we will always parse '...' as type '.' first.
     the grammar is ambiguous: is '...' an actual peculiar identfier or
     an ellipsis denoting an open class of peculiar identifiers? */
    else if (text.substr(offset, 3) === '...')
        return parseOk('identifier', '...', offset + 3);
    else
        return parseError('identifier', offset);
};

function tokenize(raw) {
    var tokens = [];
    var offset = 0;
    var token;
    while (offset < raw.length) {
        token = lex['token'](raw, offset);
        tokens.push(abbrevToken(token));
        offset = consumeWhitespace(raw, token.offset);
    }
    return tokens;

    function consumeWhitespace(text, offset) {
        var cur;
        while ((cur = text.charAt(offset)) === ' ' || cur === '\t' || cur === '\n')
            ++offset;
        return offset;
    }

    function abbrevToken(token) { return {type: token.tokenType, value: token.value}; }
}

function assertValidToken(text, type) {
    var ans = lex[type](text, 0);
    if (!ans.success) {
        console.error("parse error on valid token " + text);
    } else if (type && type !== ans.tokenType) {
        console.error("parse error on " + text + ": expected type " + type
            + ", actual type " + ans.tokenType);
    }
}

var validTokens = {
    'identifier': ['h', '+', '-', '...', '!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '~', '_', '^', '&+', 'h+...@@@-.'],
    'character': ['#\\c', '#\\space', '#\\newline', '#\\\\'],
    'string': ['""', '"hello, world"', '" \\" "', '"\\\\"'],
    'boolean': ['#t', '#f']
    //'complex': ['1+i', '1-i', '+i', '-i']
};

for (var type in validTokens)
    validTokens[type].forEach(function(text) {
        assertValidToken(text, type);
    });

console.log(tokenize("(define (foo x y) (+ x (* 2 y)))"));


var invalidIdentifiers = ['0', '\\', 'iden/tifier'];
