var scanner = {};

scanner.nextToken = function(text, offset) {

    var scan = {};

    /* <token> -> <identifier> | <boolean> | <number> | <character>
     | <string> | ( | ) | #( | ' | ` | , | ,@ | .
     */
    scan['token'] = function(text, offset) {

        var unigraph = text.charAt(offset);
        var digraph;

        /* The dot has to be handled differently because it can also be a decimal
         point or the beginning of an ellipsis. We have to peek ahead with
         requireTokenEnd() to figure out what to do. */
        if (unigraph === '.') {
            var maybeDot = requireTokenEnd(unigraph, unigraph, text, offset + 1);
            if (maybeDot.success)
                return maybeDot;
            /* If it is not the end of the token, fall through to below to try the
             decimal point and ellipsis interpretations. */


        } else if ("()'`,".indexOf(unigraph) !== -1) {
            return scanOk(unigraph, unigraph, offset + 1);
        } else if ((digraph = text.substr(offset, 2)) === '#(' || digraph === ',@') {
            return scanOk(digraph, digraph, offset + 2);
        }

        // no "else": we need to catch the fallthrough from above.

        var ans;

        // order by common case
        var toTry = ['identifier', 'boolean', 'character', 'string', 'number'];

        for (var i = 0; i < toTry.length; ++i)
            if ((ans = scan[toTry[i]](text, offset)).success)
                return ans;

        return ans;
    };

// <identifier> -> <initial> <subsequent>* | <peculiar identifier>
    scan['identifier'] = function(text, offset) {

// <initial> -> <letter> | <special initial>
        var validInitial = function(c) {
            return c.length === 1
                && ((c >= 'a' && c <= 'z') // <letter> -> a | b | ... | z | A | B | ... | Z
                || (c >= 'A' && c <= 'Z')
                || '!$%&*/:<=>?^_~'.indexOf(c) !== -1); // <special initial>
        };

        // <subsequent> -> <initial> | <digit 10> | <special subsequent>
        var validSubsequent = function(c) {
            return c.length === 1
                && (validInitial(c)
                || (c >= '0' && c <= '9')
                || '+-.@'.indexOf(c) !== -1); // special subsequents
        };

        if (!validInitial(text.charAt(offset)))
            return scan['peculiar-identifier'](text, offset);

        var subsequentEnd = consumeWhile(text, offset + 1, validSubsequent);

        return requireTokenEnd('identifier', text.substr(offset, subsequentEnd - offset), text, subsequentEnd);
    };

// <peculiar identifier> -> + | - | ...
// Not clear to me whether the ... is an admissible peculiar identifier or
// just an ellipsis.
    scan['peculiar-identifier'] = function(text, offset) {
        var maybePeculiar = text.charAt(offset);

        // we return type 'identifier' not 'peculiar-identifier'
        if (maybePeculiar === '+' || maybePeculiar === '-')
            return requireTokenEnd('identifier', maybePeculiar, text, offset + 1);
        /* todo bl: unreachable. we will always parse '...' as type '.' first.
         the grammar is ambiguous: is '...' an actual peculiar identfier or
         an ellipsis denoting an open class of peculiar identifiers? */
        else if (text.substr(offset, 3) === '...')
            return requireTokenEnd('identifier', '...', text, offset + 3);
        else
            return scanError('identifier', offset);
    };


// <boolean> -> #t | #f
    scan['boolean'] = function(text, offset) {
        var maybeBool = text.substr(offset, 2);
        switch (maybeBool) {
            case '#t':
                return scanOk('boolean', true, offset + 2);
            case '#f':
                return scanOk('boolean', false, offset + 2);
            default:
                return scanError('boolean', offset);
        }
    };

// <character> -> #\ <any character> | #\ <character name>
// <character name> -> space | newline
    scan['character'] = function(text, offset) {
        var requiredPrefix = text.substr(offset, 2);
        if (requiredPrefix !== '#\\')
            return scanError('character', offset, "invalid character literal prefix " + requiredPrefix);
        else if (text.substr(offset, 7) === "#\\space")
            return requireTokenEnd('character', ' ', text, offset + 7);
        else if (text.substr(offset, 9) === "#\\newline")
            return requireTokenEnd('character', '\n', text, offset + 9);
        else
            return requireTokenEnd('character', text.charAt(offset + 2), text, offset + 3);
    };

// <string> -> " <string element>* "
    scan['string'] = function(text, offset) {
        if (text.charAt(offset) !== '"')
            return scanError('string', offset, 'expected "');

        /* We can't use the generic consumeWhile() because of the valid
         digraphs \" and \\ */
        var len = 0;
        while (validStringElement(text, offset + ++len))
            ;

        return (text.charAt(offset + len) === '"')
            ? scanOk('string', text.substr(offset + 1, offset + len - 1), offset + len)
            : scanError('string', offset, 'unterminated string literal');

        // <string element> -> <any character other than " or \> | \" | \\
        function validStringElement(text, offset) {
            var cur = text.charAt(offset);
            if (cur.length === 1 && cur !== '"' && cur !== '\\')
                return true;
            else return (cur = text.substr(offset, 2)) === '\\"'
                || cur === '\\\\';
        }
    };

// <number> -> <num 2> | <num 8> | <num 10> | <num 16>
    scan['number'] = function(text, offset) {
        var bases = [10,16,8,2]; // order by common case
        var ans;
        for (var i = 0; i < bases.length; ++i) {
            if ((ans = scan['num'](bases[i], text, offset)).success) {
                return requireTokenEnd('number', ans.value, text, ans.offset);
            }
        }
        return ans;
    };

// <num R> -> <prefix R> <complex R>
    scan['num'] = function(base, text, offset) {
        var prefix = scan['prefix'](base, text, offset);
        if (!prefix.success)
            return prefix;
        var complex = scan['complex'](base, text, prefix.offset);
        return complex.success
            ? requireTokenEnd('num', prefix.value + complex.value, text, complex.offset)
            : complex;
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
    scan['complex'] = function(base, text, offset) {


        var sign = scan['sign'](text, offset);
        if (!sign.success)
            return sign;

        // All the rules beginning with <real R> on the RHS
        else if (sign.value === '') {
            var real1 = scan['real'](base, text, offset);
            if (!real1.success)
                return real1;
            var maybeOp = text.charAt(real1.offset);
            if (maybeOp.length === 1 && '+-@'.indexOf(maybeOp) !== -1) {

                // <complex R> -> <real R> @ <real R>
                if (maybeOp === '@') {
                    var real2 = scan['real'](base, text, real1.offset + 1);
                    return real2.success
                        ? requireTokenEnd('complex', real1.value + '@' + real2.value, text, real2.offset)
                        : real2;
                }

                // <complex R> -> <real R> + i | <real R> - i
                else if (text.charAt(real1.offset + 1) === 'i' || text.charAt(real1.offset + 1) === 'I') {
                    return requireTokenEnd('complex', real1.value + maybeOp + 'i', text, real1.offset + 2);
                }

                // <complex R> -> <real R> + <ureal R> i | <real R> - <ureal R> i
                else {
                    var mustBeUrealAfterOp = scan['ureal'](base, text, real1.offset + 1);
                    if (!mustBeUrealAfterOp.success)
                        return mustBeUrealAfterOp;
                    var mustBeIAfterUreal = text.charAt(mustBeUrealAfterOp.offset);
                    return mustBeIAfterUreal === 'i' || mustBeIAfterUreal === 'I'
                        ? requireTokenEnd('complex', real1.value + maybeOp + mustBeUrealAfterOp.value + 'i', text, mustBeUrealAfterOp.offset + 1)
                        : scanError('complex', mustBeUrealAfterOp.offset, 'expected i');
                }
            } else return requireTokenEnd('complex', real1.value, text, real1.offset); // <complex R> -> <real R>
        }

        //  <complex R> -> + i | - i
        else if (text.charAt(sign.offset) === 'i' || text.charAt(sign.offset) === 'I') {
            return requireTokenEnd('complex', sign.value + 'i', text, sign.offset + 1);
        }

        // <complex R> -> + <ureal R> i | - <ureal R> i
        else {
            var mustBeUreal = scan['ureal'](base, text, sign.offset);
            if (!mustBeUreal.success)
                return mustBeUreal;
            var mustBeI = text.charAt(text, mustBeUreal.offset)
            return (mustBeI === 'i' || mustBeI === 'I')
                ? requireTokenEnd('complex', sign.value + mustBeUreal.value + 'i', text, mustBeUreal.offset + 1)
                : scanError('complex', mustBeUreal.offset, 'expected i');
        }
    };

// <real R> -> <sign> <ureal R>
    scan['real'] = function(base, text, offset) {

        var sign = scan['sign'](text, offset);

        if (!sign.success)
            return sign;

        var ureal = scan['ureal'](base, text, sign.offset);

        return ureal.success
            ? scanOk('real', sign.value + ureal.value, ureal.offset)
            : ureal;
    };

    /*
     <ureal R> -> <uinteger R>
     | <uinteger R> / <uinteger R>
     | <decimal R>
     */
    scan['ureal'] = function(base, text, offset) {

        // <ureal 10> -> <decimal 10>
        if (base === 10) {
            var maybeDecimal = scan['decimal'](text, offset);
            if (maybeDecimal.success)
                return scanOk('ureal', maybeDecimal.value, maybeDecimal.offset);
        }

        var uint1 = scan['uinteger'](base, text, offset);
        if (!uint1.success)
            return uint1;

        // <ureal R> -> <uinteger R> / <uinteger R>
        if (text.charAt(uint1.offset) === '/') {
            var uint2 = scan['uinteger'](base, text, uint1.offset + 1);
            return uint2.success
                ? scanOk('ureal', uint1.value + '/' + uint2.value, uint2.offset)
                : uint2;
        } else return uint1; // <ureal R> -> <uinteger R>
    };

    /* <decimal 10> -> <uinteger 10> <suffix> (i.e. <digit 10>+ #* <suffix>)
     | . <digit 10>+ #* <suffix>
     | <digit 10>+ . <digit 10>+ #* <suffix>
     | <digit 10>+ #+ . #* <suffix>
     */
    scan['decimal'] = function(text, offset) {

        var leadingDot = text.charAt(offset) === '.';
        var firstDigitBlock = scan['digits'](10, text, leadingDot ? offset + 1 : offset);
        if (!firstDigitBlock.success)
            return firstDigitBlock;
        var afterHashes = consumeWhile(text, firstDigitBlock.offset, function(c) {
            return c === '#';
        });
        var suffix;

        // <decimal 10> -> . <digit 10>+ #* <suffix>
        if (leadingDot) {
            suffix = scan['suffix'](text, afterHashes);
            return suffix.success
                ? scanOk('decimal', '.' + firstDigitBlock.value + suffix.value, suffix.offset)
                : suffix;
        }

        else if (text.charAt(afterHashes) === '.') {
            // the second digit block may be empty, which is the meaning of the last parameter
            var secondDigitBlock = scan['digits'](10, text, afterHashes + 1, true);

            // <decimal 10> -> <digit 10>+ . <digit 10>* #* <suffix>
            if (secondDigitBlock.success) {
                var afterSecondHashes = consumeWhile(text, secondDigitBlock.offset, isHash);
                suffix = scan['suffix'](text, afterSecondHashes);
                return suffix
                    ? scanOk('decimal', firstDigitBlock.value + '.' + secondDigitBlock.value + suffix.value, suffix.offset)
                    : suffix;
            }

            // <decimal 10> -> <digit 10>+ #+ . #* <suffix>
            else if (afterHashes > firstDigitBlock.offset) {
                var afterSecondHashes = consumeWhile(text, afterHashes + 1, isHash);
                suffix = scan['suffix'](text, afterSecondHashes);
                return suffix.success
                    ? scanOk('decimal', firstDigitBlock.value + '.' + suffix.value, suffix.offset)
                    : suffix;
            }

            // Note that the above rule has #+, not #*, so we need at least one.
            else return scanError('decimal', firstDigitBlock.offset, 'expected #');
        }

        // <decimal 10> -> <uinteger 10> <suffix> (i.e. <digit 10>+ #* <suffix>)
        else {
            suffix = scan['suffix'](text, afterHashes);
            return suffix.success
                ? scanOk('decimal', firstDigitBlock.value + suffix.value, suffix.offset)
                : suffix;
        }
    };

// <uinteger R> -> <digit R>+ #*
    scan['uinteger'] = function(base, text, offset) {

        var digits = scan['digits'](base, text, offset);
        if (!digits.success)
            return digits;

        var finalOffset = consumeWhile(text, digits.offset, function(c) {
            return c === '#';
        });

        return scanOk('uinteger', digits.value, finalOffset);
    };


// <prefix R> -> <radix R> <exactness> | <exactness> <radix R>
    scan['prefix'] = function(base, text, offset) {
        var radix1 = scan['radix'](base, text, offset);
        if (radix1.success) {
            var exactness2 = scan['exactness'](text, radix1.offset);
            return exactness2.success
                ? scanOk('prefix', radix1.value + exactness2.value, exactness2.offset)
                : exactness2;
        } else {
            var exactness1 = scan['exactness'](text, offset);
            if (!exactness1.success)
                return exactness1;
            var radix2 = scan['radix'](base, text, offset);
            return radix2.success
                ? scanOk('prefix', exactness1.value + radix2.value, radix2.offset)
                : radix2;
        }
    };

// <suffix> -> <empty> | <exponent marker> <sign> <digit 10>+
    scan['suffix'] = function(text, offset) {

        var marker = scan['exponent-marker'](text, offset);
        if (!marker.success)
            return scanOk('suffix', '', offset);

        var sign = scan['sign'](text, marker.offset);
        if (!sign.success)
            return sign;

        var digits = scan['digits'](10, text, sign.offset);
        if (!digits.success)
            return digits;

        return scanOk('suffix', marker.value + sign.value + digits.value, digits.offset);
    };

// <exponent-marker> -> e | s | f | d | l
    scan['exponent-marker'] = function(text, offset) {
        var allowedMarkers = 'esfdlESFDL';
        var marker = text.charAt(offset);
        if (marker.length !== 1) {
            return eofError('exponent-marker', offset);
        } else if (allowedMarkers.indexOf(marker) === -1) {
            return scanError('exponent-marker', offset, 'invalid exponent-marker ' + marker);
        } else return scanOk('exponent-marker', marker, offset + 1);
    };

// <sign> -> <empty> | + | -
    scan['sign'] = function(text, offset) {
        var maybeSign = text.charAt(offset);
        if (maybeSign.length !== 1)
            return eofError('sign', offset);
        else if ('+-'.indexOf(maybeSign) !== -1)
            return scanOk('sign', maybeSign, offset + 1);
        else return scanOk('sign', '', offset);
    };

// <exactness> -> <empty> | #i | #e
    scan['exactness'] = function(text, offset) {
        var maybeExactness = text.substr(offset, 2);
        if (maybeExactness === '#i'
            || maybeExactness === '#e'
            || maybeExactness === "#I"
            || maybeExactness === "#E")
            return scanOk('exactness', maybeExactness, offset + 2);
        else return scanOk('exactness', '', offset);
    };

    /* <radix 2> -> #b
     <radix 8> -> #o
     <radix 10> -> <empty> | #d
     <radix 16> -> #x
     */
    scan['radix'] = function(base, text, offset) {

        var expected = {2: 'b', 8: 'o', 10: 'd', 16: 'x'};

        var maybeHash = text.charAt(offset);
        if (maybeHash !== '#')
            return base === 10
                ? scanOk('radix', '', offset)
                : scanError('radix', offset, 'expected #');

        var maybeBase = text.charAt(offset + 1);
        if (maybeBase.length !== 1)
            return eofError('radix', offset + 1);
        else if (maybeBase !== expected[base])
            return scanError('radix', offset + 1, 'invalid radix ' + maybeBase);
        else
            return scanOk('radix', base, offset + 2);
    };

    /* <digit 2> -> 0 | 1
     <digit 8> -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
     <digit 10> -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
     <digit 16> -> 0 | 1 | 2 | 3 |4 | 5| 6 | 7 | 8 | 9 | a | b | c | d | e | f
     */
    scan['digits'] = function(base, text, offset, zeroLengthAllowed) {

        var validDigit = function(c) {
            var acceptable = {2: '01',
                8: '01234567',
                10: '0123456789',
                16: '0123456789abcdefABCDEF'};
            return c.length === 1 && acceptable[base].indexOf(c) !== -1;
        };

        var finalOffset = consumeWhile(text, offset, validDigit);
        var numDigits = finalOffset - offset;

        return (zeroLengthAllowed || numDigits > 0)
            ? scanOk('digits', text.substr(offset, numDigits), finalOffset)
            : scanError('digits', offset, 'expected digits-' + base);
    };

    function scanError(type, offset, msg) {
        return {success: false, tokenType: type, offset: offset, msg: msg || 'parse error'};
    }

    function eofError(type, offset) {
        return scanError(type, offset, 'unexpected EOF while parsing ' + type);
    }

    function scanOk(type, value, nextOffset) {
        return {success: true, tokenType: type, value: value, offset: nextOffset}
    }

    /* 7.1.1: "Tokens which require implicit termination (identifiers, numbers,
     characters, and dot) may be terminated by any <delimiter>, but not
     necessarily by anything else." */
    function requireTokenEnd(type, value, text, nextOffset) {
        /* Note that 'blah'.indexOf('') is 0, so this will work for tokens
         ending at the end of a file. */
        return '()"; \t\n'.indexOf(text.charAt(nextOffset)) === -1
            ? scanError(type, nextOffset, 'expected end of token')
            : scanOk(type, value, nextOffset);
    }

    function consumeWhile(text, offset, predicate) {
        while (predicate(text.charAt(offset)))
            ++offset;
        return offset;
    }

    function consumeIntertokenSpace(text, offset) {
        var cur;
        var inComment = false;
        while (true) {
            if ((cur = text.charAt(offset)).length !== 1) {
                return offset;
            } else if (inComment) {
                ++offset;
                if (cur === '\n')
                    inComment = false;
            } else if (cur === ';') {
                ++offset;
                inComment = true;
            } else if (' \t\n'.indexOf(cur) !== -1) {
                ++offset;
            } else return offset;
        }
    }

    function tokenize(raw) {
        var tokens = [];
        var offset = 0;
        var token;
        while (offset < raw.length) {
            token = scan['token'](raw, offset);
            if (token.success) {
                tokens.push(abbrevToken(token));
                offset = consumeIntertokenSpace(raw, token.offset);
            } else return token;
        }
        return tokens;
    }

    function isHash(c) {
        return c === '#';
    }

    return scan['token'](text, consumeIntertokenSpace(text, offset));

};

scanner.runTests = function() {

    // todo bl add negative tests (properly raises errors)

    function abbrevToken(token) {
        return {type: token.tokenType, value: token.value};
    }

    function assertValidToken(text, type) {
        var ans = scanner.nextToken(text, 0);
        if (!ans.success) {
            console.error("parse error on valid token " + text + ': ');
            console.error(ans);
        } else if (type && type !== ans.tokenType) {
            console.error("parse error on " + text + ": expected type " + type
                + ", actual type " + ans.tokenType);
        } else console.log(abbrevToken(ans));
    }

    var validTokens = {
        'identifier': ['h', '+', '-', '...', '!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '~', '_', '^', '&+', 'h+...@@@-.'],
        'character': ['#\\c', '#\\space', '#\\newline', '#\\\\'],
        'string': ['""', '"hello, world"', '" \\" "', '"\\\\"'],
        'boolean': ['#t', '#f']
    };

    validTokens['number'] = (function() {

        var bases = ['', '#b', '#B', '#o', '#O', '#d', '#D', '#x', '#X'];
        var exactnesses = ['', '#e', '#E', '#i', '#I'];

        var prefixes = [];
        for (var i = 0; i < bases.length; ++i) {
            for (var j = 0; j < exactnesses.length; ++j) {
                prefixes.push(bases[i] + exactnesses[j])
                prefixes.push(exactnesses[j] + bases[i]);
            }
        }

        var exponentMarkers = ['e', 's', 'f', 'd', 'l', 'E', 'S', 'F', 'D', 'L'];
        var signs = ['', '+', '-'];

        var suffixes = [''];
        for (var i = 0; i < exponentMarkers.length; ++i)
            for (var j = 0; j < signs.length; ++j)
                suffixes.push(exponentMarkers[i] + signs[j] + "2387");

        var decimals = ["8762",
            "4987566###",
            ".765",
            ".549867#",
            "0.",
            "37.###",
            "565.54",
            "3765.4499##",
            "4##.",
            "56#.",
            "587##.#"];

        var ans = [];
        for (var i = 0; i < decimals.length; ++i)
            for (var j = 0; j < suffixes.length; ++j)
                ans.push(decimals[i] + suffixes[j]);

        return ans;
    })();

    for (var type in validTokens)
        validTokens[type].forEach(function(text) {
            assertValidToken(text, type);
        });

};

//scanner.runTests();


var parse = {};

function isSyntacticKeyword(str) {
    var kws = ['else', '=>', 'define', 'unquote', 'unquote-splicing', 'quote', 'lambda',
        'if', 'set!', 'begin', 'cond', 'and', 'or', 'case', 'let', 'let*', 'letrec', 'do',
        'delay', 'quasiquote'];

    for (var i = 0; i < kws.length; ++i)
        if (str === kws[i])
            return true;

    return false;
}

var tokenStream = new (function(text) {
    this.text = text;
    this.textOffset = 0;
    this.readyTokens = [];
    this.nextTokenToReturn = 0;
    this.next = function() {
        while (this.nextTokenToReturn >= this.readyTokens.length) {
            var token = scanner.nextToken(this.text, this.textOffset);
            this.readyTokens.push(token);
            this.textOffset = token.offset;
        }
        return this.readyTokens[this.nextTokenToReturn++];
    };
    this.putBack = function(tokens) {
        // should never go negative...i hope
        this.nextTokenToReturn -= tokens.length;
    };
    this.flush = function() {
        this.readyTokens = [];
        this.nextTokenToReturn = 0;
    }
})("hello");


/* <expression> -> <variable>
 | <literal>
 | <procedure call>
 | <lambda expression>
 | <conditional>
 | <assignment>
 | <derived expression>
 | <macro use>
 | <macro block>
 */
parse['expression'] = function() {

    var rhses = ['variable', 'literal', 'procedure-call', 'lambda-expression',
        'conditional', 'assignment', 'derived-expression', 'macro-use', 'macro-block'];

    var maybeSuccess;
    for (var i = 0; i < rhses.length; ++i)
        if ((maybeSuccess = parse[rhses[i]](tokenStream)).success)
            return maybeSuccess;

    return maybeSuccess;

};

function parseOk(type, tokens) {
    tokenStream.flush();
    return {success: true, type: type, tokens: tokens};
}

function parseError(type, tokensToPutBack, firstBadToken, msg) {
    tokenStream.putBack(tokensToPutBack);
    return {success: false, type: type, token: firstBadToken, msg: msg || 'parse error'};
}

parse['variable'] = function() {

    var maybeVar = tokenStream.next();
    return (maybeVar.tokenType === 'identifier'
        && !isSyntacticKeyword(maybeVar.value))
        ? parseOk('variable', [maybeVar])
        : parseError('variable', [maybeVar]);
};

console.log(parse['expression']());


