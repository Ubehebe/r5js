function nextToken(text, offset) {

    var maybeToken = text[offset];

    if (' \r\n\t'.indexOf(maybeToken) !== -1)
        return nextToken(text, offset + 1);
    else if ("()'`,.".indexOf(maybeToken) !== -1)
        return {success: true, token: {type: maybeToken}, offset: offset + 1};
    else if ((maybeToken = text.substr(offset, 2)) === '#(' || maybeToken === ',@')
        return {success: true, token: {type: twoCharToken}, offset: offset + 2};
    else {

        var ans;

        if ((ans = nextIdentifier(text, offset)).success)
            return ans;
        else if ((ans = nextBoolean(text, offset)).success)
            return ans;
        else if ((ans = nextNumber(text, offset)).success)
            return ans;
        else if ((ans = nextCharacter(text, offset)).success)
            return ans;
        else return nextString(text, offset);
    }
}

function nextNumber(text, offset) {
    var bases = [10,16,8,2];
    var ans;
    for (var i = 0; i < bases.length; ++i)
        if ((ans = parseNumber(bases[i], text, offset).success))
            return ans;
    return {success: false, offset: offset, msg: 'invalid number'};
}

function parseNumber(base, text, offset) {

    parseNumberPrefix(base, text, offset);

}

function parseNumberPrefix(text, offset) {

    var allowedInPrefix = '#iexodb';
    var maxPrefixLen = 4;
    for (var i = 0; i < maxPrefixLen; ++i) {
        if (allowedInPrefix.indexOf(text.charAt(offset + i)) === -1) {
            prefixLen = i;
            break;
        }
    }

    var props = {base: 10, exact: false};

    switch (prefixLen) {
        case 0:
            return {success: true, token: {type: 'number', value: props}, offset: offset};
        case 1:
        case 3:
            return {success: false, offset: offset, msg: 'parse error in number prefix'};
        case 4:
            captureProp(props, text.substr(offset+2,2));
            // fall through
        case 2:
            captureProp(props, text.substr(offset, 2));
            return {success: true, token: {type: 'number', value: props}, offset: offset + prefixLen};
    }

    function captureProp(existingProps, twoChars) {

    }


    var prefix = text.substr(offset, 4);

    var ans = {};

    var bases = {'#x': 16, '#o': 8, '#d': 10, '#b': 2};
    var exactnesses = {'#i': false, '#e': true};

    for (var radix in bases) {
        if (prefix.indexOf(radix) !== -1) {
            if (!ans.base)
                ans.base = bases[radix];
            else
                return {success: false, offset: offset, msg: 'number radix already defined'};
        }
    }

    for (var exact in exactnesses) {
        if (prefix.indexOf(exact) !== -1) {
            if (!ans.exactness)
                ans.exactness = exactnesses[exact];
            else
                return {success: false, offset: offset, msg: 'number exactness already defined'};
        }
    }

    return {success: true, token: {type: 'number', value: ans}, offset, }

}

function parseNumberExactness(text, offset) {
    var maybeExactness = text.substr(offset, 2);
    return maybeExactness === '#i'
        || maybeExactness === '#e'
        || maybeExactness.charAt(0) !== '#'; // not correct
}

function parseNumberRadix(base, text, offset) {
    var maybeRadix = text.substr(offset, 2);
    if (base == 10 && (maybeRadix.charAt(0) !== '#' || maybeRadix === '#d'))
        return true;
    else if (base === 16 && maybeRadix === '#x')
        return true;
    else if (base === 8 && maybeRadix === '#o')
        return true;
    else if (base === 2 && maybeRadix === '#b')
        return true;
    else
        return false;
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
    if (text[offset] !== '"')
        return {success: false, offset: offset, msg: 'expected "'};

    var len = 0;
    while (validStringElement(text, offset + ++len))
        ;

    return (text[offset + len] === '"')
        ? {success: true, token: {type: 'string', value: text.substr(offset + 1, offset + len - 1)}, offset: offset + len}
        : {success: false, offset: offset, msg: 'unterminated string literal'};


    function validStringElement(text, offset) {
        var cur = text[offset];
        if (cur && cur !== '"' && cur !== '\\')
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
            return {success: true, token: {type: 'bool', value: maybeBool}, offset: offset + 2};
        default:
            return {success: false, offset: offset, msg: 'expected #t or #f'};
    }
}

function nextIdentifier(text, offset) {
    var ans;

    if ((ans = peculiarId(text, offset)).success)
        return ans;

    var initial = text[offset];

    if (!validInitial(initial))
        return {success: false, offset: offset, msg: 'invalid identifier'};

    var len = 0;

    while (validSubsequent(text[offset + ++len]))
        ;

    return {success: true, token: {type: 'id', value: text.substr(offset, len)}, offset: offset + len};

    function peculiarId(text, offset) {
        if (text[offset] === '+' || text[offset] === '-')
            return {success: true, token: {type: 'id', value: text[offset]}, offset: offset + 1};
        else if (text.substr(offset, 3) === '...')
            return {success: true, token: {type: 'id', value: '...'}, offset: offset + 3};
        else
            return {success: false, offset: offset, msg: 'invalid peculiar-identifier'};
    }

    function validInitial(c) {
        return (c >= 'a' && c <= 'z') || '!$%&*/:<=>?^_~'.indexOf(c) !== -1;
    }

    function validSubsequent(c) {
        return validInitial(c) || (c >= '0' && c <= '9') || '+-.@'.indexOf(c) !== -1;
    }
}
