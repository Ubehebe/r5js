/* Copyright 2011, 2012 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */

var readline = require('readline');
var rl = readline.createInterface(process.stdin, process.stdout);
var normalPrompt = '>> ';
var needsMoreInputPrompt = '... ';

var linebuf = '';

rl.on('line',
    function (line) {
        linebuf += '\n' + line;
        if (GayLisp.willParse(linebuf)) {
            try {
                console.log(GayLisp.repl(linebuf, function (sideEffect) {
                    console.log(sideEffect);
                }));
            } catch (x) {
                console.log(String(x));
            }
            linebuf = '';
            rl.setPrompt(normalPrompt, normalPrompt.length);
        } else {
            rl.setPrompt(needsMoreInputPrompt, needsMoreInputPrompt.length);
        }
        rl.prompt();
    }).on('close', function () {
        process.exit(0);
    });
console.log(GayLisp.getMetadata().banner);
rl.setPrompt(normalPrompt, normalPrompt.length);
rl.prompt();