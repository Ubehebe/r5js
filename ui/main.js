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

(function() {

    window.addEventListener
        ? addEventListener('load', main, false)
        : attachEvent('onload', main);

    function main() {
        new MockTerminal(document.getElementById('repl'), 80, 5, 500)
            .println(GayLisp.getMetadata().banner)
            .println(';; Type (tutorial) and press enter for an interactive tutorial.')
            .setPrompt('>> ')
            .pushInterpreter(function (string, terminal) {
                return GayLisp.repl(string, function (sideEffect) {
                    terminal.println(sideEffect);
                });
            })
            .pushInterpreter(tutorial)
            .setInputCompleteHandler(GayLisp.willParse)
            .start();
    }
}());