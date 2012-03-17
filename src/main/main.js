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

addEventListener('load', function() {
    new MockTerminal(document.getElementById('repl'))
        .println(GayLisp.getMetadata().banner)
        .println(';; Type (tutorial) and press enter for an interactive tutorial.')
        .setPrompt('>> ')
        .pushInterpreter(function(string, terminal) {
            return GayLisp.repl(string, function(sideEffect) {
                terminal.println(sideEffect);
            });
        })
        .pushInterpreter(tutorial)
        .setInputCompleteHandler(GayLisp.willParse)
        .start();

    var anchors = document.querySelectorAll('a');
    var anchor;
    for (var i=0; i<anchors.length; ++i) {
        anchor = anchors[i];
        if (anchor.hash.charAt(0) === '#'
        && document.getElementById(anchor.hash.substr(1))) {
            anchor.addEventListener('click', function(e) {
                var pleaseNotIE = document.getElementById(e.target.hash.substr(1)).classList;
                if (pleaseNotIE)
                    pleaseNotIE.toggle('on');
            });
        }
    }

});