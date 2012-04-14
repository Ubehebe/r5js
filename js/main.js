(function() {

    /* Redirect to mobile site.
     As of early 2012, the fanciest mobile device is the "new iPad" with
     a resolution of 2048x1536. However, Safari on that device reports
     a resolution of 1024x768 on purpose, so we're safe for now. Note that
     screen real estate is not the only reason to switch to the mobile site;
     the MockTerminal implementation puts every character on a setTimeout
     to achieve a "old, high-latency terminal" effect, and this really slows
     down mobile devices, even iPads.

     We also redirect IE<9 to the mobile site, since I haven't yet made the
     terminal UI compatible with them. */
    if ((window.screen && window.screen.width < 800)
        || window.attachEvent)
        location.replace('//m.gay-lisp.org');

    document.addEventListener
        ? document.addEventListener('DOMContentLoaded', main, false)
        : window.attachEvent('onload', main);

function main() {

    asyncLoadSpec();
    setupTerminal();
    setupColors();

    function setupTerminal() {
        new MockTerminal(document.getElementsByClassName('terminal')[0], 80, 5, 500)
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

    function asyncLoadSpec() {
        var req = new XMLHttpRequest();
        req.open('GET', 'r5rs.xhtml');
        req.onreadystatechange = function() {
            if (req.readyState === 4 && req.status === 200) {
                document.body.appendChild(req.responseXML.getElementById('spec'));
                setupSpecExamples();
                setupBackLinks();
                setupHeroText();
                setupNav();
                // To get the nav and hero text looking good
                manualResize();
            }
        };
        req.send();
    }

    function setupNav() {
        var startOn;
        switch (document.location.hash) {
            // No fragment: start at the REPL
            case '':
                startOn = '#play';
                break;
            // Fragment corresponding to one of the rotary nav items: start at that item
            case '#play':
            case '#spec':
            case '#about':
                startOn = document.location.hash;
                break;
            // Other fragment: it's inside the spec, start there.
            default:
                startOn = '#spec';
                break;
        }
        new VisibilityManager()
            .registerAnchors(document.querySelectorAll('.vis-manager'))
            .bringToFront(document.querySelector(startOn));

        // Set up the rotary-phone-like nav thing in the corner
        var rotaryNav = new RotaryNav(document.getElementById('logo'), 40, 60, -60)
            .setTransitionSpeed(0.5)
            .setSelectClass('selected');

        var lis = document.getElementById('navlist').children;
        for (var i = 0; i < lis.length; ++i)
            rotaryNav.push(lis[i], lis[i].getElementsByTagName('a')[0].hash);

        rotaryNav.rotateToFront(startOn);
    }

    function setupBackLinks() {

        var h1s = document.querySelectorAll('#spec section > h1');

        // Decorate the major headings with links back to the top
        for (var i = 0; i < h1s.length; ++i) {
            var a = document.createElement('a');
            a.href = '#contents';
            a.appendChild(document.createTextNode('⬆'));
            h1s[i].parentElement.insertBefore(a, h1s[i].nextElementSibling);
        }
    }

    function setupColors() {

        // Make the logo in the rotary nav thing change colors intermittently.
        var colors = [
            'rgba(255,0,0,0.8)', // red
            'rgba(0,0,0,0)',
            'rgba(255,165,0,0.8)', // orange
            'rgba(0,0,0,0)',
            'rgba(255,255,0,0.8)', // yellow
            'rgba(0,0,0,0)',
            'rgba(0,255,0,0.8)', // green
            'rgba(0,0,0,0)',
            'rgba(0,0,255,0.8)', // blue
            'rgba(0,0,0,0)',
            'rgba(75,0,130,0.8)', // indigo
            'rgba(0,0,0,0)',
            'rgba(238,130,238,0.8)', // violet
            'rgba(0,0,0,0)'];

        function changeColor(i) {
            document.getElementById('logo').style.backgroundColor = colors[i];
            setTimeout(changeColor, 10000, (i + 1) % colors.length);
        }

        setTimeout(changeColor, 10000, 0);
    }

    function setupHeroText() {
        var heroes = document.getElementsByClassName('hero');
        for (var i=0; i<heroes.length; ++i)
            new TextResizer(heroes[i]);
    }

    function setupSpecExamples() {

        // Replace the answers in the spec with buttons to call the interpreter
        var qs = document.querySelectorAll('.ex dt');

        for (var i = 0; i < qs.length; ++i) {
            var question = qs[i];
            var input = question.textContent;
            var answer = question.nextElementSibling; // <dd>
            var button = document.createElement('button');
            button['type'] = 'button';
            button.className = 'evalButton';
            button['data-input'] = input;
            var cb = function(e) {
                var output;
                try {
                    output = GayLisp.repl(e.target['data-input']);
                } catch (x) {
                    output = x.toString();
                }

                e.target.parentElement.replaceChild(
                    document.createTextNode('⇒ ' + output),
                    e.target);
            };
            button.addEventListener
                ? button.addEventListener('click', cb, false)
                : button.attachEvent('onclick', cb);
            button.appendChild(document.createTextNode('eval'));
            answer.replaceChild(button, answer.firstChild);
        }
    }

    function manualResize() {
        var e = document.createEvent('Event');
        e.initEvent('resize', false, false);
        dispatchEvent(e);
    }
}
}());