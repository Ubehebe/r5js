(function() {

    document.addEventListener
        ? document.addEventListener('DOMContentLoaded', main, false)
        : window.attachEvent('onload', main);

function main() {

    asyncLoadSpec();
    setupTerminal();
    setupColors();

    function setupTerminal() {
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
                startOn = '#repl';
                break;
            // Fragment corresponding to one of the rotary nav items: start at that item
            case '#repl':
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
        new RotaryNav(document.getElementById('logo'), 40, 90, -90)
            .setTransitionSpeed(0.5)
            .registerNodes(document.getElementById('navlist').children)
            .setSelectClass('selected')
            .rotateElementToFront(document.querySelector('a[href="' + startOn + '"]').parentElement);
    }

    function setupBackLinks() {

        var h1s = document.querySelectorAll('section > h1');

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
        new TextResizer(document.getElementById('hero-top'));
        new TextResizer(document.getElementById('hero'));
        new TextResizer(document.getElementById('about-hero'));
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
            button.addEventListener('click', function (e) {
                var output;
                try {
                    output = GayLisp.repl(e.target['data-input']);
                } catch (x) {
                    output = x.toString();
                }

                e.target.parentElement.replaceChild(
                    document.createTextNode('⇒ ' + output),
                    e.target);
            });
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