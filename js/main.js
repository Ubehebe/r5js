addEventListener('load', function() {

    // Set up the terminal
    new MockTerminal(document.getElementById('repl'), 80, 5, 500)
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

    // Set up the layers on the page
    new VisibilityManager()
        .registerAnchors(document.querySelectorAll('#navlist a'))
    .bringToFront(document.querySelector(startOn));

    // Set up the rotary-phone-like nav thing in the corner
    new RotaryNav(document.getElementById('logo'), 50, 45, -45)
        .setTransitionSpeed(0.5)
        .registerNodes(document.getElementById('navlist').children)
        .rotateElementToFront(document.querySelector('a[href="' + startOn + '"]').parentElement);

    var h1s = document.querySelectorAll('section > h1');

    for (var i = 0; i < h1s.length; ++i) {
        var a = document.createElement('a');
        a.href = '#contents';
        a.appendChild(document.createTextNode('⇧'));
        h1s[i].parentElement.insertBefore(a, h1s[i].nextElementSibling);
    }

    var colors = ['red', 'orange', 'yellow', 'green', 'blue', 'indigo', 'violet'];

    function changeColor(i) {
        document.getElementById('logo').style.color = colors[i];
        setTimeout(changeColor, 10000, (i+1) % colors.length);
    }

    setTimeout(changeColor, 10000, 0);
});