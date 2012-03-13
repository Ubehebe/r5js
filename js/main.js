function load() {

    // Setup the terminal.
    var terminal = new MockTerminal(document.getElementById('repl'))
        .setPrompt('>> ')
        .setBanner(GayLisp.getMetadata().banner)
        .setInterpreter(function (string) {
            return GayLisp.repl(string, function (sideEffect) {
                terminal.print('\n' + sideEffect);
            });
        })
        .setInputCompleteHandler(GayLisp.willParse)
        .start();

    /* Lay out the divs like a pile of index cards thrown somewhat
     randomly on a table. */
    var zindexManager = new ZIndexManager();
    var sections = document.querySelectorAll('section');
    for (var i = 0; i < sections.length; ++i) {
        var s = sections[i];
        s.style.top = ((Math.random() * 50) + 25) + '%';
        s.style.left = ((Math.random() * 25) + 25) + '%';
//        s.style.width = '50%';
        s.style.transform
            = s.style.WebkitTransform /* or webkitTransform */
            = s.style.MozTransform /* case sensitive */
            = s.style.OTransform /* case sensitive */
            = s.style.msTransform /* case sensitive */
            = 'rotate(' + ((Math.random() * 40) - 20) + 'deg)';
        zindexManager.push(s);
    }

    zindexManager.push(document.getElementById('repl'));

    /* Fragment links to the divs should cause the same pop-to-front
     behavior as manually clicking on the divs. */
    var anchors = document.querySelectorAll('a');
    for (var i = 0; i < anchors.length; ++i)
        registerAnchor(anchors[i]);

    function registerAnchor(anchor) {
        /* We get the plain text of the href to avoid looking at
         external links that happen to have a fragment. Oddly, writing
         anchor.href will give us the "processed" link (for example
         http://localhost/#foo instead of #foo) which is not what we want. */
        var text = anchor.getAttribute('href');
        var fragment;
        if (text && text.charAt(0) === '#'
            && (fragment = document.getElementById(text.substr(1)))) {
            anchor.addEventListener('click', function (event) {
                zindexManager.bringToFront(fragment);
                event.stopPropagation();
            });
        }
    }

    /* If the browser is trying to navigate directly to a fragment, make sure
    it's on top. Otherwise index on top. */
    zindexManager.bringToFront(
        document.getElementById(
            document.location.hash.substr(1))
            || document.getElementById('index'));
}