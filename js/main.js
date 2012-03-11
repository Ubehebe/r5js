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
    var cards = document.querySelectorAll('section');
    for (var i = 0; i < cards.length; ++i) {
        var card = cards[i];
        card.style.top = ((Math.random() * 50) + 25) + '%';
        card.style.left = ((Math.random() * 25) + 25) + '%';
        card.style.width = '50%';
        card.style.transform
            = card.style.WebkitTransform /* or webkitTransform */
            = card.style.MozTransform /* case sensitive */
            = card.style.OTransform /* case sensitive */
            = card.style.msTransform /* case sensitive */
            = 'rotate(' + ((Math.random() * 40) - 20) + 'deg)';
        if (card.id !== 'index')
            zindexManager.push(card);
    }

    // The index div is topmost, followed by the terminal itself.
    zindexManager.push(document.getElementById('repl'))
        .push(document.getElementById('index'));

    /* Fragment links to the divs should cause the same pop-to-front
     behavior as manually clicking on the divs. */
    var anchors = document.querySelectorAll('a');
    for (var i = 0; i < anchors.length; ++i)
        registerAnchor(anchors[i]);

    function registerAnchor(anchor) {
        var fragment;
        if (anchor.hash
            && anchor.pathname === '/'
            && (fragment = document.getElementById(anchor.hash.substr(1)))) {
            anchor.addEventListener('click', function (event) {
                zindexManager.bringToFront(fragment);
                event.stopPropagation();
            });
        }
    }

    /* If the browser is trying to navigate directly to a fragment, make sure
    it's on top. */
    zindexManager.bringToFront(
        document.getElementById(
            document.location.hash.substr(1)));
}