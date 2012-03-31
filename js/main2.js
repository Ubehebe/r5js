addEventListener('load', function() {
    new RotaryNav(document.getElementById('nav'), 15, -15)
        .setTransitionSpeed(1)
        .registerNodes(document.getElementById('navlist').children);
    });

addEventListener('load', function() {
    var h1s = document.querySelectorAll('section > h1');

    for (var i=0; i < h1s.length; ++i) {
        var a = document.createElement('a');
        a.href = '#contents';
        a.appendChild(document.createTextNode('⇧'));
        h1s[i].parentElement.insertBefore(a, h1s[i].nextElementSibling);
    }
});
