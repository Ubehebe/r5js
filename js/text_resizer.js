function TextResizer(element) {
    this.element = element;
    var computedStyle = getComputedStyle(element);
    this.sandbox = document.createElement('span');

    this.sandbox.style.visibility = 'hidden';
    this.sandbox.style.position = 'absolute';

    /* TODO BL: this is clearly inadequate; almost any CSS property
     could affect the width of the sandbox. I experimented a little with
     setting this.sandbox = element.cloneNode(). That seems like the right
     thing to do, but I didn't get it quite right. */

    this.sandbox.style.fontFamily = computedStyle.fontFamily;
    this.sandbox.style.fontSize = computedStyle.fontSize;
    this.sandbox.style.fontStyle = computedStyle.fontStyle;
    this.sandbox.style.fontVariant = computedStyle.fontVariant;
    this.sandbox.style.fontWeight = computedStyle.fontWeight;
    this.sandbox.style.textTransform = computedStyle.textTransform;
    this.sandbox.style.whiteSpace = computedStyle.whiteSpace;

    // todo bl check compatibility -- I think IE might use element.innerText
    this.sandbox.appendChild(document.createTextNode(element.textContent));

    this.curFontSize = 12; // arbitrary; could parse computed style instead

    this.sandbox.style.fontSize = this.curFontSize + 'px';

    document.body.appendChild(this.sandbox);

    var self = this;

    /* Clients may want to manually dispatch a resize event
     to get the UI looking good. We don't do it here because other parts
     of the boot process may want to listen in too. */
    var cb = function() {
        self.resize();
    };

    window.addEventListener
        ? addEventListener('resize', cb, false)
        : attachEvent('onresize', cb);
}

TextResizer.prototype.resize = function () {
    var targetWidth = this.element.getBoundingClientRect().width;
    var tolerance = 0.001; // get within 0.1% of targetWidth
    var numHops = 50;
    var heuristic;

    /* Heuristic: if the box needs to grow by x%, the font size ought to
     grow by x%. This is only a heuristic because making a font x% larger
     (that is, making its height x% larger) doesn't guarantee the width
     of the text increases by x%. Since this is only a heuristic, there are
     no guarantees about termination (imagine a degenerate font that has
     positive height but zero width), so I've kept the numHops bound.

     Note that this is still synchronous and O(n) (just with a smaller
     constant than the naive "plus or minus one pixel" approach),
     so it still may be worth making asynchronous.*/
    do {
        heuristic = targetWidth / this.sandbox.getBoundingClientRect().width;
        this.sandbox.style.fontSize = (this.curFontSize*=heuristic) + 'px';
    } while (numHops-- && Math.abs(heuristic-1.0) > tolerance);

    numHops = 50;

    // For display purposes, it's much better to underfill than to overfill.
    while (--numHops
        && this.sandbox.getBoundingClientRect().width > targetWidth)
        this.sandbox.style.fontSize = (--this.curFontSize) + 'px';

    this.element.style.fontSize = (this.curFontSize) + 'px';

    return this;
};
