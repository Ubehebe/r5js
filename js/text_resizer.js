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
    addEventListener('resize', function() {
        self.resize();
    });

    self.resize();
}

TextResizer.prototype.resize = function () {
    var targetWidth = this.element.getBoundingClientRect().width;

    var tol = 10000;

    while (this.sandbox.getBoundingClientRect().width < targetWidth && tol-- > 0) {
        this.sandbox.style.fontSize = (++this.curFontSize) + 'px';
    }
    while (this.sandbox.getBoundingClientRect().width > targetWidth && tol-- > 0)
        this.sandbox.style.fontSize = (--this.curFontSize) + 'px';

    this.element.style.fontSize = (this.curFontSize-1) + 'px';

    return this;
};
