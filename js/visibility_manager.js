// There's probably something in jQuery to do this...oh well.
function VisibilityManager() {
    this.elements = [];
}

VisibilityManager.prototype.registerAnchors = function(array) {
    for (var i=0; i< array.length; ++i)
        this.pushAnchor(array[i]);
    return this;
};

VisibilityManager.prototype.pushAnchor = function(a) {
    var target = a.hash.length && document.querySelector(a.hash);
    if (target) {
        /* If it has a data-target, don't add the actual target to
         the visibility manager. The use case is a link to an element that's
         inside an element tracked by the visibility manager. */
        var altTarget = a.getAttribute('data-target');
        if (!altTarget)
            this.pushIfAbsent(target);
        var self = this;
        var shouldPreventDefault = a.getAttribute('data-prevent-default') === 'true';
        if (altTarget)
            target = document.querySelector(altTarget);
        var cb = function(e) {
            self.bringToFront(target);
            if (shouldPreventDefault)
                e.preventDefault();
        };
        a.addEventListener
            ? a.addEventListener('click', cb, false)
            : a.attachEvent('click', cb);
    }
};

VisibilityManager.prototype.pushIfAbsent = function(element) {
    for (var i=0; i<this.elements.length; ++i)
        if (this.elements[i] === element)
            return this;
    this.elements.push(element);
    return this;
};

VisibilityManager.prototype.bringToFront = function (element) {
    for (var i = 0; i < this.elements.length; ++i)
        this.elements[i].style.visibility = (this.elements[i] === element)
            ? 'visible'
            : 'hidden';
};