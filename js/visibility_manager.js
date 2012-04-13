// There's probably something in jQuery to do this...oh well.
function VisibilityManager() {
    this.elements = [];
    this.managedHashes = {};

    var self = this;

    var cb = function() {
        /* todo bl: location.hash is a classic XSS vector.
         I believe my use of it is safe, since the VisibilityManager ignores
         hashes it hasn't been previously informed of, but my experience
         with XSS is limited. */
        var rawHash = location.hash;
        if (self.managedHashes[rawHash])
            self.bringToFront(document.querySelector(rawHash));
    };

    window.addEventListener
        ? addEventListener('hashchange', cb, false)
        : attachEvent('onhashchange', cb);
}

VisibilityManager.prototype.registerAnchors = function(array) {
    for (var i=0; i< array.length; ++i)
        this.pushAnchor(array[i]);
    return this;
};

VisibilityManager.prototype.pushAnchor = function(a) {
    var target = a.hash.length && document.querySelector(a.hash);
    if (target) {
        this.managedHashes[a.hash] = true;
        this.pushIfAbsent(target);
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