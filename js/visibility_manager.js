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
        this.elements.push(target);
        var self = this;
        var shouldPreventDefault = a.className.indexOf('prevent-default') !== -1;
        a.addEventListener('click', function(e) {
            self.bringToFront(target);
            if (shouldPreventDefault)
                e.preventDefault();
        });
    }
};

VisibilityManager.prototype.bringToFront = function (element) {
    for (var i = 0; i < this.elements.length; ++i)
        this.elements[i].style.visibility = (this.elements[i] === element)
            ? 'visible'
            : 'hidden';
};