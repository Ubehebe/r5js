// There's probably something in jQuery to do this...oh well.
function ZIndexManager() {
    this.elements = [];
}

ZIndexManager.prototype.registerAnchors = function(array) {
    for (var i=0; i< array.length; ++i)
        this.pushAnchor(array[i]);
    return this;
};

ZIndexManager.prototype.pushAnchor = function(a) {
    var target = a.hash.length && document.querySelector(a.hash);
    if (target) {
        target.style.zIndex = String(this.elements.length);
        this.elements.push(target);
        var self = this;
        a.addEventListener('click', function() {
            self.bringToFront(target);
        });
    }
};

ZIndexManager.prototype.bringToFront = function (element) {
    var i = this.elements.indexOf(element);
    if (i !== -1) {
        this.elements.splice(i, 1);
        this.elements.push(element);
        for (i = 0; i < this.elements.length; ++i) {
            this.elements[i].style.zIndex = String(i);
            this.elements[i].style.visibility = 'visible';
        }
    }
};