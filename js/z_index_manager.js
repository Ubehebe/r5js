// There's probably something in jQuery to do this...oh well.
function ZIndexManager() {
    this.elements = [];
}

ZIndexManager.prototype.push = function (element) {
    element.style.zIndex = String(this.elements.length);
    this.elements.push(element);

    var self = this;

    element.addEventListener('click', function () {
        self.bringToFront(element);
    });

    return this;
};

ZIndexManager.prototype.bringToFront = function (element) {
    var i = this.elements.indexOf(element);
    if (i !== -1) {
        this.elements.splice(i, 1);
        this.elements.push(element);
        for (i = 0; i < this.elements.length; ++i)
            this.elements[i].style.zIndex = String(i);
    }
};