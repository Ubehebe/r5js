/* Copyright 2012 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */

function RotaryNav(centerElement, radius, fromDegree, toDegree) {
    this.centerElement = centerElement;
    this.radius = radius;
    this.fromDegree = fromDegree;
    this.toDegree = toDegree;
    this.elements = [];
    this.hashes = {};

    function recenter(self) {
        var box = self.centerElement.getBoundingClientRect();
        self.centerX = box.left + box.width/2;
        self.centerY = box.top + box.height/2;
    }

    var self = this;

    /* Clients may want to manually dispatch a resize event
     to get the UI looking good. We don't do it here because other parts
     of the boot process may want to listen in too. */
    var onResize = function() {
        recenter(self);
        for (var i=0; i<self.elements.length; ++i)
            self.elements[i].setPosition(self.centerX, self.centerY);
    };

    var onHashChange = function() {
        /* todo bl: location.hash is a vector for client-side XSS.
         I believe my usage is safe, because rotateToFront will do nothing
         when given a hash that it hasn't previously been informed of
         (and in any case, RotaryNav doesn't add to the DOM). But I should
         double-check, as I have little experience with XSS. */
        self.rotateToFront(location.hash);
    };

    if (window.addEventListener) {
        addEventListener('resize', onResize, false);
        addEventListener('hashchange', onHashChange, false);
    } else {
        attachEvent('onresize', onResize);
        attachEvent('onhashchange', onHashChange);
    }
}

RotaryNav.prototype.setSelectClass = function(cssClass) {
    this.selectedItemClass = cssClass;
    return this;
};

RotaryNav.prototype.setTransitionSpeed = function(seconds) {
    this.transitionSpeed = seconds;
    return this;
};

/* RotaryNav listens for the hashchange event, not click events, in order
 to integrate with the browser history and Back button. For example, suppose
 the RotaryNav has a list item that contains a link to an FAQ section, but
 other sections on the page also have links to the FAQ section. When any
 of those links is clicked, the RotaryNav should rotate to the FAQ list item
 around the same time the page is scrolling to the FAQ section. */
RotaryNav.prototype.push = function(element, hashToListenFor) {
    element.style.position = 'fixed';
    element.style.visibility = 'visible';
    var index = this.elements.length;
    if (!(hashToListenFor in this.hashes))
        this.hashes[hashToListenFor] = index;

    this.elements.push(new TransformHelper(element)
        .setPosition(this.centerX, this.centerY)
        .setTransitionSpeed(this.transitionSpeed || 0)
        .setPermanentTransformOrigin('center right')
        .setPermanentTransform('translate(-' + this.radius + 'px)'));
    return this.recalculateAngles();
};

RotaryNav.prototype.rotateToFront = function(hash) {
    var index = this.hashes[hash];
    if (index != null) {
        var which = this.elements[index];
        var offset = which.rot;
        for (var i = 0; i < this.elements.length; ++i)
            this.elements[i].incRot(-offset).removeClass(this.selectedItemClass);
        this.elements[index].addClass(this.selectedItemClass);
    }
    return this;
};

RotaryNav.prototype.recalculateAngles = function() {
    var offset = (this.toDegree-this.fromDegree)/this.elements.length;
    for (var i=0; i<this.elements.length; ++i)
        this.elements[i].setRot(this.fromDegree + i*offset);
    return this;
};

// Save the current rotation to avoid parsing CSS to recover it
function TransformHelper(element) {
    this.element = element;
    var bounding = element.getBoundingClientRect();
    this.w = bounding.width;
    this.h = bounding.height;
    this.rot = 0;
}

TransformHelper.prototype.getElement = function() {
    return this.element;
};

TransformHelper.prototype.setPosition = function(x,y) {
    /* Since these elements have transformOrigin 'center right',
     we have to subtract the width and half the height to get back to
     the top left of the element.

     The width and height are cached at instantiation time because rotations
     seem to change them. */
    this.element.style.left = (x - this.w) + 'px';
    this.element.style.top = (y - this.h/2) + 'px';
    return this;
};

TransformHelper.prototype.setPermanentTransform = function(str) {
    this.permanentTransform = str;
    return this;
};

TransformHelper.prototype.setPermanentTransformOrigin = function(str) {
    this.element.style.transformOrigin
        = this.element.style.webkitTransformOrigin
        = this.element.style.MozTransformOrigin
        = this.element.style.OTransformOrigin
        = str;
    return this;
};

TransformHelper.prototype.setTransitionSpeed = function(seconds) {
    this.element.style.transition
        = this.element.style.webkitTransition
        = this.element.style.MozTransition
        = this.element.style.OTransition
        = 'all ' + seconds + 's ease-in-out'; // todo bl: 'all' may be too coarse
    return this;
};

TransformHelper.prototype.reapply = function() {
    this.element.style.transform
        = this.element.style.webkitTransform
        = this.element.style.MozTransform
        = this.element.style.OTransform
        = 'rotate(' + this.rot + 'deg)' + (this.permanentTransform ? ' ' + this.permanentTransform : '');
    return this;
};

// The UI would look silly if we took the "long way" around the dial.
TransformHelper.prototype.normalizeAngle = function(angle) {
     while (angle < -180)
         angle += 360;
     while (angle > 180)
         angle -= 360;
     return angle;
 };

TransformHelper.prototype.setRot = function(rot) {
    this.rot = this.normalizeAngle(rot);
    return this.reapply();
};

TransformHelper.prototype.incRot = function(delta) {
    this.rot += this.normalizeAngle(delta);
    return this.reapply();
};

TransformHelper.prototype.addClass = function(cssClass) {
    if (cssClass) {
        if (this.element.classList)
            this.element.classList.add(cssClass);
        else {
            /* As of early 2012, all major browsers support classList except
             for IE 9. Ugh!. */
            var curClasses = this.element.className;
            if (curClasses.search('\\b' + cssClass + '\\b') === -1)
                this.element.className += ' ' + cssClass;
        }
    }
    return this;
};

TransformHelper.prototype.removeClass = function(cssClass) {
    if (cssClass) {
        if (this.element.classList)
            this.element.classList.remove(cssClass);
        else {
            /* As of early 2012, all major browsers support classList except
             for IE 9. Ugh!. */
            var pattern = new RegExp('\\b' + cssClass + '\\b\\s*', 'g');
            this.element.className = this.element.className.replace(pattern, '');
        }
    }
    return this;
};
