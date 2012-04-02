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
    this.radius = radius;
    this.fromDegree = fromDegree;
    this.toDegree = toDegree;
    this.elements = [];
}

RotaryNav.prototype.setTransitionSpeed = function(seconds) {
    this.transitionSpeed = seconds;
    return this;
};

RotaryNav.prototype.push = function(element) {
    element.style.display = 'inline-block';
    element.style.position = 'fixed';
    element.style.left = (this.centerX - element.getBoundingClientRect().width) + 'px';
    element.style.top = this.centerY + 'px';
    element.style.visibility = 'visible';

    var self = this;
    var index = this.elements.length;
    element.addEventListener('click', function () {
        self.rotateToFront(index);
    });

    this.elements.push(new TransformHelper(element)
        .setTransitionSpeed(this.transitionSpeed || 0)
        .setPermanentTransformOrigin('top right')
        .setPermanentTransform('translate(-' + this.radius + 'px)'));
    return this.recalculateAngles();
};

/* The rotary nav usually rotates when the user clicks on an item,
 but we expose this method in case another client wants to rotate the
 nav to some desired element. */
RotaryNav.prototype.rotateElementToFront = function(element) {
    console.log('rotate to front: ');
    console.log(element);
    for (var i=0; i<this.elements.length; ++i) {
        if (this.elements[i].getElement() === element) {
            console.log('got match');
            return this.rotateToFront(i);
        }
    }
};

RotaryNav.prototype.rotateToFront = function(index) {

    var which = this.elements[index];
    var offset = which.rot;
    for (var i=0; i<this.elements.length; ++i)
        this.elements[i].incRot(-offset);
    return this;
};

RotaryNav.prototype.recalculateAngles = function() {
    var offset = (this.toDegree-this.fromDegree)/this.elements.length;
    for (var i=0; i<this.elements.length; ++i)
        this.elements[i].setRot(this.fromDegree + i*offset);
    return this;
};

RotaryNav.prototype.registerNodes = function(nodeArray) {

    for (var i=0; i<nodeArray.length; ++i)
        this.push(nodeArray[i]);

    return this;
};

// Save the current rotation to avoid parsing CSS to recover it
function TransformHelper(element) {
    this.element = element;
    this.rot = 0;
}

TransformHelper.prototype.getElement = function() {
    return this.element;
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
        = 'all ' + seconds + 's ease-in-out';
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
