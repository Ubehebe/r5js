/* Copyright 2011, 2012 Brendan Linn

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

function Timer() {
    this.names = [];
    this.startingTimes = [];
    this.stopTime = null;
}

Timer.prototype.start = function(name) {
    if (!this.suspended) {
        this.names.push(name);
        this.startingTimes.push(new Date());
        return this;
    }
};

Timer.prototype.stop = function() {
    if (!this.suspended) {
        this.stopTime = new Date();
        return this;
    }
};

Timer.prototype.reset = function() {
    this.names = [];
    this.startingTimes = [];
    this.stopTime = null;
};

Timer.prototype.suspend = function() {
    this.suspended = true;
    return this;
};

Timer.prototype.unsuspend = function() {
    this.suspended = false;
    return this;
};

Timer.prototype.report = function() {
    var elapsed = [];
    var total = 0;
    var delta;
    for (var i=0; i<this.names.length-1; ++i) {
        delta = this.startingTimes[i+1]-this.startingTimes[i];
        elapsed.push(delta);
        total += delta;
    }

    delta = this.stopTime-this.startingTimes[i];
    elapsed.push(delta);
    total += delta;

    var ans = 'name\t\ttime\tpercent\n';

    for (var i=0; i < this.names.length; ++i)
        ans += this.names[i] + '\t\t' + elapsed[i] + '\t' + (elapsed[i] * 100 / total) + '\n';

    return ans;
};

function FakeTimer() {}

FakeTimer.prototype.start
    = FakeTimer.prototype.stop
    = FakeTimer.prototype.reset
    = FakeTimer.prototype.suspend
    = FakeTimer.prototype.unsuspend
    = FakeTimer.prototype.report
    = function() {};