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

/* todo bl: a more flexible design would be to specify a latency for every
 object enqueued. */
function AsyncQueue(latency) {
    this.queue = [];
    this.latency = latency;
    this.running = false;
}

AsyncQueue.prototype.isRunning = function() {
    return this.running;
};

AsyncQueue.prototype.enqueue = function(cb) {
    this.queue.push(cb);
    var self = this;
    if (!this.running) {
        setTimeout(function() { self.run(); }, this.latency);
        this.running = true;
    }
    return this;
};

AsyncQueue.prototype.dequeue = function() {
    return this.queue.shift();
};

AsyncQueue.prototype.run = function() {
  var self = this;
    var todo = this.dequeue();
    if (todo) {
        todo();
        setTimeout(function() { self.run(); }, this.latency);
    } else {
        this.running = false;
    }
};