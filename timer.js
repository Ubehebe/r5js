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

