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


goog.provide('r5js.tmp.lazy_boot');

/**
 * @param {!Pipeline} pipeline Pipeline delegate.
 * @param {function()} onBoot Callback that will be called
 * when the interpreter has booted.
 * @implements {r5js.IPipeline}
 * @constructor
 */
function LazyBoot(pipeline, onBoot) {
    /**
     * @type {!Pipeline}
     * @private
     */
    this.pipeline_ = pipeline;

    /**
     * @type {boolean}
     * @private
     */
    this.booted_ = false;

    /**
     * @type {function()}
     * @private
     */
    this.onBoot_ = onBoot;
}

/** @override */
LazyBoot.prototype.setRootEnv = function(rootEnv) {
    return this.pipeline_.setRootEnv(rootEnv);
};

/** @override */
LazyBoot.prototype.scan = function(string) {
    this.checkBooted_();
    return this.pipeline_.scan(string);
};

/** @override */
LazyBoot.prototype.read = function(scanner) {
    this.checkBooted_();
    return this.pipeline_.read(scanner);
};


/** @override */
LazyBoot.prototype.parse = function(root, lhs) {
    this.checkBooted_();
    return this.pipeline_.parse(root, lhs);
};


/** @override */
LazyBoot.prototype.desugar = function(root, replMode) {
    this.checkBooted_();
    return this.pipeline_.desugar(root, replMode);
};


/** @override */
LazyBoot.prototype.Eval = function(continuable, onOutput) {
    this.checkBooted_();
    return this.pipeline_.Eval(continuable, onOutput);
};


/** @private */
LazyBoot.prototype.checkBooted_ = function () {
    /* The actual booting blocks until done. We could change this
     to call a callback when done, but there seems no great need
     for this yet. */
    if (!this.booted_) {
        this.onBoot_();
        this.booted_ = true;
    }
};