goog.module('r5js.macro.RenameHelper');

const {isParserSensitiveId} = require('/js/parse/rename_util_collect_es6_sources.es6/node_modules/__main__/js/parse/rename_util');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');

class RenameHelper {
    /** @param {string} transformerName */
    constructor(transformerName) {
        /** @const @private */ this.transformerName_ = transformerName;
        /** @const @private {!Object<string, number> } */ this.namesToEllipsisLevels_ = {};
        /** @const @private {!Object<string, boolean>} */ this.renameCandidates_ = {};
    }

    /**
     * @param {string} name
     * @param {number} ellipsisLevel
     */
    recordPatternId(name, ellipsisLevel) {
        if (name !== this.transformerName_) {
            this.namesToEllipsisLevels_[name] = ellipsisLevel;
        }
    }

    /**
     * @param {string} name
     * @param {number} ellipsisLevel
     */
    recordTemplateId(name, ellipsisLevel) {
        const maybeInPattern = name in this.namesToEllipsisLevels_
            ? this.namesToEllipsisLevels_[name]
            : -1;
        /* An identifier in a template is a candidate for being
         renamed during transcription if it doesn't occur in the pattern
         and is not the name of the macro. I've also thrown in a check
         that it's not a parser-sensititive identifier so we don't
         accidentally break the parser, but this may be buggy.
         The right thing to do is to remove the parser altogether.
         See comments at the top of Parser. */
        if (maybeInPattern === -1 && name !== this.transformerName_) {
            if (!isParserSensitiveId(name)) {
                this.renameCandidates_[name] = true;
            }
        } else if (maybeInPattern !== ellipsisLevel && name !== this.transformerName_) {
            throw Error.macro(
                this.transformerName_,
                name +
                ' is at ellipsis level ' +
                maybeInPattern +
                ' in pattern ' +
                ' but at ellipsis level ' +
                ellipsisLevel +
                ' in template ');
        }
    }

    /** @return {!Object<string, number>} */
    getPatternIds() {
        return this.namesToEllipsisLevels_;
    }

    /** @return {!Object<string, boolean>} */
    getRenameCandidates() {
        return this.renameCandidates_;
    }
}

exports = RenameHelper;