import {Error} from "../error";
import {isParserSensitiveId} from "../parse/rename_util";

export class RenameHelper {

  private readonly namesToEllipsisLevels: { [key: string]: number } = {};
  private readonly renameCandidates: Set<string> = new Set();

  constructor(private readonly transformerName: string) {}

  recordPatternId(name: string, ellipsisLevel: number) {
    if (name !== this.transformerName) {
      this.namesToEllipsisLevels[name] = ellipsisLevel;
    }
  }

  recordTemplateId(name: string, ellipsisLevel: number) {
    const maybeInPattern = name in this.namesToEllipsisLevels
        ? this.namesToEllipsisLevels[name]
        : -1;
    // An identifier in a template is a candidate for being renamed during transcription if it
    // doesn't occur in the pattern and is not the name of the macro. I've also thrown in a check
    // that it's not a parser-sensititive identifier so we don't accidentally break the parser,
    // but this may be buggy. The right thing to do is to remove the parser altogether. See comments
    // at the top of Parser.
    if (maybeInPattern === -1 && name !== this.transformerName) {
      if (!isParserSensitiveId(name)) {
        this.renameCandidates.add(name);
      }
    } else if (maybeInPattern !== ellipsisLevel && name !== this.transformerName) {
      throw Error.macro(
          this.transformerName,
          `${name} is at ellipsis level ${maybeInPattern} in pattern `
          + `but at ellipsis level ${ellipsisLevel} in template`);
    }
  }

  getPatternIds(): { [key: string]: number } {
    return this.namesToEllipsisLevels;
  }

  getRenameCandidates(): Set<string> {
    return this.renameCandidates;
  }
}