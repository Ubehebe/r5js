import {DatumStream} from "./datum_stream";
import {Datum} from "../ast/datum";

export class /* TODO should be interface */ Rule {
  /** @return True iff the parse succeeded. */
  match(datumStream: DatumStream): boolean | Datum /* TODO wtf? */ {
    return false;
  }
}