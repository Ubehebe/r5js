import {Character} from "../ast/character";
import {Datum} from "../ast/datum";
import {newReader, Reader} from "../read/reader";
import {newTokenStream, TokenStream} from "../scan/token_stream";
import {Value} from "../value";
import {InMemoryPortBuffer} from "./in_memory_port_buffer";
import {InputPort} from "./input_port";

export class InMemoryInputPort extends InputPort {

  private leftoverDatum: Datum | null = null;

  constructor(private readonly buffer: InMemoryPortBuffer) {
    super();
    this.buffer = buffer;
  }

  /** @override */
  isCharReady(): boolean {
    return !this.buffer.isEmpty();
  }

  /** @override */
  close() {
    // TODO: implement?
  }

  /** @override */
  read(): Value | null {
    const maybeDatum = this.readLeftoverDatum();
    if (maybeDatum) {
      return maybeDatum;
    } else if (this.buffer.isEmpty()) {
      return null;
    } else {
      const text = this.buffer.getAndClear();
      this.leftoverDatum = newReader(newTokenStream(text)).read();
      return this.read();
    }
  }

  private readLeftoverDatum(): Datum | null {
    const retval = this.leftoverDatum;
    if (retval) {
      this.leftoverDatum = retval.getNextSibling();
    }
    return retval;
  }

  /** @override */
  peekChar() {
    const c = this.buffer.peekChar();
    return c ? new Character(c) : null;
  }

  /** @override */
  readChar() {
    const c = this.buffer.getChar();
    return c ? new Character(c) : null;
  }
}