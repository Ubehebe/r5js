import {InMemoryPortBuffer} from "./in_memory_port_buffer";
import {addInputPortImpl, InputPort} from "./input_port";
import {Datum} from "../ast/datum";
import {Reader} from "../read/reader";
import {TokenStream} from "../scan/token_stream";
import {Character} from "../ast/character";

export class InMemoryInputPort extends InputPort {

  private readonly buffer_: InMemoryPortBuffer;
  private leftoverDatum_: Datum | null = null;

  constructor(buffer: InMemoryPortBuffer) {
    super();
    this.buffer_ = buffer;
  }

  /** @override */
  isCharReady(): boolean {
    return !this.buffer_.isEmpty();
  }

  /** @override */
  close() {
    // TODO: implement?
  }

  /** @override */
  read(): Value | null {
    const maybeDatum = this.readLeftoverDatum_();
    if (maybeDatum) {
      return maybeDatum;
    } else if (this.buffer_.isEmpty()) {
      return null;
    } else {
      const text = this.buffer_.getAndClear();
      this.leftoverDatum_ = Reader.forTokenStream(TokenStream.forText(text)).read();
      return this.read();
    }
  }

  private readLeftoverDatum_(): Datum | null {
    const retval = this.leftoverDatum_;
    if (retval) {
      this.leftoverDatum_ = retval.getNextSibling();
    }
    return retval;
  }

  /** @override */
  peekChar() {
    const c = this.buffer_.peekChar();
    return c ? new Character(c) : null;
  }

  /** @override */
  readChar() {
    const c = this.buffer_.getChar();
    return c ? new Character(c) : null;
  }
}

addInputPortImpl(InMemoryInputPort);