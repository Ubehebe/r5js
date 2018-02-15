export class InMemoryPortBuffer {

    private buffer_: string = '';

    isEmpty(): boolean {
        return !this.buffer_.length;
    }

    getChar(): string|null {
        const c = this.peekChar();
        if (c) {
            this.buffer_ = this.buffer_.substr(1);
        }
        return c;
    }

    peekChar(): string|null {
        return this.buffer_.length ? this.buffer_.charAt(0) : null;
    }

    getAndClear(): string {
        const retval = this.buffer_;
        this.buffer_ = '';
        return retval;
    }

    append(str: string) {
        this.buffer_ += str;
    }
}