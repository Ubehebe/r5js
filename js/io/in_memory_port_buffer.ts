export class InMemoryPortBuffer {

    private buffer = '';

    isEmpty(): boolean {
        return !this.buffer.length;
    }

    getChar(): string|null {
        const c = this.peekChar();
        if (c) {
            this.buffer = this.buffer.substr(1);
        }
        return c;
    }

    peekChar(): string|null {
        return this.buffer.length ? this.buffer.charAt(0) : null;
    }

    getAndClear(): string {
        const retval = this.buffer;
        this.buffer = '';
        return retval;
    }

    append(str: string) {
        this.buffer += str;
    }
}