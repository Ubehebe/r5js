// There's probably something in jQuery to do this...oh well.
export class VisibilityManager {

    private elements: HTMLElement[] = [];
    private managedHashes: {[key:string]: boolean} = {};

    constructor() {
        const cb = () => {
            /* todo bl: location.hash is a classic XSS vector.
             I believe my use of it is safe, since the VisibilityManager ignores
             hashes it hasn't been previously informed of, but my experience
             with XSS is limited. */
            const rawHash = location.hash;
            if (this.managedHashes[rawHash]) {
                this.bringToFront(document.querySelector(rawHash) as HTMLElement);
            }
        };

        addEventListener('hashchange', cb, false);
    }

    registerAnchors(array: any[]): this {
        for (let i = 0; i < array.length; ++i) {
            this.pushAnchor(array[i]);
        }
        return this;
    }

    pushAnchor(a: HTMLAnchorElement) {
        const target = a.hash.length && document.querySelector(a.hash);
        if (target) {
            this.managedHashes[a.hash] = true;
            this.pushIfAbsent(target as HTMLElement);
        }
    }

    pushIfAbsent(element: HTMLElement): this {
        for (let i = 0; i < this.elements.length; ++i) {
            if (this.elements[i] === element) {
                return this;
            }
        }
        this.elements.push(element);
        return this;
    }

    bringToFront(element: HTMLElement) {
        for (let i = 0; i < this.elements.length; ++i) {
            this.elements[i].style.visibility = (this.elements[i] === element)
                ? 'visible'
                : 'hidden';
        }
    }
}
