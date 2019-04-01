export class RotaryNav {

  private transitionSpeed = 0;
  private centerX = 0;
  private centerY = 0;
  private readonly elements: TransformHelper[] = [];
  private readonly hashes: {[key:string]: number} = {};
  private selectedItemClass = "";

  constructor(
      private readonly centerElement: HTMLElement,
      private readonly radius: number,
      private readonly fromDegree: number,
      private readonly toDegree: number) {

    addEventListener('resize', () => {
      // Clients may want to manually dispatch a resize event to get the UI looking good. We don't
      // do it here because other parts of the boot process may want to listen in too.
      this.recenter();
      this.elements.forEach(e => e.setPosition(this.centerX, this.centerY));
    }, false);
    addEventListener('hashchange', () => {
      // TODO: location.hash is a vector for client-side XSS. I believe my usage is safe, because
      // rotateToFront will do nothing when given a hash that it hasn't previously been informed of
      // (and in any case, RotaryNav doesn't add to the DOM). But I should double-check, as I have
      // little experience with XSS.
      this.rotateToFront(location.hash);
    }, false);
  }

  private recenter() {
    const box = this.centerElement.getBoundingClientRect();
    this.centerX = box.left + box.width / 2;
    this.centerY = box.top + box.height / 2;
  }

  setSelectClass(cssClass: string): this {
    this.selectedItemClass = cssClass;
    return this;
  }

  setTransitionSpeed(seconds: number): this {
    this.transitionSpeed = seconds;
    return this;
  }

  /**
   * RotaryNav listens for the hashchange event, not click events, in order to integrate with the
   * browser history and Back button. For example, suppose the RotaryNav has a list item that
   * contains a link to an FAQ section, but other sections on the page also have links to the FAQ
   * section. When any of those links is clicked, the RotaryNav should rotate to the FAQ list item
   * around the same time the page is scrolling to the FAQ section.
   */
  push(element: HTMLElement, hashToListenFor: string) {
    element.style.position = 'fixed';
    element.style.visibility = 'visible';
    const index = this.elements.length;
    if (!(hashToListenFor in this.hashes)) {
      this.hashes[hashToListenFor] = index;
    }

    this.elements.push(new TransformHelper(element)
        .setPosition(this.centerX, this.centerY)
        .setTransitionSpeed(this.transitionSpeed || 0)
        .setPermanentTransformOrigin('center right')
        .setPermanentTransform('translate(-' + this.radius + 'px)'));
    return this.recalculateAngles();
  }

  rotateToFront(hash: string): this {
    const index = this.hashes[hash];
    if (index != null) {
      const which = this.elements[index];
      this.elements.forEach(e => e.incRot(-which.rot).removeClass(this.selectedItemClass));
      this.elements[index].addClass(this.selectedItemClass);
    }
    return this;
  }

  recalculateAngles(): this {
    const offset = (this.toDegree - this.fromDegree) / this.elements.length;
    this.elements.forEach((e, i) => e.setRot(this.fromDegree + i * offset));
    return this;
  }
}

// Save the current rotation to avoid parsing CSS to recover it
class TransformHelper {

  private readonly w: number;
  private readonly h: number;
  rot = 0;
  private permanentTransform = "";

  constructor(private readonly element: HTMLElement) {
    this.element = element;
    const bounding = element.getBoundingClientRect();
    this.w = bounding.width;
    this.h = bounding.height;
  }

  getElement(): HTMLElement {
    return this.element;
  }

  setPosition(x: number, y: number): this {
    // Since these elements have transformOrigin 'center right', we have to subtract the width and
    // half the height to get back to the top left of the element.
    // The width and height are cached at instantiation time because rotations seem to change them.
    this.element.style.left = `${x - this.w}px`;
    this.element.style.top = `${y - this.h / 2}px`;
    return this;
  }

  setPermanentTransform(str: string): this {
    this.permanentTransform = str;
    return this;
  }

  setPermanentTransformOrigin(str: string): this {
    this.element.style.transformOrigin
        = this.element.style.webkitTransformOrigin
        = str;
    return this;
  }

  setTransitionSpeed(seconds: number): this {
    this.element.style.transition
        = this.element.style.webkitTransition
        = 'all ' + seconds + 's ease-in-out'; // todo bl: 'all' may be too coarse
    return this;
  }

  reapply(): this {
    this.element.style.transform
        = this.element.style.webkitTransform
        = `rotate(${this.rot}deg${this.permanentTransform ? ' ' + this.permanentTransform : ''}`;
    return this;
  }

  setRot(rot: number) {
    this.rot = normalizeAngle(rot);
    return this.reapply();
  }

  incRot(delta: number) {
    this.rot += normalizeAngle(delta);
    return this.reapply();
  }

  addClass(cssClass: string): this {
    this.element.classList && this.element.classList.add(cssClass);
    return this;
  }

  removeClass(cssClass: string): this {
    this.element.classList && this.element.classList.remove(cssClass);
    return this;
  }
}

/** The UI would look silly if we took the "long way" around the dial. */
function normalizeAngle(angle: number): number {
  while (angle < -180) {
    angle += 360;
  }
  while (angle > 180) {
    angle -= 360;
  }
  return angle;
}
