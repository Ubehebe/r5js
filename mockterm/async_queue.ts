export class AsyncQueue {

  private readonly queue: any[] = [];
  private running = false;

  // TODO: a more flexible design would be to specify a latency for every object enqueued.
  constructor(readonly latency: number) {}

  isRunning(): boolean {
    return this.running;
  }

  enqueue(cb: any): this {
    this.queue.push(cb);
    if (!this.running) {
      setTimeout(() => this.run(), this.latency);
      this.running = true;
    }
    return this;
  }

  dequeue(): any {
    return this.queue.shift();
  }

  run() {
    const todo = this.dequeue();
    if (todo) {
      todo();
      setTimeout(() => this.run(), this.latency);
    } else {
      this.running = false;
    }
  }
}
