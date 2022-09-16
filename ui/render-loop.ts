import type {Seconds} from './types';

export default class RenderLoop {
  private scheduled = false;
  private then: Seconds | null = null;
  private f: (_: Seconds) => boolean;
  constructor(f: (_: Seconds) => boolean) {
    this.f = f;
  }
  schedule() {
    if (this.scheduled) {
      return;
    }
    this.scheduled = true;
    requestAnimationFrame((nowMS) => {
      const nowSeconds = nowMS / 1000 as Seconds;
      this.scheduled = false;
      const elapsed = this.then == null ? 0 : nowSeconds - this.then;
      if (this.f(elapsed as Seconds)) {
        this.schedule();
        this.then = nowSeconds;
      } else {
        this.then = null;
      }
    });
  }
}
