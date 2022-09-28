import type {Seconds} from './types';

export default class RenderLoop {
  private scheduled = false;
  private then: Seconds | null = null;
  private f: (_: Seconds) => void;
  constructor(f: (_: Seconds) => void) {
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

      this.f(elapsed as Seconds);
      if (this.scheduled) {
        this.then = nowSeconds;
      } else {
        this.then = null;
      }
    });
  }
}
