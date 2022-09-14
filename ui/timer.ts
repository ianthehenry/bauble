import {mod, clamp} from './util'
import * as Signal from './signals'
import type {Seconds} from './types';

export enum TimerState {
  Ambivalent,
  Playing,
  Paused,
}

export enum LoopMode {
  NoLoop = "no-loop",
  Wrap = "wrap",
  Reverse = "reverse",
}

export class Timer {
  t = Signal.create(0 as Seconds);
  state = TimerState.Ambivalent;
  private loopMode = LoopMode.NoLoop;
  loopStart = 0 as Seconds;
  loopEnd = Math.PI * 2 as Seconds;
  private rate = 1;

  playPause() {
    this.state = this.state === TimerState.Playing ? TimerState.Paused : TimerState.Playing;
  }

  stop() {
    Signal.set(this.t, this.loopStart);
    this.state = TimerState.Paused;
    this.rate = 1;
  }

  tick(delta: Seconds, isAnimation: boolean) {
    if (isAnimation && this.state === TimerState.Ambivalent) {
      this.state = TimerState.Playing;
    }
    let next = Signal.get(this.t) + this.rate * delta;

    if (next > this.loopEnd) {
      switch (this.loopMode) {
        case LoopMode.NoLoop: break;
        case LoopMode.Wrap:
          next = this.loopStart + (next - this.loopEnd);
          break;
        case LoopMode.Reverse:
          next = this.loopEnd - (next - this.loopEnd);
          this.rate = -1;
          break;
      }
    }
    if (next < this.loopStart) {
      switch (this.loopMode) {
        case LoopMode.NoLoop: break;
        case LoopMode.Wrap:
          next = this.loopStart;
          break;
        case LoopMode.Reverse:
          next = this.loopStart + (this.loopStart - next);
          this.rate = 1;
          break;
      }
    }

    Signal.set(this.t, next as Seconds);
  }

  setLoopMode(loopMode: LoopMode) {
    if (loopMode != LoopMode.Reverse) {
      this.rate = 1;
    }
    this.loopMode = loopMode;
    if (loopMode != LoopMode.NoLoop) {
      Signal.update(this.t, (t) => clamp(t as number, this.loopStart, this.loopEnd) as Seconds);
    }
  }
}
