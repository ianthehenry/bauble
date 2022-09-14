import {mod, clamp, TAU} from './util'
import * as Signal from './signals'
import {createEffect, untrack} from 'solid-js'
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

function clampTime(t_: Seconds, loopStart_: Seconds, loopEnd_: Seconds, loopMode: LoopMode): [Seconds, number] {
  let t = t_ as number;
  const loopStart = loopStart_ as number;
  const loopEnd = loopEnd_ as number;
  let rate = 0;
  if (t > loopEnd) {
    switch (loopMode) {
      case LoopMode.NoLoop: break;
      case LoopMode.Wrap:
        t = loopStart + (t - loopEnd);
        break;
      case LoopMode.Reverse:
        t = loopEnd - (t - loopEnd);
        rate = -1;
        break;
    }
  }
  if (t < loopStart) {
    switch (loopMode) {
      case LoopMode.NoLoop: break;
      case LoopMode.Wrap:
        t = loopStart;
        break;
      case LoopMode.Reverse:
        t = loopStart + (loopStart - t);
        rate = 1;
        break;
    }
  }
  return [t as Seconds, rate];
}

export class Timer {
  t = Signal.create(0 as Seconds);
  loopStart = Signal.create(0 as Seconds);
  loopEnd = Signal.create(TAU as Seconds);
  loopMode = Signal.create(LoopMode.NoLoop);

  private state = TimerState.Ambivalent;
  private rate = 1;

  playPause() {
    this.state = this.state === TimerState.Playing ? TimerState.Paused : TimerState.Playing;
  }

  stop() {
    Signal.set(this.t, Signal.get(this.loopStart));
    this.state = TimerState.Paused;
    this.rate = 1;
  }

  constructor() {
    createEffect(() => {
      const loopStart = Signal.get(this.loopStart);
      const loopEnd = Signal.get(this.loopEnd);
      const loopMode = Signal.get(this.loopMode);
      if (loopMode != LoopMode.Reverse) {
        this.rate = 1;
      }
      const [t, rate] = clampTime(untrack(Signal.getter(this.t)), loopStart, loopEnd, loopMode);
      if (rate !== 0) {
        this.rate = rate;
      }
      Signal.set(this.t, t);
    });
  }

  tick(delta: Seconds, isAnimation: boolean) {
    if (isAnimation && this.state === TimerState.Ambivalent) {
      this.state = TimerState.Playing;
    }

    const [t, rate] = clampTime(
      ((Signal.get(this.t) as number) + this.rate * (delta as number)) as Seconds,
      Signal.get(this.loopStart),
      Signal.get(this.loopEnd),
      Signal.get(this.loopMode),
    );

    Signal.set(this.t, t);
    if (rate !== 0) {
      this.rate = rate;
    }
  }
}
