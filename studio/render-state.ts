import * as Signal from './signals';
import {Seconds} from './types';
import {batch, createMemo} from 'solid-js';

export type T = {
  time: Seconds,
  renderType: number, // TODO: give this a type
  rotation: {x: number, y: number},
  origin: {x: number, y: number, z: number},
  zoom: number, // TODO: give this a unique type
  quadView: boolean,
  quadSplitPoint: {x: number, y: number},
  resolution: {width: number, height: number},
};

export type Accessors = {
  time: Signal.Accessor<Seconds>,
  renderType: Signal.Accessor<number>,
  rotation: Signal.Accessor<{x: number, y: number}>,
  origin: Signal.Accessor<{x: number, y: number, z: number}>,
  zoom: Signal.Accessor<number>,
  quadView: Signal.Accessor<boolean>,
  quadSplitPoint: Signal.Accessor<{x: number, y: number}>,
  resolution: Signal.Accessor<{width: number, height: number}>,
};

export type Signals = {
  time: Signal.T<Seconds>,
  renderType: Signal.T<number>,
  rotation: Signal.T<{x: number, y: number}>,
  origin: Signal.T<{x: number, y: number, z: number}>,
  zoom: Signal.T<number>,
  quadView: Signal.T<boolean>,
  quadSplitPoint: Signal.T<{x: number, y: number}>,
  resolution: Signal.T<{width: number, height: number}>,
};

export function accessAll(accessors: Accessors): Signal.Accessor<T> {
  return createMemo(() => {
    return {
      time: accessors.time(),
      renderType: accessors.renderType(),
      rotation: accessors.rotation(),
      origin: accessors.origin(),
      zoom: accessors.zoom(),
      quadView: accessors.quadView(),
      quadSplitPoint: accessors.quadSplitPoint(),
      resolution: accessors.resolution(),
    };
  });
};

export function setAll(signals: Signals, value: T) {
  batch(() => {
    Signal.set(signals.time, value.time);
    Signal.set(signals.renderType, value.renderType);
    Signal.set(signals.rotation, value.rotation);
    Signal.set(signals.origin, value.origin);
    Signal.set(signals.zoom, value.zoom);
    Signal.set(signals.quadView, value.quadView);
    Signal.set(signals.quadSplitPoint, value.quadSplitPoint);
    Signal.set(signals.resolution, value.resolution);
  });
}
