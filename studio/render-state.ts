import * as Signal from './signals';
import {Seconds} from './types';
import {batch, createMemo} from 'solid-js';
import {vec2, vec3} from 'gl-matrix';

export enum RenderType {
  Normal = 0,
  Surfaceless = 1,
  Convergence = 2,
  Distance = 3,
};

export type T = {
  time: Seconds,
  isVisible: boolean,
  renderType: RenderType,
  rotation: vec2,
  origin: vec3,
  zoom: number, // TODO: give this a unique type
  quadView: boolean,
  quadSplitPoint: vec2,
  resolution: {width: number, height: number},
};

export type Accessors = {
  time: Signal.Accessor<Seconds>,
  isVisible: Signal.Accessor<boolean>,
  renderType: Signal.Accessor<RenderType>,
  rotation: Signal.Accessor<vec2>,
  origin: Signal.Accessor<vec3>,
  zoom: Signal.Accessor<number>,
  quadView: Signal.Accessor<boolean>,
  quadSplitPoint: Signal.Accessor<vec2>,
  resolution: Signal.Accessor<{width: number, height: number}>,
};

export type Signals = {
  time: Signal.T<Seconds>,
  isVisible: Signal.T<boolean>,
  renderType: Signal.T<RenderType>,
  rotation: Signal.T<vec2>,
  origin: Signal.T<vec3>,
  zoom: Signal.T<number>,
  quadView: Signal.T<boolean>,
  quadSplitPoint: Signal.T<vec2>,
  resolution: Signal.T<{width: number, height: number}>,
};

export function defaultSignals() {
  return {
    time: Signal.create(0 as Seconds),
    isVisible: Signal.create(false),
    renderType: Signal.create(RenderType.Normal),
    rotation: Signal.create(vec2.fromValues(0, 0)),
    origin: Signal.create(vec3.fromValues(0, 0, 0)),
    zoom: Signal.create(0),
    quadView: Signal.create(false),
    quadSplitPoint: Signal.create(vec2.fromValues(0.5, 0.5)),
    resolution: Signal.create({width: 0, height: 0}),
  };
}

export function accessAll(accessors: Accessors): Signal.Accessor<T> {
  return createMemo(() => {
    return {
      time: accessors.time(),
      isVisible: accessors.isVisible(),
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

export function getAll(signals: Signals): Accessors {
  return {
    time: Signal.getter(signals.time),
    isVisible: Signal.getter(signals.isVisible),
    renderType: Signal.getter(signals.renderType),
    rotation: Signal.getter(signals.rotation),
    origin: Signal.getter(signals.origin),
    zoom: Signal.getter(signals.zoom),
    quadView: Signal.getter(signals.quadView),
    quadSplitPoint: Signal.getter(signals.quadSplitPoint),
    resolution: Signal.getter(signals.resolution),
  };
};

export function setAll(signals: Signals, value: T) {
  batch(() => {
    Signal.set(signals.time, value.time);
    Signal.set(signals.isVisible, value.isVisible);
    Signal.set(signals.renderType, value.renderType);
    Signal.set(signals.rotation, value.rotation);
    Signal.set(signals.origin, value.origin);
    Signal.set(signals.zoom, value.zoom);
    Signal.set(signals.quadView, value.quadView);
    Signal.set(signals.quadSplitPoint, value.quadSplitPoint);
    Signal.set(signals.resolution, value.resolution);
  });
}
