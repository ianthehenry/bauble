import * as Signal from './signals';
import {Seconds} from './types';
import {batch, createMemo} from 'solid-js';
import {vec2, vec3} from 'gl-matrix';

export type T = {
  dimensions: number,
  time: Seconds,
  isVisible: boolean,
  rotation: vec2,
  origin: vec3,
  origin2D: vec2,
  zoom: number,
  prefersFreeCamera: boolean,
  quadView: boolean,
  quadSplitPoint: vec2,
  resolution: {width: number, height: number},
  crosshairs: vec3 | null,
  customUniforms: Array<{name: string, type: string, value: any}>,
};

export type Accessors = {
  dimensions: Signal.Accessor<number>,
  time: Signal.Accessor<Seconds>,
  isVisible: Signal.Accessor<boolean>,
  rotation: Signal.Accessor<vec2>,
  origin: Signal.Accessor<vec3>,
  origin2D: Signal.Accessor<vec2>,
  zoom: Signal.Accessor<number>,
  prefersFreeCamera: Signal.Accessor<boolean>,
  quadView: Signal.Accessor<boolean>,
  quadSplitPoint: Signal.Accessor<vec2>,
  resolution: Signal.Accessor<{width: number, height: number}>,
  crosshairs: Signal.Accessor<vec3 | null>,
  customUniforms: Signal.Accessor<Array<{name: string, type: string, value: any}>>,
};

export type Signals = {
  dimensions: Signal.T<number>,
  time: Signal.T<Seconds>,
  isVisible: Signal.T<boolean>,
  rotation: Signal.T<vec2>,
  origin: Signal.T<vec3>,
  origin2D: Signal.T<vec2>,
  zoom: Signal.T<number>,
  prefersFreeCamera: Signal.T<boolean>,
  quadView: Signal.T<boolean>,
  quadSplitPoint: Signal.T<vec2>,
  resolution: Signal.T<{width: number, height: number}>,
  crosshairs: Signal.T<vec3 | null>,
  customUniforms: Signal.T<Array<{name: string, type: string, value: any}>>,
};

export function defaultSignals() {
  return {
    dimensions: Signal.create(3),
    time: Signal.create(0 as Seconds),
    isVisible: Signal.create(false),
    rotation: Signal.create(vec2.fromValues(0, 0)),
    origin: Signal.create(vec3.fromValues(0, 0, 0)),
    origin2D: Signal.create(vec2.fromValues(0, 0)),
    zoom: Signal.create(0),
    prefersFreeCamera: Signal.create(true),
    quadView: Signal.create(false),
    quadSplitPoint: Signal.create(vec2.fromValues(0.5, 0.5)),
    resolution: Signal.create({width: 0, height: 0}),
    crosshairs: Signal.create<vec3 | null>(null),
    customUniforms: Signal.create<Array<{name: string, type: string, value: any}>>([]),
  };
}

export function accessAll(accessors: Accessors): Signal.Accessor<T> {
  return createMemo(() => {
    return {
      dimensions: accessors.dimensions(),
      time: accessors.time(),
      isVisible: accessors.isVisible(),
      rotation: accessors.rotation(),
      origin: accessors.origin(),
      origin2D: accessors.origin2D(),
      zoom: accessors.zoom(),
      prefersFreeCamera: accessors.prefersFreeCamera(),
      quadView: accessors.quadView(),
      quadSplitPoint: accessors.quadSplitPoint(),
      resolution: accessors.resolution(),
      crosshairs: accessors.crosshairs(),
      customUniforms: accessors.customUniforms(),
    };
  });
};

export function getAll(signals: Signals): Accessors {
  return {
    dimensions: Signal.getter(signals.dimensions),
    time: Signal.getter(signals.time),
    isVisible: Signal.getter(signals.isVisible),
    rotation: Signal.getter(signals.rotation),
    origin: Signal.getter(signals.origin),
    origin2D: Signal.getter(signals.origin2D),
    zoom: Signal.getter(signals.zoom),
    prefersFreeCamera: Signal.getter(signals.prefersFreeCamera),
    quadView: Signal.getter(signals.quadView),
    quadSplitPoint: Signal.getter(signals.quadSplitPoint),
    resolution: Signal.getter(signals.resolution),
    crosshairs: Signal.getter(signals.crosshairs),
    customUniforms: Signal.getter(signals.customUniforms),
  };
};

export function setAll(signals: Signals, value: T) {
  batch(() => {
    Signal.set(signals.dimensions, value.dimensions);
    Signal.set(signals.time, value.time);
    Signal.set(signals.isVisible, value.isVisible);
    Signal.set(signals.rotation, value.rotation);
    Signal.set(signals.origin, value.origin);
    Signal.set(signals.origin2D, value.origin2D);
    Signal.set(signals.zoom, value.zoom);
    Signal.set(signals.prefersFreeCamera, value.prefersFreeCamera);
    Signal.set(signals.quadView, value.quadView);
    Signal.set(signals.quadSplitPoint, value.quadSplitPoint);
    Signal.set(signals.resolution, value.resolution);
    Signal.set(signals.crosshairs, value.crosshairs);
    Signal.set(signals.customUniforms, value.customUniforms);
  });
}
