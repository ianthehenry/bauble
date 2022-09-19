import type {Accessor, Setter, Signal} from "solid-js";
import {createSignal, createEffect, on} from "solid-js";

export type T<T> = Signal<T>;

const create = createSignal;
export {create};

export function getter<T>(signal: Signal<T>): Accessor<T> {
  return signal[0];
}

export function setter<T>(signal: Signal<T>): Setter<T> {
  return signal[1];
}

export function get<T>(signal: Signal<T>): T {
  return signal[0]();
}

export function set<T>(signal: Signal<T>, value: Exclude<T, Function>): T {
  return signal[1](value);
}

export function update<T>(signal: Signal<T>, update: (_: T) => T): T {
  return signal[1](update);
}

// should take an array of signals or accessors
export function onEffect(signals: any, f: (() => void)) {
  createEffect(on(() => { signals.forEach((x: any) => typeof x === 'function' ? x() : get(x)); }, f));
}
