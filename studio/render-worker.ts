import * as RenderState from './render-state';
import Renderer from './renderer';
import RenderLoop from './render-loop';
import {Timer} from './timer';
import * as Signal from './signals';
import {Seconds} from './types';

export function init() {
  const state: RenderState.Signals = RenderState.defaultSignals();
  let renderer: Renderer | undefined;
  const renderLoop = new RenderLoop((_: Seconds) => {
    if (!Signal.get(state.isVisible)) {
      return;
    }
    renderer!.draw();
  });

  Signal.onEffect([
    state.time,
    state.isVisible,
    state.renderType,
    state.rotation,
    state.origin,
    state.zoom,
    state.quadView,
    state.quadSplitPoint,
    state.resolution,
  ], () => {
    renderLoop.schedule();
  });

  const getResponse = (request : any) => {
    // TODO: enum
    switch (request.tag) {
    case 'init': {
      renderer = new Renderer(request.canvas as OffscreenCanvas, RenderState.getAll(state));
      break;
    }
    case 'set': {
      RenderState.setAll(state, request.state as RenderState.T);
      break;
    }
    case 'shader': {
      renderer!.recompileShader(request.source);
      renderLoop.schedule();
      break;
    }
    default: throw new Error("unknown request tag " + request.tag);
    }
  };

  self.addEventListener('message', (event) => {
    try {
      self.postMessage({
        id: event.data.id,
        response: getResponse(event.data.request),
      });
    } catch (e) {
      self.postMessage({
        id: event.data.id,
        error: e,
      });
    }
  });
}
