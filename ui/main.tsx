import * as Storage from './storage';
import installCodeMirror from './editor';
import Renderer from './renderer';
// import {Timer, LoopMode, TimerState} from './timer'
import {mod, clamp, TAU} from './util'
import {Emscripten} from './wasm'

interface GestureEvent extends TouchEvent {
  scale: number
}

declare global {
  interface HTMLElementEventMap {
    'gesturestart': GestureEvent;
    'gesturechange': GestureEvent;
    'gestureend': GestureEvent;
  }
}

const cameraRotateSpeed = 1 / 512;
const cameraZoomSpeed = 0.01;

function fakeGetByID(oldID: string): HTMLElement {
  return document.querySelector("." + oldID)!;
}

function clearOutput() {
  const output = fakeGetByID('output-container');
  output.innerHTML = "";
}

function print(text: string, isErr=false) {
  if (isErr) {
    console.error(text);
  } else {
    console.log(text);
  }
  const output = fakeGetByID('output-container');
  const span = document.createElement('span');
  span.classList.toggle('err', isErr);
  span.appendChild(document.createTextNode(text));
  span.appendChild(document.createTextNode('\n'));
  output.appendChild(span);
}

let resolveReady: (_: undefined) => void;
const wasmReady = new Promise((x) => { resolveReady = x; });

const Module: Partial<Emscripten> = {
  preRun: [],
  print: function(x: string) {
    print(x, false);
  },
  printErr: function(x: string) {
    print(x, true);
  },
  postRun: [function() {
    resolveReady(void(0));
  }],
  locateFile: function(path, prefix) {
    if (prefix === '') {
      return '/js/' + path;
    } else {
      return prefix + path;
    }
  },
};

const defaultCamera = {
  x: -0.125,
  y: 0.125,
  zoom: 2.0,
};

enum CompilationState {
  Unknown,
  Success,
  Error,
}

function initialize(script: string) {
  const camera = {
    x: defaultCamera.x,
    y: defaultCamera.y,
    zoom: defaultCamera.zoom,
  };

  const playButton: HTMLButtonElement = document.querySelector('.canvas-container button[data-action=play]')!;
  const pauseButton: HTMLButtonElement = document.querySelector('.canvas-container button[data-action=pause]')!;
  const stopButton: HTMLButtonElement = document.querySelector('.canvas-container button[data-action=stop]')!;
  const resetButton: HTMLButtonElement = document.querySelector('.canvas-container button[data-action=reset-camera]')!;
  const compilationErrorIndicator: HTMLElement = document.querySelector('.code-container .indicator.compilation-error')!;
  const compilationSuccessIndicator: HTMLElement = document.querySelector('.code-container .indicator.compilation-success')!;
  const compilationUnknownIndicator: HTMLElement = document.querySelector('.code-container .indicator.compilation-unknown')!;

  const timestampSpan: HTMLInputElement = document.querySelector('.toolbar span.timestamp')!;

  // const timer = new Timer();
  let drawScheduled = false;
  let recompileScheduled = false;
  let isAnimation = false;
  function draw(recompile: boolean) {
    recompileScheduled ||= recompile;
    drawScheduled = true;
  }

  let compilationState = CompilationState.Unknown;

  const canvas = fakeGetByID('render-target') as HTMLCanvasElement;
  // const renderer = new Renderer(canvas);

  function tick(now: number) {
    requestAnimationFrame(tick);
    // timer.tick(now, isAnimation);

    // drawScheduled = timer.state === TimerState.Playing;
    recompileScheduled = false;

    // if (timer.state === TimerState.Playing) {
    //   playButton.classList.add('hidden');
    //   pauseButton.classList.remove('hidden');
    // } else {
    //   playButton.classList.remove('hidden');
    //   pauseButton.classList.add('hidden');
    // }
    // timestampSpan.innerText = timer.t[0]().toFixed(2);
    switch (compilationState) {
      case CompilationState.Success:
        compilationUnknownIndicator.classList.add('hidden');
        compilationErrorIndicator.classList.add('hidden');
        compilationSuccessIndicator.classList.remove('hidden');
        break;
      case CompilationState.Error:
        compilationUnknownIndicator.classList.add('hidden');
        compilationSuccessIndicator.classList.add('hidden');
        compilationErrorIndicator.classList.remove('hidden');
        break;
    }
  }
  requestAnimationFrame(tick);

  const editorContainer = fakeGetByID('editor-container');
  const editor = installCodeMirror(script, editorContainer, () => draw(true));

  resetButton.addEventListener('click', (_e) => {
    camera.x = defaultCamera.x;
    camera.y = defaultCamera.y;
    camera.zoom = defaultCamera.zoom;
    draw(false);
  });
  playButton.addEventListener('click', (_e) => {
    // timer.state = TimerState.Playing;
  });
  pauseButton.addEventListener('click', (_e) => {
    // timer.state = TimerState.Paused;
  });
  stopButton.addEventListener('click', (_e) => {
    // timer.stop();
    draw(false);
  });

  fakeGetByID('canvas-container').addEventListener('input', (e) => {
    const target = e.target as HTMLInputElement;
    switch (target.name) {
      case 'view-type': {
        if (target.checked) {
          // renderer.viewType = parseInt(target.value, 10);
          draw(false);
        }
        break;
      }
      case 'loop-mode': {
        if (target.checked) {
          switch (target.value) {
            // case 'no-loop': timer.setLoopMode(LoopMode.NoLoop); break;
            // case 'wrap': timer.setLoopMode(LoopMode.Wrap); break;
            // case 'reverse': timer.setLoopMode(LoopMode.Reverse); break;
            default: throw new Error("invalid loop value " + target.value);
          }
        }
        break;
      }
      case 'loop-start': {
        // timer.loopStart = parseFloat(target.value);
        break;
      }
      case 'loop-end': {
        // timer.loopEnd = parseFloat(target.value);
        break;
      }
      default:
        throw new Error("unknown field " + target.name);
    }
  });

  let canvasPointerAt = [0, 0];
  let rotatePointerId: number | null = null;
  canvas.addEventListener('pointerdown', (e) => {
    if (rotatePointerId === null) {
      e.preventDefault();
      canvasPointerAt = [e.offsetX, e.offsetY];
      canvas.setPointerCapture(e.pointerId);
      rotatePointerId = e.pointerId;
    }
  });

  canvas.addEventListener('pointerup', (e) => {
    e.preventDefault();
    if (e.pointerId === rotatePointerId) {
      rotatePointerId = null;
    }
  });

  let isGesturing = false;
  let gestureEndedAt = 0;
  canvas.addEventListener('pointermove', (e) => {
    if (e.pointerId === rotatePointerId) {
      e.preventDefault();
      const pointerWasAt = canvasPointerAt;
      canvasPointerAt = [e.offsetX, e.offsetY];

      if (isGesturing) {
        return;
      }
      // if you were just trying to zoom,
      // we don't want to do a little tiny
      // pan as you lift your second finger.
      // so we wait 100ms before we allow
      // panning to continue
      if (performance.now() - gestureEndedAt < 100) {
        return;
      }

      const movementX = canvasPointerAt[0] - pointerWasAt[0];
      const movementY = canvasPointerAt[1] - pointerWasAt[1];
      // TODO: pixelScale shouldn't be hardcoded
      const pixelScale = 0.5;
      const scaleAdjustmentX = pixelScale * canvas.width / canvas.clientWidth;
      const scaleAdjustmentY = pixelScale * canvas.height / canvas.clientHeight;
      // TODO: invert the meaning of camera.x/y so that this actually makes sense
      camera.x = mod(camera.x - scaleAdjustmentY * cameraRotateSpeed * movementY, 1.0);
      camera.y = mod(camera.y - scaleAdjustmentX * cameraRotateSpeed * movementX, 1.0);
      draw(false);
    }
  });

  // TODO: I haven't actually tested if this is anything
  let initialZoom = 1;
  canvas.addEventListener('gesturestart', (_: GestureEvent) => {
    initialZoom = camera.zoom;
    isGesturing = true;
  });
  canvas.addEventListener('gestureend', (_: GestureEvent) => {
    initialZoom = camera.zoom;
    isGesturing = false;
    gestureEndedAt = performance.now();
  });
  canvas.addEventListener('gesturechange', (e: GestureEvent) => {
    camera.zoom = initialZoom / e.scale;
    draw(false);
  });

  canvas.addEventListener('wheel', (e) => {
    e.preventDefault();
    // Linux Firefox users who do not set MOZ_USE_XINPUT2
    // will report very large values of deltaY, resulting
    // in very choppy scrolling. I don't really know a good
    // way to fix this without explicit platform detection.
    camera.zoom += cameraZoomSpeed * e.deltaY;
    draw(false);
  });

  const outputContainer = fakeGetByID('output-container');
  const outputResizeHandle = fakeGetByID('output-resize-handle');
  let handlePointerAt = [0, 0];
  outputResizeHandle.addEventListener('pointerdown', (e) => {
    outputResizeHandle.setPointerCapture(e.pointerId);
    handlePointerAt = [e.screenX, e.screenY];
  });
  outputResizeHandle.addEventListener('pointermove', (e) => {
    if (outputResizeHandle.hasPointerCapture(e.pointerId)) {
      const outputStyle = getComputedStyle(outputContainer);
      const verticalPadding = parseFloat(outputStyle.paddingTop) + parseFloat(outputStyle.paddingBottom);
      const oldHeight = outputContainer.offsetHeight - verticalPadding;
      const oldScrollTop = outputContainer.scrollTop;
      const handlePointerWasAt = handlePointerAt;
      handlePointerAt = [e.screenX, e.screenY];
      const delta = handlePointerAt[1] - handlePointerWasAt[1];
      outputContainer.style.height = `${oldHeight - delta}px`;
      outputContainer.scrollTop = clamp(oldScrollTop + delta, 0, outputContainer.scrollHeight - outputContainer.offsetHeight);
    }
  });

  draw(true);
  editor.focus();
}

if (false)
document.addEventListener("DOMContentLoaded", (_) => {
  wasmReady.then(() => {
    const saved = Storage.getScript();
    if (saved) {
      initialize(saved);
    } else {
      initialize(FS.readFile('intro.janet', {encoding: 'utf8'}));
    }
  }).catch(console.error);
});

window.Module = Module;

import Bauble from './bauble';
import { render as renderSolid } from "solid-js/web";

function makeBauble(script: string, placeholder: HTMLElement) {
  renderSolid(() => <Bauble initialScript={script} hijackScroll={false} />, placeholder);
}

if (true)
document.addEventListener("DOMContentLoaded", (_) => {
  wasmReady.then(() => {
    for (const el of document.querySelectorAll('.code-example')) {
      const placeholder = el.nextElementSibling! as HTMLElement;
      makeBauble(el.textContent!, placeholder);
    }
  }).catch(console.error);
});
