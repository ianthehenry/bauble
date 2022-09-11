import * as Storage from './storage';
import installCodeMirror from './editor';

interface CompilationResult {
  isError: boolean,
  shaderSource: string,
  isAnimated: boolean,
  error: string,
}

interface MyEmscripten extends EmscriptenModule {
  cwrap: typeof cwrap;
  ccall: typeof ccall;
  evaluate_script: ((_: string) => CompilationResult);
}

interface GestureEvent extends TouchEvent {
  scale: number
}

declare global {
  interface Window { Module: Partial<MyEmscripten>; }
  interface HTMLElementEventMap {
    'gesturestart': GestureEvent;
    'gesturechange': GestureEvent;
    'gestureend': GestureEvent;
  }
}

const TAU = 2 * Math.PI;
const cameraRotateSpeed = 1 / 512;
const cameraZoomSpeed = 0.01;

function fakeGetByID(oldID: string): HTMLElement {
  return document.querySelector("." + oldID)!;
}

function clearOutput() {
  const output = fakeGetByID('output');
  output.innerHTML = "";
}

function print(text: string, isErr=false) {
  if (isErr) {
    console.error(text);
  } else {
    console.log(text);
  }
  const output = fakeGetByID('output');
  const span = document.createElement('span');
  span.classList.toggle('err', isErr);
  span.appendChild(document.createTextNode(text));
  span.appendChild(document.createTextNode('\n'));
  output.appendChild(span);
}

let updateTime: (_t: number) => void;
let updateViewType: (_t: number) => void;
let rerender: () => void;
let recompileFragmentShader: (_: string) => void;

let updateCamera: (_cameraX: number, _cameraY: number, _cameraZoom: number) => void

let resolveReady: (_: undefined) => void;
const wasmReady = new Promise((x) => { resolveReady = x; });

const Module: Partial<MyEmscripten> = {
  preRun: [],
  print: function(x: string) {
    print(x, false);
  },
  printErr: function(x: string) {
    print(x, true);
  },
  postRun: [function() {
    updateCamera = Module.cwrap!("update_camera", null, ['number', 'number', 'number']);
    updateTime = Module.cwrap!("update_time", null, ['number']);
    updateViewType = Module.cwrap!("update_view_type", null, ['number']);
    rerender = Module.cwrap!("rerender", null, []);
    recompileFragmentShader = Module.cwrap!("recompile_fragment_shader", null, ['string']);
    Module.ccall!("initialize_janet", null, [], []);
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

function clamp(value: number, min: number, max: number) {
  return Math.max(Math.min(value, max), min);
}

function mod(a: number, b: number) {
  return ((a % b) + b) % b;
}

enum TimerState {
  Ambivalent,
  Playing,
  Paused,
}

enum LoopMode {
  NoLoop = "no-loop",
  Wrap = "wrap",
  Reverse = "reverse",
}

class Timer {
  t = 0;
  state = TimerState.Ambivalent;
  private then: number | null = null;
  private loopMode = LoopMode.NoLoop;
  loopStart = 0;
  loopEnd = Math.PI * 2;
  private rate = 1;

  playPause() {
    this.state = this.state === TimerState.Playing ? TimerState.Paused : TimerState.Playing;
  }

  stop() {
    this.t = this.loopStart;
    this.state = TimerState.Paused;
    this.rate = 1;
  }

  tick(now: number, isAnimation: boolean) {
    if (isAnimation && this.state === TimerState.Ambivalent) {
      this.state = TimerState.Playing;
    }
    now /= 1000;
    if (this.state === TimerState.Playing && isAnimation) {
      const then = this.then ?? now;
      let next = this.t + this.rate * (now - then);

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

      this.t = next;
    }
    this.then = now;
  }

  setLoopMode(loopMode: LoopMode) {
    if (loopMode != LoopMode.Reverse) {
      this.rate = 1;
    }
    this.loopMode = loopMode;
    if (loopMode != LoopMode.NoLoop) {
      this.t = clamp(this.t, this.loopStart, this.loopEnd);
    }
  }
}

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

  const timer = new Timer();
  let drawScheduled = false;
  let recompileScheduled = false;
  let isAnimation = false;
  function draw(recompile: boolean) {
    recompileScheduled ||= recompile;
    drawScheduled = true;
  }

  let compilationState = CompilationState.Unknown;

  function tick(now: number) {
    requestAnimationFrame(tick);
    timer.tick(now, isAnimation);
    try {
      if (drawScheduled) {
        updateCamera(TAU * camera.x, TAU * camera.y, camera.zoom);
        updateTime(timer.t);
        if (recompileScheduled) {
          clearOutput();
          const result = Module.evaluate_script!(editor.state.doc.toString());
          if (result.isError) {
            compilationState = CompilationState.Error;
            console.error(result.error);
          } else {
            compilationState = CompilationState.Success;
            isAnimation = result.isAnimated;
            recompileFragmentShader(result.shaderSource);
          }
        }
        rerender();
      }
    } catch (e) {
      console.error(e);
    }

    drawScheduled = timer.state === TimerState.Playing;
    recompileScheduled = false;

    if (timer.state === TimerState.Playing) {
      playButton.classList.add('hidden');
      pauseButton.classList.remove('hidden');
    } else {
      playButton.classList.remove('hidden');
      pauseButton.classList.add('hidden');
    }
    timestampSpan.innerText = timer.t.toFixed(2);
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
    timer.state = TimerState.Playing;
  });
  pauseButton.addEventListener('click', (_e) => {
    timer.state = TimerState.Paused;
  });
  stopButton.addEventListener('click', (_e) => {
    timer.stop();
    draw(false);
  });

  fakeGetByID('canvas-container').addEventListener('input', (e) => {
    const target = e.target as HTMLInputElement;
    switch (target.name) {
      case 'view-type': {
        if (target.checked) {
          updateViewType(parseInt(target.value, 10));
          draw(false);
        }
        break;
      }
      case 'loop-mode': {
        if (target.checked) {
          switch (target.value) {
            case 'no-loop': timer.setLoopMode(LoopMode.NoLoop); break;
            case 'wrap': timer.setLoopMode(LoopMode.Wrap); break;
            case 'reverse': timer.setLoopMode(LoopMode.Reverse); break;
            default: throw new Error("invalid loop value " + target.value);
          }
        }
        break;
      }
      case 'loop-start': {
        timer.loopStart = parseFloat(target.value);
        break;
      }
      case 'loop-end': {
        timer.loopEnd = parseFloat(target.value);
        break;
      }
      default:
        throw new Error("unknown field " + target.name);
    }
  });

  const canvas = fakeGetByID('render-target') as HTMLCanvasElement;
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

  const outputContainer = fakeGetByID('output');
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

  let ctrlClickedAt = 0;
  const isTryingToEngageNumberDrag = () => {
    return performance.now() - ctrlClickedAt < 100;
  };

  draw(true);
  editor.focus();
}

if (true)
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
  renderSolid(() => <Bauble script={script} />, placeholder);
}

if (false)
document.addEventListener("DOMContentLoaded", (_) => {
  for (const el of document.querySelectorAll('.code-example')) {
    const placeholder = el.nextElementSibling! as HTMLElement;
    makeBauble(el.textContent!, placeholder);
  }
});
