import type {Component, JSX} from 'solid-js';
import {batch, on, createEffect, createSelector, onMount} from 'solid-js';
import {Timer, LoopMode, TimerState} from './timer'
import installCodeMirror from './editor';
import {EditorView} from '@codemirror/view';
import Renderer from './renderer';
import * as Signal from './signals';
import {mod, TAU} from './util';
import type {Seconds} from './types';

enum EvaluationState {
  Unknown,
  Success,
  EvaluationError,
  ShaderCompilationError,
}

const defaultCamera = {
  x: -0.125,
  y: 0.125,
  zoom: 2.0,
};

const cameraRotateSpeed = 1 / 512;
const cameraZoomSpeed = 0.01;

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

type GestureEventHandlerUnion<T> = JSX.EventHandlerUnion<T, GestureEvent>

declare module 'solid-js' {
  namespace JSX {
    interface HTMLAttributes<T> {
      onGestureStart?: GestureEventHandlerUnion<T>;
      onGestureChange?: GestureEventHandlerUnion<T>;
      onGestureEnd?: GestureEventHandlerUnion<T>;
    }
  }
}

const Icon: Component<{name: string}> = (props) => {
  const href = `/icons.svg#${props.name}`;
  return <svg><use href={href} /></svg>;
};

const EditorToolbar: Component<{scriptDirty: boolean}> = (props) => {
  createEffect(() => {
    console.log("editor logging:", props.scriptDirty);
  });
  return <div class="toolbar">
    <div class="spacer"></div>
    <div title="Compilation result unknown" class="indicator compilation-unknown"><svg><use href="/icons.svg#emoji-neutral" /></svg></div>
    <div title="Compilation success" class="indicator compilation-success hidden"><svg><use href="/icons.svg#emoji-smile" /></svg></div>
    <div title="Compilation error" class="indicator compilation-error hidden"><svg><use href="/icons.svg#emoji-frown" /></svg></div>
  </div>;
};

type RenderToolbarProps = {
  viewType: Signal.T<number>,
  rotation: Signal.T<{x: number, y: number}>,
  zoom: Signal.T<number>,
};
const RenderToolbar: Component<RenderToolbarProps> = (props) => {
  const isSelected = createSelector(Signal.getter(props.viewType));

  const Choice: Component<{title: string, value: number, icon: string}> = (choiceProps) => {
    return <label title={choiceProps.title}>
      <input type="radio" autocomplete="off" name="view-type"
        value={choiceProps.value}
        checked={isSelected(choiceProps.value)}
        onChange={[Signal.setter(props.viewType), choiceProps.value]} />
      <Icon name={choiceProps.icon} />
    </label>
  };

  const resetCamera = () => {
    batch(() => {
      Signal.set(props.rotation, {x: defaultCamera.x, y: defaultCamera.y});
      Signal.set(props.zoom, defaultCamera.zoom);
    });
  };

  return <div class="toolbar">
    <button title="Reset camera" onClick={resetCamera}><Icon name="box" /></button>
    {/*<button title="Toggle quad view" data-action="toggle-quad-view"><svg><use href="/icons.svg#grid" /></svg></button>*/}
    <div class="spacer"></div>
    <fieldset>
      <Choice value={0} icon="camera" title="Render normally" />
      <Choice value={1} icon="magnet" title="Debug number of raymarching steps" />
      <Choice value={2} icon="arrows-collapse" title="Debug surface distance" />
    </fieldset>
  </div>;
};

const AnimationToolbar = () => {
  return <div class="toolbar">
    <button title="Play" data-action="play"><svg><use href="/icons.svg#play" /></svg></button>
    <button title="Pause" data-action="pause" class="hidden"><svg><use href="/icons.svg#pause" /></svg></button>
    <button title="Stop" data-action="stop"><svg><use href="/icons.svg#stop" /></svg></button>
    <span title="Current timestamp" class="timestamp">0.00</span>
    <div class="spacer"></div>
    {/* <div class="scrubber"></div>*/}
    <fieldset>
      <label title="No loop"><input type="radio" autocomplete="off" name="loop-mode" value="no-loop" checked /><svg><use href="/icons.svg#arrow-bar-right" /></svg></label>
      <label title="Loop"><input type="radio" autocomplete="off" name="loop-mode" value="wrap" /><svg><use href="/icons.svg#repeat" /></svg></label>
      <label title="Loop back and forth"><input type="radio" autocomplete="off" name="loop-mode" value="reverse" /><svg><use href="/icons.svg#arrow-left-right" /></svg></label>
    </fieldset>
    <input name="loop-start" inputmode="numeric" value="0.00" autocomplete="off" />
    <span class="text">to</span>
    <input name="loop-end" inputmode="numeric" value="6.28" autocomplete="off" />
  </div>;
};

class RenderLoop {
  private scheduled = false;
  private then: Seconds | null = null;
  private f: (_: Seconds) => boolean;
  constructor(f: (_: Seconds) => boolean) {
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
      if (this.f(elapsed as Seconds)) {
        this.schedule();
        this.then = nowSeconds;
      } else {
        this.then = null;
      }
    });
  }
}

const Bauble = (props: { script: string }) => {
  let canvasContainer: HTMLDivElement;
  let editorContainer: HTMLDivElement;
  let canvas: HTMLCanvasElement;
  let editor: EditorView;

  let isGesturing = false;
  let gestureEndedAt = 0;

  let viewType = Signal.create(0);
  let zoom = Signal.create(defaultCamera.zoom);
  let rotation = Signal.create({x: defaultCamera.x, y: defaultCamera.y});
  let scriptDirty = Signal.create(true);
  let lastCompilationResult = Signal.create(EvaluationState.Unknown);
  let isAnimation = Signal.create(false);

  const timer = new Timer();

  onMount(() => {
    editor = installCodeMirror(props.script, editorContainer, () => Signal.set(scriptDirty, true));
    const renderer = new Renderer(canvas, timer.t, viewType, rotation, zoom);

    const renderLoop = new RenderLoop((elapsed) => batch(() => {
      const isAnimation_ = Signal.get(isAnimation);
      timer.tick(elapsed, isAnimation_);

      if (Signal.get(scriptDirty)) {
        // TODO: should do something to set like the stdout/stderr target here...
        const result = window.Module.evaluate_script!(editor.state.doc.toString());
        Signal.set(scriptDirty, false);
        if (result.isError) {
          Signal.set(lastCompilationResult, EvaluationState.EvaluationError);
          console.error(result.error);
        } else {
          try {
            renderer.recompileShader(result.shaderSource);
            Signal.set(lastCompilationResult, EvaluationState.Success);
            Signal.set(isAnimation, result.isAnimated);
          } catch (e) {
            Signal.set(lastCompilationResult, EvaluationState.ShaderCompilationError);
            console.error(e);
          }
        }
      }
      renderer.draw();
      return isAnimation_;
    }));

    Signal.onEffect([rotation, zoom, scriptDirty] as Signal.T<any>[], () => {
      renderLoop.schedule();
    });
  });

  let canvasPointerAt = [0, 0];
  let rotatePointerId: number | null = null;

  const onPointerDown = (e: PointerEvent) => {
    if (rotatePointerId === null) {
      e.preventDefault();
      canvasPointerAt = [e.offsetX, e.offsetY];
      canvas.setPointerCapture(e.pointerId);
      rotatePointerId = e.pointerId;
    }
  };

  const onPointerUp = (e: PointerEvent) => {
    e.preventDefault();
    if (e.pointerId === rotatePointerId) {
      rotatePointerId = null;
    }
  };

  const onPointerMove = (e: PointerEvent) => {
    if (e.pointerId !== rotatePointerId) {
      return;
    }
    e.preventDefault();
    const pointerWasAt = canvasPointerAt;
    canvasPointerAt = [e.offsetX, e.offsetY];

    if (isGesturing) {
      return;
    }
    // if you were just trying to zoom, we don't want to do a little tiny
    // pan as you lift your second finger. so we wait 100ms before we allow
    // panning to continue
    if (performance.now() - gestureEndedAt < 100) { return; }

    const movementX = canvasPointerAt[0] - pointerWasAt[0];
    const movementY = canvasPointerAt[1] - pointerWasAt[1];
    // TODO: pixelScale shouldn't be hardcoded
    const pixelScale = 0.5;
    const scaleAdjustmentX = pixelScale * canvas.width / canvas.clientWidth;
    const scaleAdjustmentY = pixelScale * canvas.height / canvas.clientHeight;
    // TODO: invert the meaning of camera.x/y so that this actually makes sense
    Signal.update(rotation, ({x, y}) => ({
      x: mod(x - scaleAdjustmentY * cameraRotateSpeed * movementY, 1.0),
      y: mod(y - scaleAdjustmentX * cameraRotateSpeed * movementX, 1.0)
    }));
  };

  const onWheel = (e: WheelEvent) => {
    e.preventDefault();
    // Linux Firefox users who do not set MOZ_USE_XINPUT2
    // will report very large values of deltaY, resulting
    // in very choppy scrolling. I don't really know a good
    // way to fix this without explicit platform detection.
    Signal.update(zoom, (x) => x + cameraZoomSpeed * e.deltaY);
  };

  let initialZoom = 1;
  const onGestureStart = () => {
    initialZoom = Signal.get(zoom);
    isGesturing = true;
  };
  const onGestureChange = (e: GestureEvent) => {
    Signal.set(zoom, initialZoom / e.scale);
  };
  const onGestureEnd = () => {
    isGesturing = false;
    gestureEndedAt = performance.now();
  };

  return <div class="bauble standalone">
    <div class="code-and-preview">
      <div class="canvas-container" ref={canvasContainer!}>
        <RenderToolbar viewType={viewType} rotation={rotation} zoom={zoom} />
        <canvas ref={canvas!} class="render-target" width="1024" height="1024"
          onWheel={onWheel}
          onPointerDown={onPointerDown}
          onPointerUp={onPointerUp}
          onPointerMove={onPointerMove}
          onGestureStart={onGestureStart}
          onGestureChange={onGestureChange}
          onGestureEnd={onGestureEnd}
        />
        <AnimationToolbar />
      </div>
      <div class="code-container">
        <EditorToolbar scriptDirty={Signal.get(scriptDirty)} />
        <div class="editor-container" ref={editorContainer!} />
      </div>
    </div>
    <div class="output-resize-handle" />
    <pre class="output" />
  </div>;
};
export default Bauble;
