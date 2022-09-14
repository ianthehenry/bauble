import type {Component, JSX} from 'solid-js';
import {batch, on, createEffect, createSelector, onMount, For, Switch, Match} from 'solid-js';
import {Timer, LoopMode, TimerState} from './timer'
import installCodeMirror from './editor';
import {EditorView} from '@codemirror/view';
import Renderer from './renderer';
import * as Signal from './signals';
import {mod, clamp, TAU} from './util';
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

interface ChoiceDescription<T> {
  title: string,
  value: T,
  icon: string,
}

interface ChoicesProps<T extends number | string> {
  signal: Signal.T<T>,
  choices: ChoiceDescription<T>[],
}

function choices<T extends number | string>(
  signal: Signal.T<T>,
  choices: ChoiceDescription<T>[]
): JSX.Element {
  const isSelected = createSelector(Signal.getter(signal));
  const setter = Signal.setter(signal);

  return <fieldset>
    <For each={choices}>{ ({title, value, icon}) =>
      <label title={title}>
        <input
          type="radio"
          autocomplete="off"
          value={value}
          checked={isSelected(value)}
          onChange={[setter, value]} />
        <Icon name={icon} />
      </label>
    }</For>
  </fieldset>
};

const EditorToolbar: Component<{state: EvaluationState}> = (props) => {
  return <div class="toolbar">
    <div class="spacer"></div>
    <Switch>
      <Match when={props.state === EvaluationState.Unknown}>
        <div title="Compilation unknown" class="indicator compilation-unknown">
          <Icon name="emoji-neutral" />
        </div>
      </Match>
      <Match when={props.state === EvaluationState.Success}>
        <div title="Compilation success" class="indicator compilation-success">
          <Icon name="emoji-smile" />
        </div>
      </Match>
      <Match when={props.state === EvaluationState.EvaluationError}>
        <div title="Compilation error" class="indicator compilation-error">
          <Icon name="emoji-frown" />
        </div>
      </Match>
      <Match when={props.state === EvaluationState.ShaderCompilationError}>
        <div title="Compilation error" class="indicator compilation-error">
          <Icon name="emoji-angry" />
        </div>
      </Match>
    </Switch>
  </div>;
};

interface RenderToolbarProps {
  viewType: Signal.T<number>,
  rotation: Signal.T<{x: number, y: number}>,
  zoom: Signal.T<number>,
};
const RenderToolbar: Component<RenderToolbarProps> = (props) => {
  const isSelected = createSelector(Signal.getter(props.viewType));

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
    {choices(props.viewType, [
      { value: 0, icon: "camera", title: "Render normally" },
      { value: 1, icon: "magnet", title: "Debug number of raymarching steps" },
      { value: 2, icon: "arrows-collapse", title: "Debug surface distance" },
    ])}
  </div>;
};

const timestampInput = (signal: Signal.T<Seconds>): JSX.Element => {
  return <input
    inputmode="numeric"
    value={Signal.get(signal).toFixed(2)}
    autocomplete="off"
    onChange={(e) => {
      Signal.set(signal, parseInt(e.currentTarget.value, 10) as Seconds);
    }} />
}

interface AnimationToolbarProps {
  timer: Timer,
};
const AnimationToolbar: Component<AnimationToolbarProps> = (props) => {
  return <div class="toolbar">
    <button title="Play" data-action="play"><svg><use href="/icons.svg#play" /></svg></button>
    <button title="Pause" data-action="pause" class="hidden"><svg><use href="/icons.svg#pause" /></svg></button>
    <button title="Stop" data-action="stop"><svg><use href="/icons.svg#stop" /></svg></button>
    <span title="Current timestamp" class="timestamp">{Signal.get(props.timer.t).toFixed(2)}</span>
    <div class="spacer"></div>
    {/* <div class="scrubber"></div>*/}
    {choices(props.timer.loopMode, [
      { value: LoopMode.NoLoop, icon: "arrow-bar-right", title: "No loop" },
      { value: LoopMode.Wrap, icon: "repeat", title: "Loop" },
      { value: LoopMode.Reverse, icon: "arrow-left-right", title: "Loop back and forth" },
    ])}
    {timestampInput(props.timer.loopStart)}
    <span class="text">to</span>
    {timestampInput(props.timer.loopEnd)}
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

// TODO: what is the correct way to write this type?
const ResizableArea = (props: {ref: any}) => {
  let outputContainer: HTMLPreElement;
  let handlePointerAt = [0, 0];
  onMount(() => props.ref(outputContainer as HTMLElement));
  return <>
    <div class="output-resize-handle"
      title="double click to auto size"
      onPointerDown={(e) => {
        e.currentTarget.setPointerCapture(e.pointerId);
        handlePointerAt = [e.screenX, e.screenY];
      }}
      onDblClick={(e) => {
        outputContainer.style.flexBasis = 'auto';
      }}
      onPointerMove={(e) => {
        if (!e.currentTarget.hasPointerCapture(e.pointerId)) {
          return;
        }
        const outputStyle = getComputedStyle(outputContainer);
        const verticalPadding = parseFloat(outputStyle.paddingTop) + parseFloat(outputStyle.paddingBottom);
        const oldHeight = outputContainer.offsetHeight - verticalPadding;
        const oldScrollTop = outputContainer.scrollTop;
        const handlePointerWasAt = handlePointerAt;
        handlePointerAt = [e.screenX, e.screenY];
        const delta = handlePointerAt[1] - handlePointerWasAt[1];
        outputContainer.style.flexBasis = `${oldHeight - delta}px`;
        outputContainer.scrollTop = clamp(oldScrollTop + delta, 0, outputContainer.scrollHeight - outputContainer.offsetHeight);
      }}
      />
    <pre class="output-container" ref={outputContainer!} />
  </>
}

interface BaubleProps {
  initialScript: string,
  hijackScroll: boolean,
}
const Bauble = (props: BaubleProps) => {
  let canvasContainer: HTMLDivElement;
  let editorContainer: HTMLDivElement;
  let canvas: HTMLCanvasElement;
  let editor: EditorView;
  let outputContainer: HTMLElement;

  let isGesturing = false;
  let gestureEndedAt = 0;

  const viewType = Signal.create(0);
  const zoom = Signal.create(defaultCamera.zoom);
  const rotation = Signal.create({x: defaultCamera.x, y: defaultCamera.y});
  const scriptDirty = Signal.create(true);
  const evaluationState = Signal.create(EvaluationState.Unknown);
  const isAnimation = Signal.create(false);

  const timer = new Timer();

  onMount(() => {
    editor = installCodeMirror(props.initialScript, editorContainer, () => Signal.set(scriptDirty, true));
    const renderer = new Renderer(canvas, timer.t, viewType, rotation, zoom);

    const renderLoop = new RenderLoop((elapsed) => batch(() => {
      const isAnimation_ = Signal.get(isAnimation);
      timer.tick(elapsed, isAnimation_);

      if (Signal.get(scriptDirty)) {
        outputContainer.innerHTML = '';
        window.Module.outputTarget = outputContainer;
        const result = window.Module.evaluate_script!(editor.state.doc.toString());
        window.Module.outputTarget = undefined;
        Signal.set(scriptDirty, false);
        if (result.isError) {
          Signal.set(evaluationState, EvaluationState.EvaluationError);
          console.error(result.error);
        } else {
          try {
            renderer.recompileShader(result.shaderSource);
            Signal.set(evaluationState, EvaluationState.Success);
            Signal.set(isAnimation, result.isAnimated);
          } catch (e) {
            Signal.set(evaluationState, EvaluationState.ShaderCompilationError);
            console.error(e);
          }
        }
      }
      renderer.draw();
      return Signal.get(isAnimation);
    }));

    Signal.onEffect([rotation, zoom, scriptDirty, viewType] as Signal.T<any>[], () => {
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

  return <div class="bauble">
    <div class="code-and-preview">
      <div class="canvas-container" ref={canvasContainer!}>
        <RenderToolbar viewType={viewType} rotation={rotation} zoom={zoom} />
        <canvas ref={canvas!} class="render-target" width="1024" height="1024"
          onWheel={props.hijackScroll ? onWheel : undefined}
          onPointerDown={onPointerDown}
          onPointerUp={onPointerUp}
          onPointerMove={onPointerMove}
          onGestureStart={onGestureStart}
          onGestureChange={onGestureChange}
          onGestureEnd={onGestureEnd}
        />
        <AnimationToolbar timer={timer} />
      </div>
      <div class="code-container">
        <EditorToolbar state={Signal.get(evaluationState)} />
        <div class="editor-container" ref={editorContainer!} />
      </div>
    </div>
    <ResizableArea ref={outputContainer!} />
  </div>;
};
export default Bauble;
