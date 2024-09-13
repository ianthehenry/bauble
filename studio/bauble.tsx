import type {Component, JSX} from 'solid-js';
import {batch, createEffect, createMemo, createSelector, onMount, For, Switch, Match} from 'solid-js';
import {Timer, LoopMode, TimerState} from './timer';
import installCodeMirror from './editor';
import {EditorView} from '@codemirror/view';
import * as Signal from './signals';
import {mod, clamp} from './util';
import {vec2, vec3} from 'gl-matrix';
import type {Seconds} from './types';
import type {Definition} from 'bauble-runtime';
import RenderLoop from './render-loop';
import * as RenderState from './render-state';
import type Mailbox from './mailbox';
import type {Property} from 'csstype';

enum EvaluationState {
  Unknown,
  Success,
  EvaluationError,
  ShaderCompilationError,
}

const defaultCamera = {
  origin: vec3.fromValues(0, 0, 0),
  rotation: vec2.fromValues(0.125, -0.125),
  zoom: 1.0,
};

const cameraRotateSpeed = 1 / 512;
const cameraZoomSpeed = 0.01;
const cameraPanSpeed = 1.5;

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
  return <svg><use href={`/icons.svg#${props.name}`} /></svg>;
};

interface ChoiceDescription<T> {
  title: string,
  value: T,
  icon: string,
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
  </fieldset>;
}

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
        <div title="Shader compilation error" class="indicator compilation-error">
          <Icon name="emoji-angry" />
        </div>
      </Match>
    </Switch>
  </div>;
};

const resetCamera = (
  rotation: Signal.T<vec2>,
  origin: Signal.T<vec3>,
  zoom: Signal.T<number>,
) => {
  batch(() => {
    Signal.set(rotation, defaultCamera.rotation);
    Signal.set(origin, defaultCamera.origin);
    Signal.set(zoom, defaultCamera.zoom);
  });
};

interface RenderToolbarProps {
  renderType: Signal.T<RenderState.RenderType>,
  usingFreeCamera: Signal.Accessor<boolean>,
  freeCamera: Signal.T<boolean>,
  quadView: Signal.T<boolean>,
  rotation: Signal.T<vec2>,
  origin: Signal.T<vec3>,
  zoom: Signal.T<number>,
}

const RenderToolbar: Component<RenderToolbarProps> = (props) => {
  const toggleQuadView = () => {
    Signal.update(props.quadView, (x) => !x);
  };
  const toggleFreeCamera = () => {
    Signal.update(props.freeCamera, (x) => !x);
  };

  return <div class="toolbar">
    <button title="Toggle free camera" onClick={toggleFreeCamera}>
      <Icon name={props.usingFreeCamera() ? "camera-reels" : "camera-reels-fill"} />
    </button>
    <button title="Reset camera" onClick={() => {
      Signal.set(props.freeCamera, true);
      resetCamera(props.rotation, props.origin, props.zoom);
      }}>
      <Icon name="box" />
    </button>
    <button title="Toggle quad view" onClick={toggleQuadView}>
      <Icon name={Signal.get(props.quadView) ? "grid-fill" : "grid"} />
    </button>
    <div class="spacer"></div>
    {choices(props.renderType, [
      { value: RenderState.RenderType.Normal, icon: "camera", title: "Render normally" },
      { value: RenderState.RenderType.Surfaceless, icon: "circle", title: "Use default surface" },
      { value: RenderState.RenderType.Convergence, icon: "magnet", title: "Debug number of raymarching steps" },
      { value: RenderState.RenderType.Distance, icon: "arrows-collapse", title: "Debug surface distance" },
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
    }} />;
};

interface AnimationToolbarProps {
  timer: Timer,
}
const AnimationToolbar: Component<AnimationToolbarProps> = (props) => {
  return <div class="toolbar">
    <button
      title={Signal.get(props.timer.state) === TimerState.Playing ? "Pause" : "Play"}
      onClick={() => props.timer.playPause()}>
      <Icon name={Signal.get(props.timer.state) === TimerState.Playing ? "pause" : "play"} />
    </button>
    <button title="Stop" onClick={() => props.timer.stop()}><Icon name="stop" /></button>
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

// TODO: what is the correct way to write this type?
const ResizableArea = (props: {ref: any}) => {
  let outputContainer: HTMLPreElement;
  let handlePointerAt = 0;
  onMount(() => props.ref(outputContainer as HTMLElement));
  return <>
    <div class="resize-handle output-resize-handle"
      title="double click to auto size"
      onPointerDown={(e) => {
        e.currentTarget.setPointerCapture(e.pointerId);
        handlePointerAt = e.screenY;
      }}
      onDblClick={() => {
        outputContainer.style.flexBasis = null!;
        outputContainer.style.maxHeight = null!;
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
        handlePointerAt = e.screenY;
        const delta = handlePointerAt - handlePointerWasAt;
        outputContainer.style.flexBasis = `${oldHeight - delta}px`;
        outputContainer.style.maxHeight = '100%';
        outputContainer.scrollTop = clamp(oldScrollTop + delta, 0, outputContainer.scrollHeight - outputContainer.offsetHeight);
      }}
    />
    <pre class="output-container" ref={outputContainer!} />
  </>;
};

function assert(expr: boolean) {
  if (!expr) {
    throw new Error("assertion failure");
  }
}

// a one-in-flight job scheduler
class Throttle {
  private currentJob : Promise<void> | undefined = undefined;
  private nextJob : (() => Promise<void>) | undefined = undefined;

  schedule(job : (() => Promise<void>)) {
    if (this.currentJob == undefined) {
      assert(this.nextJob === undefined);
      this.currentJob = job().then(() => {
        this.currentJob = undefined;
        if (this.nextJob !== undefined) {
          const nextJob = this.nextJob;
          this.nextJob = undefined;
          this.schedule(nextJob);
        }
      });
    } else {
      this.nextJob = job;
    }
  }
}

enum Interaction {
  Rotate,
  PanXY,
  PanZY,
  PanXZ,
  ResizeSplit,
}

interface BaubleProps {
  initialScript: string,
  focusable: boolean,
  canSave: boolean,
  definitions: Array<Definition>,
  wasmBox: Mailbox,
  renderBox: Mailbox,
  size: {width: number, height: number},
}

class AsyncRenderer {
  constructor(private mailbox: Mailbox, canvas: HTMLCanvasElement, state: RenderState.Accessors) {
    const offscreenCanvas = canvas.transferControlToOffscreen();

    const stateJoined = RenderState.accessAll(state);
    createEffect(() => {
      this.mailbox.send({tag: 'set', state: stateJoined()});
    });
    this.mailbox.send({tag: 'init', canvas: offscreenCanvas}, [offscreenCanvas]);
  }
  recompileShader(source: string) {
    return this.mailbox.send({tag: 'shader', source: source});
  }
}

const Bauble = (props: BaubleProps) => {
  const {definitions, wasmBox, renderBox} = props;
  let canvasContainer: HTMLDivElement;
  let editorContainer: HTMLDivElement;
  let canvas: HTMLCanvasElement;
  let editor: EditorView;
  let outputContainer: HTMLElement;

  let isGesturing = false;
  let gestureEndedAt = 0;

  const canvasSize = Signal.create(props.size);
  const pixelRatio = Signal.create(window.devicePixelRatio);
  const imageRendering: Signal.T<Property.ImageRendering> = Signal.create('auto' as Property.ImageRendering);
  const canvasResolution = createMemo(() => {
    const dpr = Signal.get(pixelRatio);
    const size = Signal.get(canvasSize);
    return {width: dpr * size.width, height: dpr * size.height};
  });
  const renderType = Signal.create(RenderState.RenderType.Normal);
  const freeCamera = Signal.create(false);
  const quadView = Signal.create(false);
  const quadSplitPoint = Signal.create(vec2.fromValues(0.5, 0.5));
  const zoom = Signal.create(defaultCamera.zoom);
  const rotation = Signal.create(defaultCamera.rotation);
  const origin = Signal.create(defaultCamera.origin);
  const evaluationState = Signal.create(EvaluationState.Unknown);
  const hasCamera = Signal.create(false);
  const isAnimated = Signal.create(false);
  const isVisible = Signal.create(false);
  const usingFreeCamera = createMemo(() => Signal.get(freeCamera) || !Signal.get(hasCamera));

  const timer = new Timer();

  const intersectionObserver = new IntersectionObserver((entries) => {
    for (const entry of entries) {
      Signal.set(isVisible, entry.isIntersecting);
    }
  });

  // TODO: the whole timeAdvancer thing is pretty weird. This is
  // fallout from the switch to asynchronous rendering. It made more
  // sense when this was also driving the render loop. Now it just
  // increments a timestamp... this could be cleaned up a lot.
  let timeAdvancer : RenderLoop;
  let renderer : AsyncRenderer;
  const compileQueue = new Throttle();

  const print = (line: string, isErr: boolean) => {
    const span = document.createElement('span');
    span.classList.toggle('err', isErr);
    span.appendChild(document.createTextNode(line));
    span.appendChild(document.createTextNode('\n'));
    outputContainer.appendChild(span);
  };

  const flush = (outputs: Array<[string, boolean]>) => {
    outputContainer.innerHTML = '';
    for (let [line, isErr] of outputs) {
      print(line, isErr);
    }
  };

  const recompile = () => {
    compileQueue.schedule(async () => {
      Signal.set(evaluationState, EvaluationState.Unknown);
      const request = {tag: 'compile', script: editor.state.doc.toString()};
      const result: any = await wasmBox.send(request);
      if (result.isError) {
        Signal.set(evaluationState, EvaluationState.EvaluationError);
        flush(result.outputs);
      } else {
        try {
          //console.log(result.shaderSource);
          const shaderRecompilationTimeMs = await renderer.recompileShader(result.shaderSource) as number | undefined;
          Signal.set(evaluationState, EvaluationState.Success);
          Signal.set(isAnimated, result.isAnimated);
          Signal.set(hasCamera, result.hasCamera);

          flush(result.outputs);
          // TODO: this isn't really the best way to surface this information
          print(`eval ${result.evalTimeMs.toFixed(0)}ms glsl ${result.compileTimeMs.toFixed(0)}ms` +
            (shaderRecompilationTimeMs
              ? ` shader ${shaderRecompilationTimeMs.toFixed(0)}ms`
              : ``)
            , false);
        } catch (e: any) {
          Signal.set(evaluationState, EvaluationState.ShaderCompilationError);
          flush(result.outputs);
          print(e.toString(), true);
          if (e.cause != null) {
            print(e.cause, true);
          }
        }
      }
    });
  };

  onMount(() => {
    intersectionObserver.observe(canvas);

    editor = installCodeMirror({
      initialScript: props.initialScript,
      parent: editorContainer,
      canSave: props.canSave,
      onChange: recompile,
      definitions: definitions,
    });
    renderer = new AsyncRenderer(renderBox, canvas, {
      time: Signal.getter(timer.t),
      isVisible: Signal.getter(isVisible),
      renderType: Signal.getter(renderType),
      rotation: Signal.getter(rotation),
      origin: Signal.getter(origin),
      zoom: Signal.getter(zoom),
      freeCamera: Signal.getter(freeCamera),
      quadView: Signal.getter(quadView),
      quadSplitPoint: Signal.getter(quadSplitPoint),
      resolution: canvasResolution,
    });

    timeAdvancer = new RenderLoop((elapsed) => batch(() => {
     const isAnimated_ = Signal.get(isAnimated);
     const isTimeAdvancing = isAnimated_ && Signal.get(timer.state) !== TimerState.Paused;
     if (isTimeAdvancing) {
       // If you hit the stop button, we want to redraw at zero,
       // but we don't want to advance time forward by 16ms.
       timer.tick(elapsed, isAnimated_);
       // Normally the advancing of time is sufficient
       // to reschedule the loop, but if you're just
       // resuming after a stop the initial elapsed time
       // is 0.
       if (elapsed === 0) {
         timeAdvancer.schedule();
       }
     }
    }));

    Signal.onEffect([
      timer.state,
      timer.t,
      isAnimated,
    ], () => {
      timeAdvancer.schedule();
    });
    requestAnimationFrame(() => {
      // onMount is called before the children have been mounted,
      // apparently, which means that all of our refs are not
      // actually set up yet, so we can't invoke this synchronously
      recompile();
    });
  });

  let canvasPointerAt = [0, 0];
  let interactionPointer: number | null = null;
  let interaction: Interaction | null = null;

  const getRelativePoint = (e: MouseEvent) => vec2.fromValues(e.offsetX / canvas.offsetWidth, e.offsetY / canvas.offsetHeight);
  const isOnSplitPoint = (e: MouseEvent) => {
    const size = vec2.fromValues(canvas.offsetWidth, canvas.offsetHeight);
    const splitPointPixels = vec2.clone(Signal.get(quadSplitPoint));
    vec2.mul(splitPointPixels, splitPointPixels, size);
    return vec2.distance(splitPointPixels, [e.offsetX, e.offsetY]) < 10;
  };

  const isOnCustomCamera = (e: MouseEvent) => {
    let isInPerspective;
    if (Signal.get(quadView)) {
      const mouse = getRelativePoint(e);
      const quadSplit = Signal.get(quadSplitPoint);
      isInPerspective = mouse[0] < quadSplit[0] && mouse[1] < quadSplit[1];
    } else {
      isInPerspective = true;
    }
    return isInPerspective ? !usingFreeCamera() : false;
  };

  const getCursorStyle = (e: PointerEvent) => {
    if (interaction == null) {
      if (interactionPointer != null || isOnCustomCamera(e)) {
        return 'default';
      }
      return Signal.get(quadView) && isOnSplitPoint(e) ? 'move' : 'grab';
    } else if (interaction === Interaction.ResizeSplit) {
      return 'move';
    } else {
      return 'grabbing';
    }
  }

  const setCursorStyle = (e: PointerEvent) => {
    canvas.style.cursor = getCursorStyle(e);
  };

  const getInteraction = (e: MouseEvent) => {
    if (Signal.get(quadView)) {
      if (isOnSplitPoint(e)) {
        return Interaction.ResizeSplit;
      } else {
        const relativePoint = getRelativePoint(e);
        const splitPoint = Signal.get(quadSplitPoint);
        if (relativePoint[1] < splitPoint[1]) {
          if (relativePoint[0] < splitPoint[0]) {
            if (usingFreeCamera()) {
              return Interaction.Rotate;
            } else {
              return null;
            }
          } else {
            return Interaction.PanXZ;
          }
        } else {
          if (relativePoint[0] < splitPoint[0]) {
            return Interaction.PanXY;
          } else {
            return Interaction.PanZY;
          }
        }
      }
    } else if (usingFreeCamera()) {
      return Interaction.Rotate;
    } else {
      return null;
    }
  };

  const onPointerDown = (e: PointerEvent) => {
    if (interactionPointer != null) {
      return;
    }
    e.preventDefault();
    if (props.focusable) {
      canvas.focus();
    }
    canvasPointerAt = [e.offsetX, e.offsetY];
    canvas.setPointerCapture(e.pointerId);
    interactionPointer = e.pointerId;
    interaction = getInteraction(e);
    setCursorStyle(e);
  };

  const onPointerUp = (e: PointerEvent) => {
    e.preventDefault();
    if (e.pointerId === interactionPointer) {
      interactionPointer = null;
      interaction = null;
    }
    setCursorStyle(e);
  };

  const onDblClick = (e: MouseEvent) => {
    if (Signal.get(quadView)) {
      switch (getInteraction(e)) {
      case Interaction.Rotate:
        batch(() => {
          Signal.set(rotation, defaultCamera.rotation);
          Signal.set(zoom, defaultCamera.zoom);
        });
        break;
      case Interaction.PanXY: Signal.update(origin, (old) => vec3.fromValues(defaultCamera.origin[0], defaultCamera.origin[1], old[2])); break;
      case Interaction.PanZY: Signal.update(origin, (old) => vec3.fromValues(old[0], defaultCamera.origin[1], defaultCamera.origin[2])); break;
      case Interaction.PanXZ: Signal.update(origin, (old) => vec3.fromValues(defaultCamera.origin[0], old[1], defaultCamera.origin[2])); break;
      case Interaction.ResizeSplit: Signal.set(quadSplitPoint, vec2.fromValues(0.5, 0.5)); break;
      }
    } else {
      if (usingFreeCamera()) {
        resetCamera(rotation, origin, zoom);
      } else {
        Signal.set(freeCamera, true);
      }
    }
  };

  const onPointerMove = (e: PointerEvent) => {
    setCursorStyle(e);

    if (e.pointerId !== interactionPointer) {
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

    const size = Signal.get(canvasSize);
    const deltaX = (canvasPointerAt[0] - pointerWasAt[0]) * (size.width / canvas.clientWidth);
    const deltaY = (canvasPointerAt[1] - pointerWasAt[1]) * (size.height / canvas.clientHeight);
    const panRate = Signal.get(zoom) * cameraPanSpeed;

    switch (interaction!) {
    case Interaction.Rotate: {
      Signal.update(rotation, (old) =>
        vec2.fromValues(
          mod(old[0] - deltaX * cameraRotateSpeed, 1.0),
          mod(old[1] - deltaY * cameraRotateSpeed, 1.0)));
      break;
    }
    case Interaction.PanXY: {
      Signal.update(origin, (old) =>
        vec3.fromValues(old[0] - deltaX * panRate, old[1] + deltaY * panRate, old[2]));
      break;
    }
    case Interaction.PanZY: {
      Signal.update(origin, (old) =>
        vec3.fromValues(old[0], old[1] + deltaY * panRate, old[2] - deltaX * panRate));
      break;
    }
    case Interaction.PanXZ: {
      Signal.update(origin, (old) =>
        vec3.fromValues(old[0] - deltaX * panRate, old[1], old[2] + deltaY * panRate));
      break;
    }
    case Interaction.ResizeSplit: {
      const deltaX = (canvasPointerAt[0] - pointerWasAt[0]) / canvas.clientWidth;
      const deltaY = (canvasPointerAt[1] - pointerWasAt[1]) / canvas.clientHeight;
      Signal.update(quadSplitPoint, (old) =>
        vec2.fromValues(old[0] + deltaX, old[1] + deltaY));
      break;
    }
    }
  };

  const onWheel = (e: WheelEvent) => {
    if (props.focusable && document.activeElement !== canvas) {
      return;
    }
    e.preventDefault();
    // Linux Firefox users who do not set MOZ_USE_XINPUT2
    // will report very large values of deltaY, resulting
    // in very choppy scrolling. I don't really know a good
    // way to fix this without explicit platform detection.
    Signal.update(zoom, (x) => Math.max(0, x + cameraZoomSpeed * e.deltaY));
  };

  let initialZoom = 1;
  const onGestureStart = () => {
    initialZoom = Signal.get(zoom);
    isGesturing = true;
  };
  const onGestureChange = (e: GestureEvent) => {
    Signal.set(zoom, Math.max(0, initialZoom / e.scale));
  };
  const onGestureEnd = () => {
    isGesturing = false;
    gestureEndedAt = performance.now();
  };

  let codeContainer: HTMLDivElement;
  let handlePointerAt = [0, 0];
  const onHandlePointerDown = (e: PointerEvent & {currentTarget: HTMLDivElement}) => {
    e.currentTarget.setPointerCapture(e.pointerId);
    handlePointerAt = [e.screenX, e.screenY];
  };
  const onHandleDblClick = () => {
    // TODO: width or height!
    codeContainer.style.flexBasis = `var(--canvas-width)`;
    canvasContainer.style.flexBasis = 'var(--canvas-width)';
  };
  const onHandlePointerMove = (e: PointerEvent & {currentTarget: HTMLDivElement}) => {
    if (!e.currentTarget.hasPointerCapture(e.pointerId)) {
      return;
    }
    const isVertical = getComputedStyle(e.currentTarget.parentElement!).flexDirection === 'column';
    const containerStyle = getComputedStyle(canvasContainer);

    const padding = isVertical
      ? parseFloat(containerStyle.paddingTop) + parseFloat(containerStyle.paddingBottom)
      : parseFloat(containerStyle.paddingLeft) + parseFloat(containerStyle.paddingRight);
    const oldSize = (isVertical ? canvasContainer.offsetHeight : canvasContainer.offsetWidth) - padding;

    const handlePointerWasAt = handlePointerAt;
    handlePointerAt = [e.screenX, e.screenY];
    const delta = isVertical
      ? handlePointerWasAt[1] - handlePointerAt[1]
      : handlePointerAt[0] - handlePointerWasAt[0];
    codeContainer.style.flexBasis = `0`;
    canvasContainer.style.flexBasis = `${oldSize - delta}px`;
  };

  return <div class="bauble" style={{
    '--canvas-width': `${Signal.get(canvasSize).width}px`,
    '--canvas-height': `${Signal.get(canvasSize).height}px`,
  }}>
    <div class="canvas-container" ref={canvasContainer!}>
      <RenderToolbar
      renderType={renderType}
      usingFreeCamera={usingFreeCamera}
      freeCamera={freeCamera}
      quadView={quadView}
      rotation={rotation}
      origin={origin}
      zoom={zoom} />
      <canvas
        ref={canvas!}
        class="render-target"
        style={{'image-rendering': Signal.get(imageRendering)}}
        width={canvasResolution().width}
        height={canvasResolution().height}
        tabindex={props.focusable ? 0 : undefined}
        onWheel={onWheel}
        onDblClick={onDblClick}
        onPointerDown={onPointerDown}
        onPointerUp={onPointerUp}
        onPointerMove={onPointerMove}
        onGestureStart={onGestureStart}
        onGestureChange={onGestureChange}
        onGestureEnd={onGestureEnd}
      />
      <AnimationToolbar timer={timer} />
    </div>
    <div class="resize-handle canvas-resize-handle"
      title="double click to auto size"
      onPointerDown={onHandlePointerDown}
      onPointerMove={onHandlePointerMove}
      onDblClick={onHandleDblClick}
    />
    <div class="code-container" ref={codeContainer!}>
      <EditorToolbar state={Signal.get(evaluationState)} />
      <div class="editor-container" ref={editorContainer!} />
      <ResizableArea ref={outputContainer!} />
    </div>
  </div>;
};
export default Bauble;
