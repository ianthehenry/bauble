import type {Component, JSX} from 'solid-js';
import {batch, createMemo, createSelector, onMount, For, Switch, Match} from 'solid-js';
import {Timer, LoopMode, TimerState} from './timer';
import installCodeMirror from './editor';
import {EditorView} from '@codemirror/view';
import Renderer from './renderer';
import * as Signal from './signals';
import {mod, clamp} from './util';
import {vec2} from 'gl-matrix';
import type {Seconds} from './types';
import type {BaubleModule} from 'bauble-runtime';
import OutputChannel from './output-channel';
import RenderLoop from './render-loop';
import type {Property} from 'csstype';
import janetAutocomplete from './autocomplete';

enum EvaluationState {
  Unknown,
  Success,
  EvaluationError,
  ShaderCompilationError,
}

const defaultCamera = {
  origin: {
    x: 0,
    y: 0,
    z: 0,
  },
  rotation: {
    x: 0.125,
    y: -0.125,
  },
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
  rotation: Signal.T<{x: number, y: number}>,
  origin: Signal.T<{x: number, y: number, z: number}>,
  zoom: Signal.T<number>,
) => {
  batch(() => {
    Signal.set(rotation, defaultCamera.rotation);
    Signal.set(origin, defaultCamera.origin);
    Signal.set(zoom, defaultCamera.zoom);
  });
};

interface RenderToolbarProps {
  // TODO: render type should be an enum
  renderType: Signal.T<number>,
  quadView: Signal.T<boolean>,
  rotation: Signal.T<{x: number, y: number}>,
  origin: Signal.T<{x: number, y: number, z: number}>,
  zoom: Signal.T<number>,
}

const RenderToolbar: Component<RenderToolbarProps> = (props) => {
  const toggleQuadView = () => {
    Signal.update(props.quadView, (x) => !x);
  };

  return <div class="toolbar">
    <button title="Reset camera" onClick={() => resetCamera(props.rotation, props.origin, props.zoom)}>
      <Icon name="box" />
    </button>
    <button title="Toggle quad view" onClick={toggleQuadView}>
      <Icon name={Signal.get(props.quadView) ? "grid-fill" : "grid"} />
    </button>
    <div class="spacer"></div>
    {choices(props.renderType, [
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
  runtime: BaubleModule,
  outputChannel: OutputChannel,
  size: {width: number, height: number},
}
const Bauble = (props: BaubleProps) => {
  const {runtime, outputChannel} = props;
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
  const renderType = Signal.create(0);
  const quadView = Signal.create(false);
  const quadSplitPoint = Signal.create({x: 0.5, y: 0.5});
  const zoom = Signal.create(defaultCamera.zoom);
  const rotation = Signal.create(defaultCamera.rotation);
  const origin = Signal.create(defaultCamera.origin);
  const scriptDirty = Signal.create(true);
  const evaluationState = Signal.create(EvaluationState.Unknown);
  const isAnimation = Signal.create(false);
  const isVisible = Signal.create(false);

  const timer = new Timer();

  const intersectionObserver = new IntersectionObserver((entries) => {
    for (const entry of entries) {
      Signal.set(isVisible, entry.isIntersecting);
    }
  });

  onMount(() => {
    intersectionObserver.observe(canvas);
    editor = installCodeMirror({
      initialScript: props.initialScript,
      parent: editorContainer,
      canSave: props.canSave,
      onChange: () => Signal.set(scriptDirty, true),
      definitions: runtime.getDefinitions(),
    });
    // TODO: these should really be named
    const renderer = new Renderer(
      canvas,
      timer.t,
      renderType,
      rotation,
      origin,
      zoom,
      quadView,
      quadSplitPoint,
      canvasResolution,
    );

    const renderLoop = new RenderLoop((elapsed) => batch(() => {
      if (!Signal.get(isVisible)) {
        return;
      }
      const isAnimation_ = Signal.get(isAnimation);
      const isTimeAdvancing = isAnimation_ && Signal.get(timer.state) !== TimerState.Paused;
      if (isTimeAdvancing) {
        // If you hit the stop button, we want to redraw at zero,
        // but we don't want to advance time forward by 16ms.
        timer.tick(elapsed, isAnimation_);
        // Normally the advancing of time is sufficient
        // to reschedule the loop, but if you're just
        // resuming after a stop the initial elapsed time
        // is 0.
        if (elapsed === 0) {
          renderLoop.schedule();
        }
      }

      if (Signal.get(scriptDirty)) {
        outputContainer.innerHTML = '';
        outputChannel.target = outputContainer;
        const result = runtime.evaluateScript(editor.state.doc.toString());
        Signal.set(scriptDirty, false);
        if (result.isError) {
          Signal.set(evaluationState, EvaluationState.EvaluationError);
          console.error(result.error);
        } else {
          try {
            renderer.recompileShader(result.shaderSource);
            Signal.set(evaluationState, EvaluationState.Success);
            Signal.set(isAnimation, result.isAnimated);
          } catch (e: any) {
            Signal.set(evaluationState, EvaluationState.ShaderCompilationError);
            outputChannel.print(e.toString(), true);
            if (e.cause != null) {
              outputChannel.print(e.cause, true);
            }
          }
        }
        outputChannel.target = null;
      }
      renderer.draw();
    }));

    Signal.onEffect([
      isVisible,
      rotation,
      origin,
      zoom,
      scriptDirty,
      renderType,
      timer.state,
      timer.t,
      quadView,
      quadSplitPoint,
      canvasResolution,
    ] as Signal.T<any>[], () => {
      renderLoop.schedule();
    });
  });

  let canvasPointerAt = [0, 0];
  let interactionPointer: number | null = null;
  let interaction: Interaction | null = null;

  const getRelativePoint = (e: MouseEvent) => ({
    x: e.offsetX / canvas.offsetWidth,
    y: e.offsetY / canvas.offsetHeight,
  });
  const isOnSplitPoint = (e: MouseEvent) => {
    const splitPoint = Signal.get(quadSplitPoint);
    const size = vec2.fromValues(canvas.offsetWidth, canvas.offsetHeight);
    const splitPointPixels = vec2.fromValues(splitPoint.x, splitPoint.y);
    vec2.mul(splitPointPixels, splitPointPixels, size);
    return vec2.distance(splitPointPixels, [e.offsetX, e.offsetY]) < 10;
  };

  const setCursorStyle = (e: PointerEvent) => {
    if (interaction == null) {
      canvas.style.cursor = Signal.get(quadView) && isOnSplitPoint(e) ? 'move' : 'grab';
    } else if (interaction === Interaction.ResizeSplit) {
      canvas.style.cursor = 'move';
    } else {
      canvas.style.cursor = 'grabbing';
    }
  };

  const getInteraction = (e: MouseEvent) => {
    if (Signal.get(quadView)) {
      if (isOnSplitPoint(e)) {
        return Interaction.ResizeSplit;
      } else {
        const relativePoint = getRelativePoint(e);
        const splitPoint = Signal.get(quadSplitPoint);
        if (relativePoint.y < splitPoint.y) {
          if (relativePoint.x < splitPoint.x) {
            return Interaction.Rotate;
          } else {
            return Interaction.PanXZ;
          }
        } else {
          if (relativePoint.x < splitPoint.x) {
            return Interaction.PanXY;
          } else {
            return Interaction.PanZY;
          }
        }
      }
    } else {
      return Interaction.Rotate;
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
        case Interaction.PanXY: Signal.update(origin, ({z}) => ({x: defaultCamera.origin.x, y: defaultCamera.origin.y, z: z})); break;
        case Interaction.PanZY: Signal.update(origin, ({x}) => ({x: x, y: defaultCamera.origin.y, z: defaultCamera.origin.z})); break;
        case Interaction.PanXZ: Signal.update(origin, ({y}) => ({x: defaultCamera.origin.x, y: y, z: defaultCamera.origin.z})); break;
        case Interaction.ResizeSplit: Signal.set(quadSplitPoint, {x: 0.5, y: 0.5}); break;
      }
    } else {
      resetCamera(rotation, origin, zoom);
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
        Signal.update(rotation, ({x, y}) => ({
          x: mod(x - deltaX * cameraRotateSpeed, 1.0),
          y: mod(y - deltaY * cameraRotateSpeed, 1.0),
        }));
        break;
      }
      case Interaction.PanXY: {
        Signal.update(origin, ({x, y, z}) => ({
          x: x - deltaX * panRate,
          y: y + deltaY * panRate,
          z: z,
        }));
        break;
      }
      case Interaction.PanZY: {
        Signal.update(origin, ({x, y, z}) => ({
          x: x,
          y: y + deltaY * panRate,
          z: z - deltaX * panRate,
        }));
        break;
      }
      case Interaction.PanXZ: {
        Signal.update(origin, ({x, y, z}) => ({
          x: x - deltaX * panRate,
          y: y,
          z: z - deltaY * panRate,
        }));
        break;
      }
      case Interaction.ResizeSplit: {
        const deltaX = (canvasPointerAt[0] - pointerWasAt[0]) / canvas.clientWidth;
        const deltaY = (canvasPointerAt[1] - pointerWasAt[1]) / canvas.clientHeight;
        Signal.update(quadSplitPoint, ({x, y}) => ({
          x: x + deltaX,
          y: y + deltaY,
        }));
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
    '--canvas-height': `${Signal.get(canvasSize).height}px`
  }}>
    <div class="canvas-container" ref={canvasContainer!}>
      <RenderToolbar renderType={renderType} quadView={quadView} rotation={rotation} origin={origin} zoom={zoom} />
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
