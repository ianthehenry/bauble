import type {Component, JSX} from 'solid-js';
import {batch, createEffect, createMemo, createSelector, onMount, For, Switch, Match, Show} from 'solid-js';
import {Timer, LoopMode, TimerState} from './timer';
import installCodeMirror from './editor';
import {EditorView} from '@codemirror/view';
import * as Signal from './signals';
import {mod, clamp} from './util';
import {vec2, vec3} from 'gl-matrix';
import {Seconds, RenderType} from './types';
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
// where 45 is the free camera FOV
const cameraPanSpeed = 2 * Math.tan(Math.PI * 45 / 180 / 2);

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

type EditorToolbarProps = {
  state: EvaluationState,
  canExport: boolean,
  onExportEmbed: () => void,
};

enum CopyStatus {
  Default,
  Done,
  Error,
}

const ExportEmbedDialog = (props: {
  onClose: () => void,
  onBackgroundClick: () => void,
  isVisible: boolean,
  getScript: () => string,
  wasmBox: Mailbox,
}) => {
  let dialog: HTMLDialogElement;
  let outputContainer: HTMLTextAreaElement;
  const onClick = (e: MouseEvent) => {
    // clicking on the background of the page vs. clicking
    // on the div that fills up the whole dialog element
    if (e.target === dialog) {
      props.onBackgroundClick();
    }
  };

  const set = (text: string) => {
    outputContainer.value = text;
  };

  const regenOutput = async () => {
    const script = props.getScript();
    const request = {tag: 'compile', script, renderType: RenderType.Normal, crosshairs: false};
    // TODO: do something with result.outputs?
    const result: any = await props.wasmBox.send(request);
    if (result.isError) {
      set("error");
    } else {
      const options: any = {source: result.shaderSource};
      if (result.dimension !== 3) {
        options.dimension = result.dimension;
      }
      if (result.isAnimated) {
        options.animate = true;
      }
      if (!result.hasCustomCamera) {
        options.freeCamera = true;
      }
      if (result.uniforms.length > 0) {
        const uniforms: any = {};
        for (let {name, type} of result.uniforms) {
          uniforms[name] = type;
        }
        options.uniforms = uniforms;
      }

      const toJSNotation = (obj: any, indent: number) =>
        JSON.stringify(obj, null, indent)
        .replaceAll(/"([^"]+)":/g, (_, key) => key + ':')
        .replaceAll(/: \[([^\]]+)\]/gm, (_, innards) => ': [' + innards.trim().replaceAll(/\s+/gm, ' ') + ']');

      let str = `const bauble = new Bauble(canvas, ${toJSNotation(options, 2)});`;
      if (result.uniforms.length > 0) {
        const uniforms: any = {};
        for (let {name, type, value} of result.uniforms) {
          uniforms[name] = value[type];
        }
        str += `\nbauble.set(${toJSNotation(uniforms, 2)});`
      }
      set(str);
    }
  }

  const copyStatus = Signal.create(CopyStatus.Default);
  createEffect(() => {
    if (props.isVisible) {
      Signal.set(copyStatus, CopyStatus.Default);
      set("compiling...");
      dialog.showModal();
      regenOutput();
    } else {
      dialog.close();
    }
  })
  const copyIcon = createMemo(() => {
    switch (Signal.get(copyStatus)) {
    case CopyStatus.Default: return 'clipboard';
    case CopyStatus.Done: return 'clipboard-check';
    case CopyStatus.Error: return 'clipboard-x';
    }
  });
  const copy = async () => {
    Signal.set(copyStatus, CopyStatus.Default);
    try {
      await navigator.clipboard.writeText(outputContainer.value);
      Signal.set(copyStatus, CopyStatus.Done);
    } catch (err) {
      Signal.set(copyStatus, CopyStatus.Error);
      console.error("copy failed:", err);
    }
  };
  return <dialog class="export-embed" ref={dialog!} onClick={onClick} onClose={props.onClose}>
    <div>
      <p>Put this JavaScript in your thing:<button onClick={copy}><Icon name={copyIcon()} />Copy</button></p>
      <textarea rows="10" ref={outputContainer!}></textarea>
      <p><a href="/help#embauble" target="_blank">You'll want a copy of the Bauble player as well.</a></p>
    </div>
  </dialog>;
};

const EditorToolbar: Component<EditorToolbarProps> = (props) => {
  let exportPopover: HTMLDivElement;
  let exportButton: HTMLButtonElement;
  return <div class="toolbar">
    <div class="spacer"></div>
    <Show when={props.canExport}>
      <button title="Export" ref={exportButton!} onClick={(e) => {
        const buttonPosition = exportButton.getBoundingClientRect();
        exportPopover.style.top = buttonPosition.bottom.toString() + "px";
        exportPopover.style.left = buttonPosition.left.toString() + "px";
      }}>
        <Icon name="box-arrow-up" />
      </button>
      <div popover ref={(el) => {
        exportPopover = el;
        exportButton!.popoverTargetElement = el
      }}>
      <ul>
      <li>Export Image [CLI only]</li>
      <li>Export Video [todo]</li>
      <li>Export 3D model [CLI only]</li>
      <li>Export to Shadertoy [todo]</li>
      <li><button onClick={() => { props.onExportEmbed(); }}>Export to HTML Embed</button></li>
      </ul>
      <p>Sign up for a Bauble Pro account to unlock these features!</p>
      <p>No I'm kidding look, this is theoretically the export menu, but I haven't
      actually implemented an export UI yet, so you kinda have to like
      record your screen or use the extremely alpha-quality Bauble CLI if
      you want to share your creations.</p>
      </div>
    </Show>
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

const resetFreeCamera = (
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
  renderType: Signal.T<RenderType>,
  usingFreeCamera: Signal.Accessor<boolean>,
  prefersFreeCamera: Signal.T<boolean>,
  quadView: Signal.T<boolean>,
  rotation: Signal.T<vec2>,
  origin: Signal.T<vec3>,
  zoom: Signal.T<number>,
  origin2D: Signal.T<vec2>,
}

const RenderToolbar: Component<RenderToolbarProps> = (props) => {
  const toggleQuadView = () => {
    Signal.update(props.quadView, (x) => !x);
  };
  const toggleFreeCamera = () => {
    Signal.update(props.prefersFreeCamera, (x) => !x);
  };

  return <div class="toolbar">
    <button title="Toggle free camera" onClick={toggleFreeCamera}>
      <Icon name={props.usingFreeCamera() ? "camera-reels" : "camera-reels-fill"} />
    </button>
    <button title="Reset camera [alt-r]" onClick={() => batch(() => {
      Signal.set(props.prefersFreeCamera, true);
      resetFreeCamera(props.rotation, props.origin, props.zoom);
      Signal.set(props.origin2D, vec2.fromValues(0, 0));
      })}>
      <Icon name="box" />
    </button>
    <button title="Toggle quad view [alt-q]" onClick={toggleQuadView}>
      <Icon name={Signal.get(props.quadView) ? "grid-fill" : "grid"} />
    </button>
    <div class="spacer"></div>
    {choices(props.renderType, [
      { value: RenderType.Normal, icon: "camera", title: "Render normally" },
      { value: RenderType.Surfaceless, icon: "circle", title: "Use default surface" },
      { value: RenderType.Convergence, icon: "magnet", title: "Debug number of raymarching steps" },
      { value: RenderType.Distance, icon: "arrows-collapse", title: "Debug surface distance" },
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
  TranslateXY,
  TranslateZY,
  TranslateXZ,
  ResizeSplit,
  Pan2D,
}

interface BaubleProps {
  initialScript: string,
  focusable: boolean,
  canSave: boolean,
  canSearch: boolean,
  canExport: boolean,
  showLineGutter: boolean,
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
  let editorView: EditorView;
  let outputContainer: HTMLElement;

  let isGesturing = false;
  let gestureEndedAt = 0;

  let crosshairs3D: Signal.Accessor<vec3 | null>;

  const canvasSize = Signal.create(props.size);
  const pixelRatio = Signal.create(window.devicePixelRatio);
  const imageRendering: Signal.T<Property.ImageRendering> = Signal.create('auto' as Property.ImageRendering);
  const canvasResolution = createMemo(() => {
    const dpr = Signal.get(pixelRatio);
    const size = Signal.get(canvasSize);
    return {width: dpr * size.width, height: dpr * size.height};
  });
  const renderType = Signal.create(RenderType.Normal);
  const dimension = Signal.create(0);
  const prefersFreeCamera = Signal.create(false);
  const quadView = Signal.create(false);
  const quadSplitPoint = Signal.create(vec2.fromValues(0.5, 0.5));
  const zoom = Signal.create(defaultCamera.zoom);
  const rotation = Signal.create(defaultCamera.rotation);
  const origin = Signal.create(defaultCamera.origin);
  const origin2D = Signal.create(vec2.fromValues(0, 0));
  const evaluationState = Signal.create(EvaluationState.Unknown);
  const hasCustomCamera = Signal.create(false);
  const customUniforms = Signal.create<Array<{name: string, type: string, value: any}>>([]);
  const isAnimated = Signal.create(false);
  const isVisible = Signal.create(false);
  const showExportEmbed = Signal.create(false);
  const usingFreeCamera = createMemo(() => Signal.get(prefersFreeCamera) || !Signal.get(hasCustomCamera));
  const translateOffset = Signal.create(vec3.fromValues(0, 0, 0));
  const translateOrigin = Signal.create(null as vec3 | null);

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

  const recompile = (script: string, renderType: RenderType) => {
    compileQueue.schedule(async () => {
      Signal.set(evaluationState, EvaluationState.Unknown);
      const request = {tag: 'compile', script, renderType, crosshairs: true};
      const result: any = await wasmBox.send(request);

      if (result.isError) {
        Signal.set(evaluationState, EvaluationState.EvaluationError);
        flush(result.outputs);
      } else {
        try {
          //console.log(result.shaderSource);
          const shaderRecompilationTimeMs = await renderer.recompileShader(result.shaderSource) as number | undefined;
          Signal.set(dimension, result.dimension);
          Signal.set(isAnimated, result.isAnimated);
          Signal.set(hasCustomCamera, result.hasCustomCamera);
          Signal.set(evaluationState, EvaluationState.Success);
          Signal.set(customUniforms, result.uniforms);

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

  const script = Signal.create<string | null>(null);
  onMount(() => {
    intersectionObserver.observe(canvas);

    const compilationArgs = createEffect(() => {
      const script_ = Signal.get(script);
      const renderType_ = Signal.get(renderType);
      if (script_ != null) {
        recompile(script_, renderType_);
      }
    })

    const {view, focusVec, setFocusVec} = installCodeMirror({
      initialScript: props.initialScript,
      parent: editorContainer,
      canSave: props.canSave,
      canSearch: props.canSearch,
      showLineGutter: props.showLineGutter,
      onChange: () => { Signal.set(script, editorView.state.doc.toString()) },
      definitions: definitions,
    });
    crosshairs3D = focusVec;
    createEffect(() => {
      const result = vec3.create();
      vec3.add(result, Signal.get(translateOrigin) ?? vec3.fromValues(0, 0, 0), Signal.get(translateOffset));
      setFocusVec(result);
    });
    editorView = view;
    const crosshairs = createMemo(() => {
      const focusVec_ = focusVec();
      return Signal.get(quadView) ? focusVec_ : null;
    });
    renderer = new AsyncRenderer(renderBox, canvas, {
      time: Signal.getter(timer.t),
      isVisible: Signal.getter(isVisible),
      rotation: Signal.getter(rotation),
      origin: Signal.getter(origin),
      origin2D: Signal.getter(origin2D),
      zoom: Signal.getter(zoom),
      prefersFreeCamera: Signal.getter(prefersFreeCamera),
      quadView: Signal.getter(quadView),
      quadSplitPoint: Signal.getter(quadSplitPoint),
      resolution: canvasResolution,
      crosshairs: crosshairs,
      customUniforms: Signal.getter(customUniforms),
    });

    const modalVisible = createMemo(() => Signal.get(showExportEmbed));

    timeAdvancer = new RenderLoop((elapsed) => batch(() => {
     const isAnimated_ = Signal.get(isAnimated);
     const isTimeAdvancing = isAnimated_ && Signal.get(timer.state) !== TimerState.Paused && !modalVisible();
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
      modalVisible,
      isAnimated,
    ], () => {
      timeAdvancer.schedule();
    });
    requestAnimationFrame(() => {
      Signal.set(script, editorView.state.doc.toString());
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

  const getCursorStyle = (e: PointerEvent) => {
    if (interaction == null) {
      if (interactionPointer != null || getInteraction(e) == null) {
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

  const getMainViewInteraction = () => {
    if (Signal.get(dimension) === 2) {
      return Interaction.Pan2D;
    }
    if (usingFreeCamera()) {
      return Interaction.Rotate;
    } else {
      return null;
    }
  }

  const controlled = <T,>(e: MouseEvent, a: T, b: T): T => (e.ctrlKey || e.metaKey) ? a : b;

  const getInteraction = (e: MouseEvent) => {
    if (Signal.get(quadView)) {
      if (isOnSplitPoint(e)) {
        return Interaction.ResizeSplit;
      } else {
        const relativePoint = getRelativePoint(e);
        const splitPoint = Signal.get(quadSplitPoint);
        if (relativePoint[1] < splitPoint[1]) {
          if (relativePoint[0] < splitPoint[0]) {
            return getMainViewInteraction();
          } else {
            return controlled(e, Interaction.TranslateXZ, Interaction.PanXZ);
          }
        } else {
          if (relativePoint[0] < splitPoint[0]) {
            return controlled(e, Interaction.TranslateXY, Interaction.PanXY);
          } else {
            return controlled(e, Interaction.TranslateZY, Interaction.PanZY);
          }
        }
      }
    } else {
      return getMainViewInteraction();
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
    batch(() => {
      Signal.set(translateOffset, vec3.fromValues(0, 0, 0));
      Signal.set(translateOrigin, crosshairs3D());
    });
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
    switch (getInteraction(e)) {
    case Interaction.Rotate: {
      if (Signal.get(quadView)) {
        batch(() => {
          Signal.set(rotation, defaultCamera.rotation);
          Signal.set(zoom, defaultCamera.zoom);
        });
      } else {
        resetFreeCamera(rotation, origin, zoom);
      }
      break;
    }
    case Interaction.PanXY: Signal.update(origin, (old) => vec3.fromValues(defaultCamera.origin[0], defaultCamera.origin[1], old[2])); break;
    case Interaction.PanZY: Signal.update(origin, (old) => vec3.fromValues(old[0], defaultCamera.origin[1], defaultCamera.origin[2])); break;
    case Interaction.PanXZ: Signal.update(origin, (old) => vec3.fromValues(defaultCamera.origin[0], old[1], defaultCamera.origin[2])); break;
    case Interaction.Pan2D: Signal.set(origin2D, vec2.fromValues(0, 0)); break;
    case Interaction.ResizeSplit: Signal.set(quadSplitPoint, vec2.fromValues(0.5, 0.5)); break;
    case Interaction.TranslateXY: break;
    case Interaction.TranslateZY: break;
    case Interaction.TranslateXZ: break;
    case null: Signal.set(prefersFreeCamera, true); break;
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

    // TODO: panRate should take the size of the viewport you're panning into account,
    // instead of assuming a half quad
    const panRate = Signal.get(zoom) * cameraPanSpeed * (Signal.get(quadView) ? 2 : 1);

    switch (interaction!) {
    case Interaction.Rotate: {
      Signal.update(rotation, (old) =>
        vec2.fromValues(
          mod(old[0] - deltaX * cameraRotateSpeed, 1.0),
          mod(old[1] - deltaY * cameraRotateSpeed, 1.0)));
      break;
    }
    case Interaction.Pan2D: {
      Signal.update(origin2D, (old) =>
        vec2.fromValues(old[0] - deltaX * panRate, old[1] + deltaY * panRate));
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
    case Interaction.TranslateXY: {
      Signal.update(translateOffset, (old) =>
        vec3.fromValues(old[0] + deltaX * panRate, old[1] - deltaY * panRate, old[2]));
      break;
    }
    case Interaction.TranslateZY: {
      Signal.update(translateOffset, (old) =>
        vec3.fromValues(old[0], old[1] - deltaY * panRate, old[2] + deltaX * panRate));
      break;
    }
    case Interaction.TranslateXZ: {
      Signal.update(translateOffset, (old) =>
        vec3.fromValues(old[0] + deltaX * panRate, old[1], old[2] - deltaY * panRate));
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

  // keyboard shortuct notes:
  // - e.key tells you the key that will be typed, so e.g. alt-r gives ®, which is useless
  // - e.code tells you the physical key that was pressed, so in the face of software key
  //   re-mapping it does not work. There is an experimental keyboard layout API that is not
  //   widely supported which you can use to map e.code to the actual key pressed, but no.
  // - e.which and e.keyCode seem to be the same; they're both deprecated but are the only
  //   way to handle keyboard shortucts correctly as far as I can figure out
  const KeyQ = 81;
  const KeyR = 82;
  const onKeyDown = (e: KeyboardEvent) => {
    if (e.altKey) {
      let handled = true;
      switch (e.which) {
      case KeyQ: Signal.update(quadView, (quadView) => !quadView); break;
      case KeyR:
        batch(() => {
            Signal.set(prefersFreeCamera, true);
            resetFreeCamera(rotation, origin, zoom);
            Signal.set(origin2D, vec2.fromValues(0, 0));
        }); break;
      default: handled = false;
      }
      if (handled) {
        e.preventDefault();
      }
    }
  };

  return <div class="bauble" style={{
    '--canvas-width': `${Signal.get(canvasSize).width}px`,
    '--canvas-height': `${Signal.get(canvasSize).height}px`,
  }} onKeyDown={onKeyDown}>
    <div class="canvas-container" ref={canvasContainer!}>
      <RenderToolbar
      renderType={renderType}
      usingFreeCamera={usingFreeCamera}
      prefersFreeCamera={prefersFreeCamera}
      quadView={quadView}
      rotation={rotation}
      origin={origin}
      origin2D={origin2D}
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
      <EditorToolbar
        state={Signal.get(evaluationState)}
        canExport={props.canExport}
        onExportEmbed={() => Signal.set(showExportEmbed, true)}
      />
      <div class="editor-container" ref={editorContainer!} />
      <ResizableArea ref={outputContainer!} />
    </div>
    <ExportEmbedDialog
      isVisible={Signal.get(showExportEmbed)}
      onClose={() => {Signal.set(showExportEmbed, false); editorView.focus()}}
      onBackgroundClick={() => Signal.set(showExportEmbed, false)}
      getScript={() => Signal.get(script) || ""}
      wasmBox={wasmBox}
      />
  </div>;
};
export default Bauble;
