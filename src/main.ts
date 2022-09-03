import {basicSetup} from 'codemirror';
import {EditorView, keymap, ViewUpdate} from '@codemirror/view';
import {indentWithTab} from '@codemirror/commands';
import {syntaxTree} from '@codemirror/language';
import {SyntaxNode} from '@lezer/common';
import {janet} from 'codemirror-lang-janet';
import {
  EditorState, EditorSelection, Transaction,
} from '@codemirror/state';
import Big from 'big.js';

const TAU = 2 * Math.PI;
const cameraRotateSpeed = 1 / 512;
const cameraZoomSpeed = 0.01;
const LOCAL_STORAGE_KEY = "script";

function clearOutput() {
  const output = document.getElementById('output')!;
  output.innerHTML = "";
}

function print(text: string, isErr=false) {
  if (isErr) {
    console.error(text);
  } else {
    console.log(text);
  }
  const output = document.getElementById('output')!;
  const span = document.createElement('span');
  span.classList.toggle('err', isErr);
  span.appendChild(document.createTextNode(text));
  span.appendChild(document.createTextNode('\n'));
  output.appendChild(span);
  output.scrollTop = output.scrollHeight;
}

interface Camera { x: number, y: number, zoom: number; }
let evaluateScript: ((_code: string) => number) | null = null;
let updateCamera: ((_cameraX: number, _cameraY: number, _cameraZoom: number) => void) | null = null;
let updateTime: ((_t: number) => void) | null = null;
let updateViewType: ((_t: number) => void) | null = null;
let rerender: (() => void) | null = null;
let ready: (() => void) | null = function() { ready = null; };

function onReady(f: (() => void)) {
  if (ready == null) {
    f();
  } else {
    const old = ready;
    ready = function() {
      old();
      f();
    };
  }
}

const preamble = '(use ./helpers) (use ./dsl) (use ./pipe-syntax) (use ./dot-syntax) (use ./globals) (use ./glslisp/src/builtins) (resolve-dots (pipe \n';
const postamble = '\n))'; // newline is necessary in case the script ends in a comment

function recompileScript(script: string) {
  return evaluateScript(preamble + script + postamble);
}

interface MyEmscripten extends EmscriptenModule {
  cwrap: typeof cwrap;
  ccall: typeof ccall;
}

declare global {
  interface Window { Module: Partial<MyEmscripten>; }
}

let resolveInitialScript = null;
const initialScript = new Promise((x) => { resolveInitialScript = x; });

const Module: Partial<MyEmscripten> = {
  preRun: [],
  print: function(x: string) {
    print(x, false);
  },
  printErr: function(x: string) {
    print(x, true);
  },
  postRun: [function() {
    evaluateScript = Module.cwrap!("evaluate_script", 'number', ['string']);
    updateCamera = Module.cwrap!("update_camera", null, ['number', 'number', 'number']);
    updateTime = Module.cwrap!("update_time", null, ['number']);
    updateViewType = Module.cwrap!("update_view_type", null, ['number']);
    rerender = Module.cwrap!("rerender", null, []);
    Module.ccall!("initialize_janet", null, [], []);
    ready();
    resolveInitialScript(FS.readFile('intro.janet', {encoding: 'utf8'}));
  }],
  locateFile: function(path, prefix) {
    if (prefix === '') {
      return '/js/' + path;
    } else {
      return prefix + path;
    }
  },
};

function isNumberNode(node: SyntaxNode) {
  return node.type.name === 'Number';
}

interface StateCommandInput {state: EditorState, dispatch: (_: Transaction) => void}

function alterNumber({state, dispatch}: StateCommandInput, amount: Big) {
  const range = state.selection.ranges[state.selection.mainIndex];
  const tree = syntaxTree(state);

  let node = tree.resolveInner(range.head, -1);
  if (!isNumberNode(node)) {
    node = tree.resolveInner(range.head, 1);
  }
  if (!isNumberNode(node)) {
    return false;
  }

  // TODO: we shouldn't be doing any floating point math; we should
  // parse this as a decimal number and increment it as a decimal number
  const numberText = state.sliceDoc(node.from, node.to);
  let number;
  try {
    number = Big(numberText);
  } catch (e) {
    console.error('unable to parse number: ', numberText);
    return false;
  }
  const decimalPointIndex = numberText.indexOf('.');
  const digitsAfterDecimalPoint = decimalPointIndex < 0 ? 0 : numberText.length - decimalPointIndex - 1;
  const increment = Big('10').pow(-digitsAfterDecimalPoint);

  const newNumber = number.add(amount.times(increment));
  const newNumberText = newNumber.toFixed(digitsAfterDecimalPoint);

  const lengthDifference = newNumberText.length - numberText.length;

  dispatch(state.update({
    changes: {
      from: node.from,
      to: node.to,
      insert: newNumberText,
    },
    selection: EditorSelection.single(node.from, node.to + lengthDifference),
    scrollIntoView: true,
    userEvent: 'increment',
  }));
  return true;
}

function clamp(value: number, min: number, max: number) {
  return Math.max(Math.min(value, max), min);
}

function save({state}: StateCommandInput) {
  const script = state.doc.toString();
  if (script.trim().length > 0) {
    localStorage.setItem(LOCAL_STORAGE_KEY, script);
  } else {
    localStorage.removeItem(LOCAL_STORAGE_KEY);
  }
  return true;
}

function mod(a: number, b: number) {
  return ((a % b) + b) % b;
}

interface GestureEvent extends TouchEvent {
  scale: number
}

enum CompilationResult {
  NoChange = 0,
  ChangedStatic = 1,
  ChangedAnimated = 2,
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

  tick(now, isAnimation) {
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

  setLoopMode(loopMode) {
    if (loopMode != LoopMode.Reverse) {
      this.rate = 1;
    }
    this.loopMode = loopMode;
    if (loopMode != LoopMode.NoLoop) {
      this.t = clamp(this.t, this.loopStart, this.loopEnd);
    }
  }
}

function initialize(script) {
  const camera = {
    x: -0.125,
    y: 0.125,
    zoom: 2.0,
  };

  let viewType = 0;

  const timestampInput: HTMLInputElement = document.querySelector('input[name=timestamp]')!;

  const timer = new Timer();
  let drawScheduled = false;
  let recompileScheduled = false;
  let isAnimation = false;
  function draw(recompile) {
    recompileScheduled ||= recompile;
    drawScheduled = true;
  }
  function tick(now) {
    requestAnimationFrame(tick);
    timer.tick(now, isAnimation);
    if (timer.state === TimerState.Playing) {
      playButton.classList.add('hidden');
      pauseButton.classList.remove('hidden');
    } else {
      playButton.classList.remove('hidden');
      pauseButton.classList.add('hidden');
    }
    timestampInput.value = timer.t.toFixed(2);
    if (drawScheduled) {
      clearOutput();
      updateCamera(TAU * camera.x, TAU * camera.y, camera.zoom);
      updateTime(timer.t);
      if (recompileScheduled) {
        let result = recompileScript(editor.state.doc.toString());
        if (result >= 0) {
          // TODO: surely this isn't actually how you do this
          switch (CompilationResult[CompilationResult[result]]) {
            case CompilationResult.NoChange: break;
            case CompilationResult.ChangedAnimated: isAnimation = true; break;
            case CompilationResult.ChangedStatic: isAnimation = false; break;
          }
        }
      }
      rerender();
    }
    drawScheduled = timer.state === TimerState.Playing;
    recompileScheduled = false;
  }
  requestAnimationFrame(tick);

  const incrementNumber = (editor: StateCommandInput) => alterNumber(editor, Big('1'));
  const decrementNumber = (editor: StateCommandInput) => alterNumber(editor, Big('-1'));

  const editorContainer = document.getElementById('editor-container')!;

  const editor = new EditorView({
    extensions: [
      basicSetup,
      janet(),
      keymap.of([
        indentWithTab,
        { key: "Alt-h", run: incrementNumber, shift: decrementNumber },
        { key: "Mod-s", run: save },
      ]),
      EditorView.updateListener.of(function(viewUpdate: ViewUpdate) {
        if (viewUpdate.docChanged) {
          draw(true);
        }
      }),
      EditorView.theme({
        ".cm-content": {
          fontFamily: "Menlo, monospace",
          fontSize: "13px",
        },
      }),
    ],
    parent: editorContainer,
    doc: script,
  });

  // honestly this is so annoying on firefox that
  // i'm not even gonna bother
  const usePointerLock = false;
  if (usePointerLock) {
    document.addEventListener('keydown', (e) => {
      if (e.key === 'Control') {
        document.body.requestPointerLock();
      }
    });
    document.addEventListener('keyup', (e) => {
      if (e.key === 'Control') {
        document.exitPointerLock();
      }
    });
  }

  const playButton: HTMLButtonElement = document.querySelector('#canvas-container button[data-action=play]')!;
  const pauseButton: HTMLButtonElement = document.querySelector('#canvas-container button[data-action=pause]')!;
  const stopButton: HTMLButtonElement = document.querySelector('#canvas-container button[data-action=stop]')!;
  const resetButton: HTMLButtonElement = document.querySelector('#canvas-container button[data-action=reset]')!;
  resetButton.addEventListener('click', (e) => {
    console.log("reset");
  });
  playButton.addEventListener('click', (e) => {
    timer.state = TimerState.Playing;
  });
  pauseButton.addEventListener('click', (e) => {
    timer.state = TimerState.Paused;
  });
  stopButton.addEventListener('click', (e) => {
    timer.stop();
  });

  document.getElementById('canvas-container')!.addEventListener('input', (e) => {
    const target = <HTMLInputElement>e.target;
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

  const canvas = document.getElementById('render-target')! as HTMLCanvasElement;
  let canvasPointerAt = [0, 0];
  let rotatePointerId = null;
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
  canvas.addEventListener('gesturestart', (_e: GestureEvent) => {
    initialZoom = camera.zoom;
    isGesturing = true;
  });
  canvas.addEventListener('gestureend', (_e: GestureEvent) => {
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

  const outputContainer = document.getElementById('output')!;
  const outputResizeHandle = document.getElementById('output-resize-handle')!;
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

  editorContainer.addEventListener('pointerdown', (e) => {
    if ((e.buttons === 1 || e.buttons === 2) && e.ctrlKey) {
      ctrlClickedAt = performance.now();
      editorContainer.setPointerCapture(e.pointerId);
      e.preventDefault();
    }
  });
  editorContainer.addEventListener('contextmenu', (e) => {
    if (isTryingToEngageNumberDrag()) {
      e.preventDefault();
    }
  });
  editorContainer.addEventListener('pointermove', (e) => {
    if (editorContainer.hasPointerCapture(e.pointerId)) {
      alterNumber(editor, Big(e.movementX).times('1'));
    }
  });

  // There is a bug in Firefox where ctrl-click fires as
  // a pointermove event instead of a pointerdown event,
  // and then will not respect setPointerCapture() when
  // called from the pointermove event.
  //
  // https://bugzilla.mozilla.org/show_bug.cgi?id=1504210
  //
  // So on Firefox you have to use an actual right-click.
  // It's very annoying. This is an *okay* workaround.
  document.addEventListener('pointermove', (e) => {
    if (e.shiftKey && e.metaKey) {
      alterNumber(editor, Big(e.movementX).times('1'));
    }
  });

  document.addEventListener('pagehide', (_e) => {
    save(editor);
  });
  let savedBefore = false;
  // iOS Safari doesn't support beforeunload,
  // but it does support unload.
  window.addEventListener('beforeunload', (_e) => {
    savedBefore = true;
    save(editor);
  });
  window.addEventListener('unload', (_e) => {
    if (!savedBefore) {
      save(editor);
    }
  });

  onReady(() => draw(true));
  editor.focus();
}

document.addEventListener("DOMContentLoaded", (_) => {
  const saved = localStorage.getItem(LOCAL_STORAGE_KEY);
  if (saved == null) {
    initialScript.then(initialize);
  } else {
    initialize(saved);
  }
});

window.Module = Module;
