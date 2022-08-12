import {basicSetup} from 'codemirror';
import {EditorView, keymap, ViewUpdate} from '@codemirror/view';
import {indentWithTab, cursorDocEnd} from '@codemirror/commands';
import {syntaxTree} from '@codemirror/language';
import {SyntaxNode} from '@lezer/common';
import {janet} from 'codemirror-lang-janet';
import {
  EditorState, EditorSelection, Transaction,
} from '@codemirror/state';
import Big from 'big.js';

const TAU = 2 * Math.PI;
const cameraRotateSpeed = 0.001;
const cameraZoomSpeed = 0.01;
const LOCAL_STORAGE_KEY = "script";

function clear() {
  const output = document.getElementById('output')!;
  output.innerHTML = "";
}

function print(text: string, isErr=false) {
  const output = document.getElementById('output')!;
  const span = document.createElement('span');
  span.classList.toggle('err', isErr);
  span.appendChild(document.createTextNode(text));
  span.appendChild(document.createTextNode('\n'));
  output.appendChild(span);
  output.scrollTop = output.scrollHeight;
}

interface Camera { x: number, y: number, zoom: number; }
let evaluateJanet: ((_code: string, _cameraX: number, _cameraY: number, _cameraZoom: number) => number) | null = null;
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

const initialScript = `
# Hello, and welcome to an extremely
# early and unfinished demo!

# Bauble is a playground for creating
# and rendering 3D shapes using signed
# distance functions. Like this one:

(intersect
  (box 50)
  (sphere 70))

# Drag the viewport around with your
# mouse, and scroll to move the camera
# in and out.

# This text field is a Janet program
# that is re-evaluated every time you
# make a change. This program "returns"
# whatever the final expression is --
# in this case, that little rounded-ish
# box up there. Uncomment the next line
# to return something else:

# (morph 2.50 (sphere 50) (box 50))

# Janet is a fully-featured language, so
# you can define variables, functions,
# macros, loops -- anything your heart
# desires. Here's a nonsense example --
# to uncomment it, select the whole
# paragraph and press "cmd-/"
# or "ctrl-/":

# (var top 0)
# (defn hollow-box [size]
#   (smooth-subtract 5 (box size) (sphere (* 1.20 size))))
# (defn stack-box [size]
#   (let [result (translate [0 (+ top size) 0] (hollow-box size))]
#     (+= top (* 2 size))
#     result))
# (translate [0 -45 0]
#   (smooth-union 20 ;(map stack-box [40 30 20])))

# You can also edit values with your
# mouse. Uncomment the next block of
# code, and put your cursor on the
# value 0.00. Then hold down the
# control key, and move your mouse left
# to right.

# (def r 0.00)
# (-> (cone :x 40 100)
#   (rotate :y (tau r) :z (tau r))
#   (symmetry))

# When editing values with your mouse,
# Bauble will increment the smallest
# digit of the number, so you can
# increase the precision by adding
# zeroes to the end. In other words,
# editing a value like 3.0 will
# increment by 0.1, but editing 3.000
# will increment by 0.001.

# You can also apply surfaces to shapes.

# (union
#   (blinn-phong [1 0.62 0.54] 1 8 (sphere 50))
#   (blinn-phong [0.9 0.9 0.9] 0 1 (translate [0 -50 0] (half-space :-y))))

# This is not very intuitive, but the
# invocation is:
#
# (blinn-phong color shininess glossiness shape)
#
# To apply a surface. This... this will
# be better in the future.

# You can also take the color from one
# shape and apply it to another. For
# example:

(with-surface (offset 1 (box 50))
  (intersect
    (blinn-phong [1 0 0] 1 4 (box 50))
    (blinn-phong [0 0 1] 1 4 (sphere 70))))

# This is a useful technique for "painting"
# complex colors onto shapes.

# And... that's it for now! You cannot
# currently control lighting, shadow
# parameters, raymarching parameters,
# or anything else.

# There is no documentation or help or
# anything for Bauble functions right
# now, but here's a quick overview of
# the basics. Uncomment these one at a
# time to see what they do:

# (union (box 50) (sphere 70))
# (smooth-union 10 (box 50) (cone :z 40 100))
# (intersect (onion 1 (sphere 20)) (half-space :-z))
# (morph 0.5 (box 50) (sphere 50))
# (subtract (box 50) (cylinder :z 30 100))
# (smooth-subtract 30 (rotate :y (tau 0.125) :z (tau 0.125) (box 50)) (translate [50 0 0] (sphere 50)))
# (cone :x 50 200)
# (reflect :x (cone :x 50 200))
# (-> (cone :x 50 200) (rotate :y (tau 0.125)) (mirror :x :z))
# (smooth-union 5 (line [-50 0 0] [50 0 0] 10) (mirror :x (sphere 30 [50 0 0])))
# (tile [100 100 100] (sphere 50) :limit [1 3 1])
# (scale 2.0 (offset 5 (box 40)))

# What else? You can print values for
# debugging with (print "string") or
# (pp expression). Error messages are
# extremely bad right now, so don't
# make any mistakes. If you write an
# infinite loop it *will* just hang the
# browser tab and you will have no way
# to get out of it except to refresh
# the page.

# Umm that's all okay good luck! Your
# changes will automatically save, but
# if you want to restore this initial
# tutorial, just empty out this text
# field and refresh the page.
`.trimLeft();

const preamble = '(use ./shapes)\n';

function executeJanet(code: string, camera) {
  if (evaluateJanet === null) {
    console.error('not ready yet');
    return;
  }

  const result = evaluateJanet(preamble + code, TAU * camera.x, TAU * camera.y, camera.zoom);
  if (result !== 0) {
    print('ERREXIT: ' + result.toString(), true);
  }
}

interface MyEmscripten extends EmscriptenModule {
  cwrap: typeof cwrap;
}

declare global {
  interface Window { Module: Partial<MyEmscripten>; }
}

const Module: Partial<MyEmscripten> = {
  preRun: [],
  print: function(x: string) {
    print(x, false);
  },
  printErr: function(x: string) {
    print(x, true);
  },
  postRun: [function() {
    evaluateJanet = Module.cwrap!("run_janet", 'number', ['string', 'number', 'number', 'number']);
    ready();
  }],
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

function clamp(value, min, max) {
  return Math.max(Math.min(value, max), min);
}

function save({state, dispatch}: StateCommandInput) {
  const script = state.doc.toString();
  if (script.trim().length > 0) {
    localStorage.setItem(LOCAL_STORAGE_KEY, script);
  } else {
    localStorage.removeItem(LOCAL_STORAGE_KEY);
  }
  return true;
}

function mod(a, b) {
  return ((a % b) + b) % b;
}

document.addEventListener("DOMContentLoaded", (_) => {
  const camera = {
    x: -0.125,
    y: 0.125,
    zoom: 2.0
  };
  function draw() {
    setTimeout(function() {
      clear();
      executeJanet(editor.state.doc.toString(), camera);
    }, 0);
  }

  const incrementNumber = (editor: StateCommandInput) => alterNumber(editor, Big('1'));
  const decrementNumber = (editor: StateCommandInput) => alterNumber(editor, Big('-1'));

  const script = localStorage.getItem(LOCAL_STORAGE_KEY) ?? initialScript;

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
          draw();
        }
      }),
    ],
    parent: document.getElementById('editor-container')!,
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

  const canvas = document.getElementById('render-target')!;
  canvas.addEventListener('pointerdown', (e) => {
    e.preventDefault();
    canvas.setPointerCapture(e.pointerId);
  });
  
  canvas.addEventListener('pointermove', (e) => {
    if (canvas.hasPointerCapture(e.pointerId)) {
      e.preventDefault();
      camera.x = mod(camera.x - cameraRotateSpeed * e.movementY, 1.0);
      camera.y = mod(camera.y - cameraRotateSpeed * e.movementX, 1.0);
      draw();
    }
  });
  canvas.addEventListener('wheel', (e) => {
    e.preventDefault();
    camera.zoom += cameraZoomSpeed * e.deltaY;
    // this seems to make this much more smooth?
    // requestAnimationFrame(draw);
    draw();
  });

  const outputContainer = document.getElementById('output')!;
  const outputResizeHandle = document.getElementById('output-resize-handle')!;
  outputResizeHandle.addEventListener('pointerdown', (e) => {
    outputResizeHandle.setPointerCapture(e.pointerId);
  });
  outputResizeHandle.addEventListener('pointermove', (e) => {
    if (outputResizeHandle.hasPointerCapture(e.pointerId)) {
      const outputStyle = getComputedStyle(outputContainer);
      const verticalPadding = parseFloat(outputStyle.paddingTop) + parseFloat(outputStyle.paddingBottom);
      const oldHeight = outputContainer.offsetHeight - verticalPadding;
      const oldScrollTop = outputContainer.scrollTop;
      outputContainer.style.height = `${oldHeight - e.movementY}px`;
      outputContainer.scrollTop = clamp(oldScrollTop + e.movementY, 0, outputContainer.scrollHeight - outputContainer.offsetHeight);
    }
  });

  document.body.addEventListener('pointermove', (e) => {
    if (e.ctrlKey) {
      alterNumber(editor, Big(e.movementX).times('1'));
    }
  });

  window.addEventListener('beforeunload', (_e) => {
    save(editor);
  });

  onReady(draw);
  editor.focus();
  // cursorDocEnd(editor);
});

window.Module = Module;
