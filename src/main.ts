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

(spoon :r 15
  (torus :z 50 25)
  (move :y 50 | rotate :y tau/4)
| fresnel 1)

# Drag the viewport around with your
# mouse, and scroll to move the camera
# in and out.

# This text field is a Janet program
# that is re-evaluated every time you
# make a change. This program "returns"
# whatever the final expression is --
# in this case, those interlocking
# donuts up there. Uncomment the next
# line to return something else:

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
#   (subtract :r 5
#     (box size :r 2)
#     (sphere (* 1.20 size))))
# (defn stack-box [size]
#   (let [result (move :y (+ top size)
#                  (hollow-box size))]
#     (+= top (* 2 size))
#     result))
# (move :y -45
#   (union :r 10
#     ;(map stack-box [40 30 20])))

# You can also edit values with your
# mouse. Uncomment the next block of
# code, then put your cursor on the
# value 0.00. Then hold down the
# control key, and move your mouse left
# to right.

# (def r 0.00)
# (-> (cone :x 60 80 :r 1)
#   (rotate-tau :y r :z r)
#   (symmetry))

# When editing values with your mouse,
# Bauble will increment the smallest
# digit of the number, so you can
# increase the precision by adding
# zeroes to the end. In other words,
# editing a value like 3.0 will
# increment by 0.1, but editing 3.000
# will increment by 0.001.

#### Surfacing ####

# So far everything has been weird
# shades of pastel, which is a mapping
# of the XYZ normal vector into RGB
# space. That's the default surface for
# new shapes, but you can apply other
# surfaces:

# (union
#   (color (box 50 :r 10)
#     (hsv 0.00 1 1)
#     :gloss 4
#     :shine 0.5
#     :ambient 0.2)
#   (color (half-space :-y -50)
#     [0.9 0.9 0.9]))

# (color) is an alias for (blinn-phong),
# a simple material shader. Try tweaking
# the parameters to see how they work,
# and remember that you can use your
# mouse to edit numbers! Also note that
# specular highlights depend on the
# viewing angle, so rotate the viewport
# a little too.

# When you combine shapes together, you
# also combine their surfaces. For
# example, here are a couple shapes:

# (def green-box (color [0 1 0] (box 50 :r 5) :gloss 12 :shine 1))
# (def red-sphere (color [1 0 0] (sphere 60)))

# Now uncomment each of these one at a
# time to see how the colors interact:

# (union green-box red-sphere)
# (intersect green-box red-sphere)
# (subtract green-box red-sphere)

# And now let's try it with smooth
# transitions:

# (union :r 5 green-box red-sphere)
# (intersect :r 5 green-box red-sphere)
# (subtract :r 5 green-box red-sphere)

# That's interesting, but sometimes you
# might not want to see that yellow
# bleeding through. Sometimes you want
# a smooth shape transition, but a sharp
# color transition. And you can have it:

# (resurface
#   (subtract :r 5 green-box red-sphere)
#   (subtract green-box red-sphere))

# (resurface) works to transplant the
# color field from any shape to
# another shape. In that case the shapes
# were very similar, but they don't have
# to be.

# (resurface
#   green-box
#   (union green-box red-sphere))

# The way this works is that the
# raymarcher uses the signed distance
# field from the first shape to
# determine the geometry, but when it
# hits the surface it uses the second
# shape to determine the color.

# This is a useful technique for
# "painting" complex colors onto shapes,
# but you can also use (resurface) to
# save a material to apply to multiple
# shapes. Instead of this:

# (color [1 1 0] (sphere 50))

# You can write:

# (def yellow (color [1 1 0]))
# (resurface (sphere 50) yellow)

# The way this works is that (color) and
# other material primitives, when not
# given a shape to act on, default to
# the entirety of ℝ³ -- the shape that
# is a distance 0 away from every point.
# So a "material" is still a pair of
# distance and color functions, but the
# distance function isn't really useful.

# Last thing: Bauble also has functions
# to modify the underlying color field
# in some way. Actually, just one at the
# moment:

# (fresnel red-sphere [1 1 0] 0.5 :exponent 5)

# That adds a little bit of (simulated)
# fresnel reflectivity to a surface.
# Move the camera around a bit to see
# what it does. Note that Bauble doesn't
# actually support reflection yet, so it
# just tints the edges, but it still
# looks pretty nice.

# All of the arguments are optional,
# so you can quickly apply it to a shape
# and add a little depth. Note that it
# works even with the default
# normal-coloring:

# (sphere 50)
# (fresnel (sphere 50))

#### Lisp heresy ####

# So far our examples have mostly stuck
# to "vanilla" Janet, which, of course,
# has a lot of parentheses. But Bauble
# provides a helpful macro that you can
# use to invoke functions with a little
# less typing. Let's take a look,
# starting without any helpers:

# (color [1 0 0] (rotate :y pi/4 (box 50)))

# First of all, the Bauble DSL is very
# forgiving about named and positional
# argument order. So that's actually the
# same as:

# (color (rotate (box 50) :y pi/4) [1 0 0])

# Janet provides a useful threading
# macro that we can use to write this
# as a subject and then a series of
# transformations, so that the
# expression is not as nested:

# (-> (box 50) (rotate :y pi/4) (color [1 0 0]))

# Which is very useful. Bauble lets you
# go a little bit further:

# (box 50 | rotate :y pi/4 | color [1 0 0])

# At first this might not look like Lisp
# at all, but it's a pretty simple macro
# that has the same effect as the (->)
# threading macro -- but it's a lot
# easier to type it out.

#### Light and shadow ####

# Currently there's no way to customize
# anything about the lighting. But it's
# coming soon!

#### Procedural texturing ####

# Haha aw I haven't gotten to that yet
# either. But a future version of Bauble
# will support it!

#### Getting Help ####

# Uhhh okay look you have just read
# literally all of the documentation.

# Sorry about that.

# You can print values for debugging
# with (print "string") or
# (pp expression). Error messages are
# extremely bad right now, so don't
# make any mistakes. If you write an
# infinite loop it *will* just hang the
# browser tab and you will have no way
# to get out of it except to refresh
# the page.

# Your changes will automatically save,
# but if you want to restore this
# initial tutorial, just empty out this
# text field and refresh the page.

# For more info... maybe check out the
# source?

# https://github.com/ianthehenry/bauble/blob/master/src/dsl.janet
# https://github.com/ianthehenry/bauble/blob/master/src/helpers.janet

# Or try studying these examples:

# (union (box 50) (sphere 70))
# (union :r 10 (box 50) (cone :z 40 50))
# (sphere 100 | onion 5 | intersect (half-space :-z))
# (sphere 100 | onion 5 | intersect :r 5 (half-space :-z 60))
# (morph 0.5 (box 50) (sphere 50))
# (box 50 | subtract (cylinder :z 30 100))
# (subtract :r 30 (box 50 | rotate :y tau/8 :z tau/8) (sphere 50 | move :x 50))
# (cone :x 50 70)
# (cone :x 50 70 | reflect :x)
# (cone :x 50 70 | rotate :y pi/4 | mirror :x :z)
# (union :r 50 (line [-50 0 0] [50 0 0] 10) (sphere 50 | move :x 100 | mirror :x))
# (sphere 50 | tile [100 100 100] :limit [3 10 2])
# (cone :-z 50 100 :r 10)
# (cone :-z 50 100 | offset 10)
# (box 40 | scale 1.5)
# (box 40 | scale :x 0.5)
# (box 40 | scale [1 2 0.5])
# (torus :y 100 25)

# Comments? Questions? Requests?
# https://github.com/ianthehenry/bauble/discussions

# Found a bug? Let me know!
# https://github.com/ianthehenry/bauble/issues
`.trimLeft();

const preamble = '(use ./helpers) (use ./dsl) (use ./pipe) (pipe\n';
const postamble = '\n)'; // newline is necessary in case the script ends in a comment

function executeJanet(code: string, camera: Camera) {
  if (evaluateJanet === null) {
    console.error('not ready yet');
    return;
  }

  const result = evaluateJanet(preamble + code + postamble, TAU * camera.x, TAU * camera.y, camera.zoom);
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

document.addEventListener("DOMContentLoaded", (_) => {
  const camera = {
    x: -0.125,
    y: 0.125,
    zoom: 2.0,
  };

  let drawScheduled = false;
  function draw() {
    if (drawScheduled) {
      return;
    }
    drawScheduled = true;
    requestAnimationFrame(function() {
      drawScheduled = false;
      clearOutput();
      executeJanet(editor.state.doc.toString(), camera);
    });
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
      EditorView.theme({
        ".cm-content": {
          fontFamily: "Menlo, monospace",
          fontSize: "13px",
        },
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
      draw();
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
    draw();
  });

  canvas.addEventListener('wheel', (e) => {
    e.preventDefault();
    // Linux Firefox users who do not set MOZ_USE_XINPUT2
    // will report very large values of deltaY, resulting
    // in very choppy scrolling. I don't really know a good
    // way to fix this without explicit platform detection.
    camera.zoom += cameraZoomSpeed * e.deltaY;
    draw();
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

  document.body.addEventListener('pointermove', (e) => {
    if (e.ctrlKey) {
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

  onReady(draw);
  editor.focus();
});

window.Module = Module;
