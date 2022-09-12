import { createSignal, onMount } from "solid-js";
import installCodeMirror from './editor';

const EditorToolbar = () => {
  return <div class="toolbar">
    <div class="spacer"></div>
    <div title="Compilation result unknown" class="indicator compilation-unknown"><svg><use href="/icons.svg#emoji-neutral" /></svg></div>
    <div title="Compilation success" class="indicator compilation-success hidden"><svg><use href="/icons.svg#emoji-smile" /></svg></div>
    <div title="Compilation error" class="indicator compilation-error hidden"><svg><use href="/icons.svg#emoji-frown" /></svg></div>
  </div>;
};

const RenderToolbar = () => {
  return <div class="toolbar">
    <button title="Reset camera" data-action="reset-camera"><svg><use href="/icons.svg#box"/></svg></button>
    {/*<button title="Toggle quad view" data-action="toggle-quad-view"><svg><use href="/icons.svg#grid" /></svg></button>*/}
    <div class="spacer"></div>
    <fieldset>
      <label title="Render normally"><input type="radio" autocomplete="off" name="view-type" value="0" checked /><svg><use href="/icons.svg#camera" /></svg></label>
      <label title="Debug number of raymarching steps"><input type="radio" autocomplete="off" name="view-type" value="1" /><svg><use href="/icons.svg#magnet" /></svg></label>
      <label title="Debug surface distance"><input type="radio" autocomplete="off" name="view-type" value="2" /><svg><use href="/icons.svg#arrows-collapse" /></svg></label>
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

const Bauble = (props: { script: string }) => {
  let canvasContainer: HTMLDivElement;
  let editorContainer: HTMLDivElement;
  onMount(() => {
    installCodeMirror(props.script, editorContainer, () => console.log('changed'));
  });

  return <div class="bauble standalone">
    <div class="code-and-preview">
      <div class="code-container">
        <EditorToolbar />
        <div class="editor-container" ref={editorContainer!} />
      </div>
      <div class="canvas-container" ref={canvasContainer!}>
        <RenderToolbar />
        <canvas class="render-target" width="1024" height="1024" />
        <AnimationToolbar />
      </div>
    </div>
    <div class="output-resize-handle" />
    <pre class="output" />
  </div>;
};
export default Bauble;
