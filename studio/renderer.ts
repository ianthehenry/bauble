import {mat3, vec3} from 'gl-matrix';
import * as Signal from './signals';
import type {Accessor} from 'solid-js';
import {clamp, TAU} from './util';
import type {Seconds} from './types';

const baseCameraDistance = 512;

function rotateX(target: mat3, angle: number) {
  const s = Math.sin(angle);
  const c = Math.cos(angle);
  mat3.set(target,
    1.0, 0.0, 0.0,
    0.0, c, -s,
    0.0, s, c);
}

function rotateY(target: mat3, angle: number) {
  const s = Math.sin(angle);
  const c = Math.cos(angle);
  mat3.set(target,
    c, 0.0, s,
    0.0, 1.0, 0.0,
    -s, 0.0, c);
}

function rotateXY(target: mat3, x: number, y: number) {
  const tempY = mat3.create();
  rotateX(target, x);
  rotateY(tempY, y);
  mat3.multiply(target, tempY, target);
}

const vertexSource = `#version 300 es
in vec4 position;
void main() {
  gl_Position = position;
}
`;

declare global {
  interface ErrorConstructor {
    new (message: string, info: {cause: any}): Error;
    (message: string, info: {cause: any}): Error;
  }
}

function compileShader(gl: WebGLRenderingContext, type: number, source: string) {
  const shader = gl.createShader(type)!;
  gl.shaderSource(shader, source);
  gl.compileShader(shader);

  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    const info = gl.getShaderInfoLog(shader);
    gl.deleteShader(shader);
    throw new Error("failed to compile shader", {cause: info});
  }

  return shader;
}

export default class Renderer {
  private gl: WebGLRenderingContext;
  private program: WebGLProgram;
  private currentFragmentShader: WebGLShader | null = null;
  private currentFragmentShaderSource: string | null = null;
  private _positionLocation: number | null = null;
  private vertexBuffer: WebGLBuffer;
  private vertexData: Float32Array;

  private cameraDirty = true;
  private cameraOrientation: vec3 = vec3.create();
  private cameraOrigin: vec3 = vec3.create();

  // TODO: the perspective is actually calculated
  // in the shader, not here, so this is actually a
  // "perspective XY" view
  private orthogonalXY: vec3 = vec3.fromValues(0, 0, 0);
  private orthogonalXZ: vec3 = vec3.fromValues(0.5 * Math.PI, 0, 0);
  private orthogonalZY: vec3 = vec3.fromValues(0, 0.5 * Math.PI, 0);

  constructor(
    canvas: HTMLCanvasElement,
    private time: Signal.T<Seconds>,
    private renderType: Signal.T<number>, // TODO: give this a type
    private rotation: Signal.T<{x: number, y: number}>,
    private origin: Signal.T<{x: number, y: number, z: number}>,
    private zoom: Signal.T<number>, // TODO: give this a unique type
    private quadView: Signal.T<boolean>,
    private quadSplitPoint: Signal.T<{x: number, y: number}>,
    private resolution: Accessor<{width: number, height: number}>,
  ) {
    const gl = canvas.getContext('webgl2', { antialias: false });
    if (!gl) {
      throw new Error("failed to create webgl2 context");
    }

    const program = gl.createProgram()!;
    gl.attachShader(program, compileShader(gl, gl.VERTEX_SHADER, vertexSource));

    const left = -0.5 * canvas.width;
    const right = 0.5 * canvas.width;
    const top = 0.5 * canvas.height;
    const bottom = -0.5 * canvas.height;

    const vertexBuffer = gl.createBuffer()!;
    const vertexData = new Float32Array(6 * 3);
    vertexData[0]  = left;  vertexData[1]  = top;    vertexData[2]  = 0;
    vertexData[3]  = right; vertexData[4]  = top;    vertexData[5]  = 0;
    vertexData[6]  = right; vertexData[7]  = bottom; vertexData[8]  = 0;
    vertexData[9]  = right; vertexData[10] = bottom; vertexData[11] = 0;
    vertexData[12] = left;  vertexData[13] = top;    vertexData[14] = 0;
    vertexData[15] = left;  vertexData[16] = bottom; vertexData[17] = 0;

    this.gl = gl;
    this.program = program;
    this.vertexBuffer = vertexBuffer;
    this.vertexData = vertexData;

    Signal.onEffect([rotation, origin, zoom], () => {
      this.cameraDirty = true;
    });
  }

  private get positionLocation(): number {
    if (this._positionLocation == null) {
      const {gl, program} = this;
      this._positionLocation = gl.getAttribLocation(program, "position");
    }
    return this._positionLocation;
  }

  private calculateCameraMatrix() {
    const {x, y} = Signal.get(this.rotation);
    // x and y here are in "screen" coordinates, where dragging left/right
    // actually rotates around the Y axis, and dragging up/down rotates along
    // the X axis. Hence the strange inversion here.
    vec3.set(this.cameraOrientation, -y * TAU, -x * TAU, 0);
    const cameraMatrix = mat3.create();
    rotateXY(cameraMatrix, this.cameraOrientation[0], this.cameraOrientation[1]);
    vec3.set(this.cameraOrigin, 0, 0, baseCameraDistance * Signal.get(this.zoom));
    vec3.transformMat3(this.cameraOrigin, this.cameraOrigin, cameraMatrix);
    const target = Signal.get(this.origin);
    vec3.add(this.cameraOrigin, this.cameraOrigin, [target.x, target.y, target.z]);
    this.cameraDirty = false;
  }

  private setViewport(left: number, bottom: number, width: number, height: number) {
    const {gl, program} = this;
    const uViewport = gl.getUniformLocation(program, "viewport");
    gl.uniform4fv(uViewport, [left, bottom, width, height]);
    gl.viewport(left, bottom, width, height);
  }

  private setSimpleUniforms() {
    const {gl, program} = this;
    const uT = gl.getUniformLocation(program, "t");
    const uRenderType = gl.getUniformLocation(program, "render_type");

    gl.uniform1f(uT, Signal.get(this.time));
    gl.uniform1i(uRenderType, Signal.get(this.renderType));
  }

  private drawSingleView() {
    const {gl, program} = this;
    const uCameraOrientation = gl.getUniformLocation(program, "camera_orientation");
    const uCameraOrigin = gl.getUniformLocation(program, "camera_origin");
    if (this.cameraDirty) {
      this.calculateCameraMatrix();
    }
    gl.uniform3fv(uCameraOrigin, this.cameraOrigin);
    gl.uniform3fv(uCameraOrientation, this.cameraOrientation);
    const resolution = this.resolution();
    this.setViewport(0, 0, resolution.width, resolution.height);
    gl.drawArrays(gl.TRIANGLES, 0, 6);
  }

  private drawQuadView() {
    const {gl, program} = this;
    const uCameraOrientation = gl.getUniformLocation(program, "camera_orientation");
    const uCameraOrigin = gl.getUniformLocation(program, "camera_origin");

    const splitPoint = Signal.get(this.quadSplitPoint);
    const resolution = this.resolution();
    const minPanelSize = 64;
    const freePaneSize = [
      clamp(Math.round(splitPoint.x * resolution.width), minPanelSize, resolution.width - minPanelSize),
      clamp(Math.round(splitPoint.y * resolution.height), minPanelSize, resolution.height - minPanelSize),
    ];

    const leftPaneWidth = freePaneSize[0];
    const topPaneHeight = freePaneSize[1];
    const rightPaneWidth = resolution.width - freePaneSize[0];
    const bottomPaneHeight = resolution.height - freePaneSize[1];

    const zoom = Signal.get(this.zoom);
    const target = Signal.get(this.origin);
    // bottom left: XY
    gl.uniform3fv(uCameraOrigin, [target.x, target.y, target.z + baseCameraDistance * zoom]);
    gl.uniform3fv(uCameraOrientation, this.orthogonalXY);
    this.setViewport(0, 0, leftPaneWidth, bottomPaneHeight);
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    // bottom right: ZY
    gl.uniform3fv(uCameraOrigin, [target.x - baseCameraDistance * zoom, target.y, target.z]);
    gl.uniform3fv(uCameraOrientation, this.orthogonalZY);
    this.setViewport(leftPaneWidth, 0, rightPaneWidth, bottomPaneHeight);
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    // top left: free camera
    if (this.cameraDirty) {
      this.calculateCameraMatrix();
    }
    gl.uniform3fv(uCameraOrigin, this.cameraOrigin);
    gl.uniform3fv(uCameraOrientation, this.cameraOrientation);
    this.setViewport(0, bottomPaneHeight, leftPaneWidth, topPaneHeight);
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    // top right: top-down
    gl.uniform3fv(uCameraOrigin, [target.x, target.y + baseCameraDistance * zoom, target.z]);
    gl.uniform3fv(uCameraOrientation, this.orthogonalXZ);
    this.setViewport(leftPaneWidth, bottomPaneHeight, rightPaneWidth, topPaneHeight);
    gl.drawArrays(gl.TRIANGLES, 0, 6);
  }

  draw() {
    if (!this.currentFragmentShader) {
      return;
    }
    this.setSimpleUniforms();
    const {gl, vertexBuffer, vertexData, positionLocation} = this;
    gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, vertexData, gl.STATIC_DRAW);
    gl.vertexAttribPointer(positionLocation, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(positionLocation);
    if (Signal.get(this.quadView)) {
      this.drawQuadView();
    } else {
      this.drawSingleView();
    }
  }

  recompileShader(fragmentShaderSource: string) {
    const { gl, program, currentFragmentShader, currentFragmentShaderSource } = this;

    if (fragmentShaderSource === currentFragmentShaderSource) {
      console.info("skipping shader compilation");
      return;
    }

    if (currentFragmentShader) {
      gl.detachShader(program, currentFragmentShader);
      gl.deleteShader(currentFragmentShader);
    }
    try {
      const startTime = performance.now();
      const fragmentShader = compileShader(gl, gl.FRAGMENT_SHADER, fragmentShaderSource);
      gl.attachShader(program, fragmentShader);
      gl.linkProgram(program);
      this._positionLocation = null;
      if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        const info = gl.getProgramInfoLog(program);
        throw new Error("failed to link shader program", {cause: info});
      }
      gl.useProgram(program);
      this.currentFragmentShader = fragmentShader;
      this.currentFragmentShaderSource = fragmentShaderSource;
      const endTime = performance.now();
      console.log(`spent ${endTime - startTime}ms compiling shader`);
    } catch (e) {
      this.currentFragmentShader = null;
      throw e;
    }
  }
}
