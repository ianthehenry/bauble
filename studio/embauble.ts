import * as Signal from './signals';
import type {Accessor} from 'solid-js';
import type * as RenderState from './render-state';
import {vec3, vec4} from 'gl-matrix';

enum CameraType {
  Custom = 0,
  Free = 1,
  Top = 2,
  Front = 3,
  Right = 4,
}

const vertexSource = `#version 300 es
in vec4 position;
void main() {
  gl_Position = position;
}
`;

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

class Renderer {
  private gl: WebGLRenderingContext;
  private canvas: HTMLCanvasElement;
  private program: WebGLProgram;
  private currentFragmentShader: WebGLShader | null = null;
  private _positionLocation: number | null = null;
  private vertexBuffer: WebGLBuffer;
  private vertexData: Float32Array;

  constructor(canvas: HTMLCanvasElement, private state: {
    time: number,
    rotation: [number, number],
    origin: [number, number, number],
    origin2D: [number, number],
    zoom: number,
  }) {
    this.canvas = canvas;
    const gl = canvas.getContext('webgl2', { antialias: false, premultipliedAlpha: false });
    if (!gl) {
      throw new Error("failed to create webgl2 context");
    }

    const program = gl.createProgram()!;
    gl.attachShader(program, compileShader(gl, gl.VERTEX_SHADER, vertexSource));

    // TODO: change if canvas resizes
    gl.viewport(0, 0, this.canvas.width, this.canvas.height);

    const left = -0.5 * this.canvas.width;
    const right = 0.5 * this.canvas.width;
    const top = 0.5 * this.canvas.height;
    const bottom = -0.5 * this.canvas.height;

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
  }

  private get positionLocation(): number {
    if (this._positionLocation == null) {
      const {gl, program} = this;
      this._positionLocation = gl.getAttribLocation(program, "position");
    }
    return this._positionLocation;
  }

  private setViewport() {
    const {gl, program} = this;
    const uViewport = gl.getUniformLocation(program, "viewport");
    gl.uniform4fv(uViewport, [0, 0, this.canvas.width, this.canvas.height]);
  }

  private setSimpleUniforms() {
    const {gl, program} = this;
    const uT = gl.getUniformLocation(program, "t");
    const uCameraTarget = gl.getUniformLocation(program, "free_camera_target");
    const uOrigin2D = gl.getUniformLocation(program, "origin_2d");
    const uCameraOrbit = gl.getUniformLocation(program, "free_camera_orbit");
    const uCameraZoom = gl.getUniformLocation(program, "free_camera_zoom");
    const uCrosshairs = gl.getUniformLocation(program, "crosshairs_3d");

    gl.uniform1f(uT, this.state.time);
    gl.uniform3fv(uCameraTarget, this.state.origin);
    gl.uniform2fv(uOrigin2D, this.state.origin2D);
    gl.uniform2fv(uCameraOrbit, this.state.rotation);
    gl.uniform1f(uCameraZoom, this.state.zoom);
  }

  draw() {
    this.setSimpleUniforms();
    const {gl, program, vertexBuffer, vertexData, positionLocation} = this;
    gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, vertexData, gl.STATIC_DRAW);
    gl.vertexAttribPointer(positionLocation, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(positionLocation);
    const uCameraType = gl.getUniformLocation(program, "camera_type");
    gl.uniform1i(uCameraType, CameraType.Custom);
    this.setViewport();
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    gl.finish();
  }

  recompileShader(source: string) {
    const {gl, program} = this;
    const fragmentShader = compileShader(gl, gl.FRAGMENT_SHADER, source);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      const info = gl.getProgramInfoLog(program);
      throw new Error("failed to link shader program", {cause: info});
    }
    gl.useProgram(program);
  }
}

export default function Bauble(canvas: HTMLCanvasElement, opts: {
  source: string,
  isAnimated: boolean,
}) {
  const {source, isAnimated} = opts;
  const renderer = new Renderer(canvas, {
    time: 0,
    rotation: [0.1, -0.1],
    origin: [0, 0, 0],
    origin2D: [0, 0],
    zoom: 1,
  });

  renderer.recompileShader(source);

  return renderer;
};
console.log("hi");
