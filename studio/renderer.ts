import * as Signal from './signals';
import type {Accessor} from 'solid-js';
import {clamp, TAU} from './util';
import type {Seconds} from './types';
import type * as RenderState from './render-state';

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

  constructor(canvas: OffscreenCanvas, private state: RenderState.Accessors) {
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
  }

  private get positionLocation(): number {
    if (this._positionLocation == null) {
      const {gl, program} = this;
      this._positionLocation = gl.getAttribLocation(program, "position");
    }
    return this._positionLocation;
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
    const uCameraTarget = gl.getUniformLocation(program, "free_camera_target");
    const uCameraOrbit = gl.getUniformLocation(program, "free_camera_orbit");
    const uCameraZoom = gl.getUniformLocation(program, "free_camera_zoom");

    gl.uniform1f(uT, this.state.time());
    gl.uniform1i(uRenderType, this.state.renderType());
    gl.uniform3fv(uCameraTarget, this.state.origin());
    gl.uniform2fv(uCameraOrbit, this.state.rotation());
    gl.uniform1f(uCameraZoom, this.state.zoom());
  }

  private drawSingleView() {
    const {gl, program} = this;
    const uCameraType = gl.getUniformLocation(program, "camera_type");
    const uOrigin2D = gl.getUniformLocation(program, "origin_2d");
    gl.uniform1i(uCameraType, this.state.prefersFreeCamera() ? CameraType.Free : CameraType.Custom);
    gl.uniform2fv(uOrigin2D, this.state.origin2D());
    const resolution = this.state.resolution();
    this.setViewport(0, 0, resolution.width, resolution.height);
    gl.drawArrays(gl.TRIANGLES, 0, 6);
  }

  private drawQuadView() {
    const {gl, program} = this;
    const uCameraType = gl.getUniformLocation(program, "camera_type");
    const uOrigin2D = gl.getUniformLocation(program, "origin_2d");

    const origin = this.state.origin();
    const splitPoint = this.state.quadSplitPoint();
    const resolution = this.state.resolution();
    const minPanelSize = 64;
    const freePaneSize = [
      clamp(Math.round(splitPoint[0] * resolution.width), minPanelSize, resolution.width - minPanelSize),
      clamp(Math.round(splitPoint[1] * resolution.height), minPanelSize, resolution.height - minPanelSize),
    ];

    const leftPaneWidth = freePaneSize[0];
    const topPaneHeight = freePaneSize[1];
    const rightPaneWidth = resolution.width - freePaneSize[0];
    const bottomPaneHeight = resolution.height - freePaneSize[1];

    // bottom left: XY
    gl.uniform1i(uCameraType, CameraType.Front);
    gl.uniform2fv(uOrigin2D, [origin[0], origin[1]]);
    this.setViewport(0, 0, leftPaneWidth, bottomPaneHeight);
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    // bottom right: ZY
    gl.uniform1i(uCameraType, CameraType.Right);
    gl.uniform2fv(uOrigin2D, [origin[2], origin[1]]);
    this.setViewport(leftPaneWidth, 0, rightPaneWidth, bottomPaneHeight);
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    // top left: free camera
    gl.uniform1i(uCameraType, this.state.prefersFreeCamera() ? CameraType.Free : CameraType.Custom);
    gl.uniform2fv(uOrigin2D, this.state.origin2D());
    this.setViewport(0, bottomPaneHeight, leftPaneWidth, topPaneHeight);
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    // top right: top-down
    gl.uniform1i(uCameraType, CameraType.Top);
    gl.uniform2fv(uOrigin2D, [origin[0], origin[2]]);
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
    if (this.state.quadView()) {
      this.drawQuadView();
    } else {
      this.drawSingleView();
    }
    gl.finish();
  }

  recompileShader(source: string) {
    const {
      gl,
      program,
      currentFragmentShader: oldFragmentShader,
      currentFragmentShaderSource: oldFragmentShaderSource,
    } = this;

    if (source === oldFragmentShaderSource) {
      console.info("skipping shader compilation");
      return;
    }

    const startTime = performance.now();
    const newFragmentShader = compileShader(gl, gl.FRAGMENT_SHADER, source);
    this.currentFragmentShader = newFragmentShader;
    this.currentFragmentShaderSource = source;
    if (oldFragmentShader) {
      gl.detachShader(program, oldFragmentShader);
      gl.deleteShader(oldFragmentShader);
    }
    gl.attachShader(program, newFragmentShader);
    gl.linkProgram(program);
    this._positionLocation = null;
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      const info = gl.getProgramInfoLog(program);
      throw new Error("failed to link shader program", {cause: info});
    }
    gl.useProgram(program);
    const endTime = performance.now();
    return endTime - startTime;
  }
}
