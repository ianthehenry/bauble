import {mat3, vec3} from 'gl-matrix';

function rotateXY(x: number, y: number): mat3 {
  const sx = Math.sin(x);
  const sy = Math.sin(y);
  const cx = Math.cos(x);
  const cy = Math.cos(y);

  return mat3.fromValues(
    cy     , 0.0,     -sy,
    sy * sx,  cx, cy * sx,
    sy * cx, -sx, cy * cx,
  );
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
    console.error(info);
    throw new Error("failed to compile shader ugh typescript why");
    // throw new Error("failed to compile shader", {cause: info});
  }

  return shader;
}

export default class Renderer {
  private gl: WebGLRenderingContext;
  private program: WebGLProgram;
  private currentFragmentShader: WebGLShader | null = null;
  private _positionLocation: number | null = null;
  private vertexBuffer: WebGLBuffer;
  private vertexData: Float32Array;

  private cameraMatrix: mat3 = mat3.create();
  private cameraOrigin: vec3 = vec3.create();
  time: number = 0;
  viewType: number = 0;

  constructor(canvas: HTMLCanvasElement) {
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

  get positionLocation(): number {
    if (this._positionLocation == null) {
      const {gl, program} = this;
      this._positionLocation = gl.getAttribLocation(program, "position");
    }
    return this._positionLocation;
  }

  setAllUniforms() {
    const {gl, program} = this;
    const uCameraMatrix = gl.getUniformLocation(program, "camera_matrix");
    const uCameraOrigin = gl.getUniformLocation(program, "camera_origin");
    const uT = gl.getUniformLocation(program, "t");
    const uViewType = gl.getUniformLocation(program, "view_type");

    gl.uniform3fv(uCameraOrigin, this.cameraOrigin);
    gl.uniformMatrix3fv(uCameraMatrix, false, this.cameraMatrix);
    gl.uniform1f(uT, this.time);
    gl.uniform1i(uViewType, this.viewType);
  }

  draw() {
    if (!this.currentFragmentShader) {
      return;
    }
    this.setAllUniforms();
    const {gl, vertexBuffer, vertexData, positionLocation} = this;
    gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, vertexData, gl.STATIC_DRAW);
    gl.vertexAttribPointer(positionLocation, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(positionLocation);
    gl.viewport(0, 0, 1024, 1024);
    gl.drawArrays(gl.TRIANGLES, 0, 6);
  }

  recompileShader(fragmentSource: string) {
    const { gl, program, currentFragmentShader } = this;
    if (currentFragmentShader) {
      gl.detachShader(program, currentFragmentShader);
      gl.deleteShader(currentFragmentShader);
    }
    try {
      const fragmentShader = compileShader(gl, gl.FRAGMENT_SHADER, fragmentSource);
      gl.attachShader(program, fragmentShader);
      gl.linkProgram(program);
      this._positionLocation = null;
      if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        const info = gl.getProgramInfoLog(program);
        console.error(info);
        throw new Error("failed to link shader program ugh typescript why");
        // throw new Error("failed to link shader program", {cause: info});
      }
      gl.useProgram(program);
      this.currentFragmentShader = fragmentShader;
    } catch (e) {
      this.currentFragmentShader = null;
      console.error(e);
      // TODO:
      // if (e instanceof Error) {
      //   print(e.stack!, true);
      // }
    }
  }

  // TODO: obviously don't call this on every frame
  updateCamera(x: number, y: number, zoom: number) {
    const cameraMatrix = rotateXY(x, y);
    const cameraOrigin = vec3.fromValues(0, 0, 256 * zoom);
    vec3.transformMat3(cameraOrigin, cameraOrigin, cameraMatrix);
    this.cameraMatrix = cameraMatrix;
    this.cameraOrigin = cameraOrigin;
  }
}
