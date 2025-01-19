const vertexSource = "#version 300 es\nin vec4 position;void main(){gl_Position=position;}";

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
  private program: WebGLProgram;
  private currentFragmentShader: WebGLShader | null = null;
  private positionLocation: number;
  private vertexBuffer: WebGLBuffer;
  private vertexData: Float32Array;
  private uniformSetters: {[name: string]: (value: any) => void} = {};

  constructor(private canvas: HTMLCanvasElement, opts: {
    time: number,
    freeCamera: 2 | 3 | null,
    source: string,
    uniforms: {[name: string]: string},
    animation: boolean,
  }) {
    const {source, uniforms} = opts;
    const gl = canvas.getContext('webgl2', { antialias: false, premultipliedAlpha: false });
    if (!gl) {
      throw new Error("failed to create webgl2 context");
    }

    const program = gl.createProgram()!;
    gl.attachShader(program, compileShader(gl, gl.VERTEX_SHADER, vertexSource));
    gl.attachShader(program, compileShader(gl, gl.FRAGMENT_SHADER, source));
    gl.linkProgram(program);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      const info = gl.getProgramInfoLog(program);
      throw new Error("failed to link shader", {cause: info});
    }
    gl.useProgram(program);

    const uniformSetter = (name: string, type: string) => {
      const location = gl.getUniformLocation(program, name);
      switch (type) {
      case 'float': return (value: number) => gl.uniform1f(location, value);
      case 'vec2': return (value: [number, number]) => gl.uniform2fv(location, value);
      case 'vec3': return (value: [number, number, number]) => gl.uniform3fv(location, value);
      case 'vec4': return (value: [number, number, number, number]) => gl.uniform4fv(location, value);
      case 'bool': return (value: boolean) => gl.uniform1f(location, value ? 1 : 0);
      case 'int': return (value: number) => gl.uniform1i(location, value);
      case 'ivec2': return (value: [number, number]) => gl.uniform2iv(location, value);
      case 'ivec3': return (value: [number, number, number]) => gl.uniform3iv(location, value);
      case 'ivec4': return (value: [number, number, number, number]) => gl.uniform4iv(location, value);
      case 'uint': return (value: number) => gl.uniform1ui(location, value);
      case 'uvec2': return (value: [number, number]) => gl.uniform2uiv(location, value);
      case 'uvec3': return (value: [number, number, number]) => gl.uniform3uiv(location, value);
      case 'uvec4': return (value: [number, number, number, number]) => gl.uniform4uiv(location, value);
      default: throw new Error("unknown uniform type", {cause: type});
      }
    };

    const addUniform = (name: string, type: string) => {
      this.uniformSetters[name] = uniformSetter(name, type);
    };

    switch (opts.freeCamera) {
    case 2:
      addUniform('free_camera_target', 'vec2');
      addUniform('free_camera_zoom', 'float');
      break;
    case 3:
      addUniform('free_camera_target', 'vec3');
      addUniform('free_camera_orbit', 'vec2');
      addUniform('free_camera_zoom', 'float');
      break;
    case null: break;
    }

    addUniform('t', 'float');
    addUniform('viewport', 'vec4');
    for (let name in opts.uniforms) {
      addUniform(name, opts.uniforms[name]);
    }

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
    this.positionLocation = gl.getAttribLocation(program, 'position');

    this.uniformSetters.viewport([0, 0, this.canvas.width, this.canvas.height]);
    this.uniformSetters.t(opts.time);
  }

  draw() {
    const {gl, program, vertexBuffer, vertexData, positionLocation} = this;
    gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, vertexData, gl.STATIC_DRAW);
    gl.vertexAttribPointer(positionLocation, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(positionLocation);
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    gl.finish();
  }

  setUniform(name: string, value: any) {
    let setter = this.uniformSetters[name];
    if (setter == null) {
      throw new Error("unknown uniform", {cause: name});
    }
    setter(value);
  }
}

const fract = (x: number) => ((x % 1) + 1) % 1;

export default function Bauble(canvas: HTMLCanvasElement, opts: {
  source: string,
  animation?: boolean,
  freeCamera?: boolean,
  interaction?: boolean,
  dimensions?: 2 | 3,
  uniforms?: any,
}) {
  const source = opts.source;
  if (source == null) {
    throw new Error("missing source", {cause: opts});
  }
  const animation = opts.animation ?? false;
  const dimensions = opts.dimensions ?? 3;
  const uniforms = opts.uniforms ?? {};
  const freeCamera = opts.freeCamera ?? true;
  const interaction = opts.interaction ?? true;
  const renderer = new Renderer(canvas, {
    time: 0,
    source,
    uniforms,
    freeCamera: freeCamera ? dimensions : null,
    animation: animation,
  });

  let time = 0;
  let isTimeAdvancing = animation;
  let then: number | null = null;

  const setTime = (t: number) => {
    if (time === t) {
      return;
    }
    time = t;
    renderer.setUniform('t', time)
    draw();
  };

  let drawEnqueued = false;
  function draw() {
    if (drawEnqueued) {
      return;
    }
    drawEnqueued = true;
    requestAnimationFrame((now) => {
      drawEnqueued = false;
      renderer.draw();

      if (then != null) {
        const elapsed = (now - then) / 1000;
        if (isTimeAdvancing) {
          setTime(time + elapsed);
        }
      }
      then = now;
      if (isTimeAdvancing) {
        draw();
      } else {
        then = null;
      }
    });
  }

  const camera: any = {zoom: 1};

  switch (dimensions) {
  case 2: {
    camera.target = [0, 0];
    renderer.setUniform('free_camera_target', camera.target)
    renderer.setUniform('free_camera_zoom', camera.zoom)
    break;
  }
  case 3: {
    camera.rotation = [0.125, -0.125];
    camera.target = [0, 0, 0];
    renderer.setUniform('free_camera_orbit', camera.rotation)
    renderer.setUniform('free_camera_target', camera.target)
    renderer.setUniform('free_camera_zoom', camera.zoom)

    if (!interaction) {
      break;
    }

    let canvasPointerAt = [0, 0];
    let interactionPointer: number | null = null;
    canvas.addEventListener('pointerdown', (e: PointerEvent) => {
      if (interactionPointer != null) {
        return;
      }
      e.preventDefault();
      // TODO: does this do anything if no tabindex?
      canvas.focus();
      canvasPointerAt = [e.offsetX, e.offsetY];
      canvas.setPointerCapture(e.pointerId);
      interactionPointer = e.pointerId;
    });

    canvas.addEventListener('pointerup', (e: PointerEvent) => {
      e.preventDefault();
      if (e.pointerId === interactionPointer) {
        interactionPointer = null;
      }
    });

    canvas.addEventListener('pointermove', (e: PointerEvent) => {
      if (e.pointerId !== interactionPointer) {
        return;
      }
      e.preventDefault();
      const pointerWasAt = canvasPointerAt;
      canvasPointerAt = [e.offsetX, e.offsetY];

      console.log(canvas.offsetWidth, canvas.clientWidth, canvas.offsetWidth / canvas.clientWidth);
      const deltaX = (canvasPointerAt[0] - pointerWasAt[0]) * (canvas.offsetWidth / canvas.clientWidth);
      const deltaY = (canvasPointerAt[1] - pointerWasAt[1]) * (canvas.offsetHeight / canvas.clientHeight);

      const cameraRotateSpeed = 1 / 512;
      camera.rotation = [
        fract(camera.rotation[0] - deltaX * cameraRotateSpeed),
        fract(camera.rotation[1] - deltaY * cameraRotateSpeed),
      ];
      renderer.setUniform('free_camera_orbit', camera.rotation);
      draw();
    });
    break;
  }
  default: throw new Error('unknown dimension', {cause: dimensions});
  }

  draw();
  return {
    draw: draw,
    togglePlay: function(value: boolean) {
      if (arguments.length === 0) {
        isTimeAdvancing = !isTimeAdvancing;
      } else {
        isTimeAdvancing = !!value;
      }
      if (isTimeAdvancing) {
        draw();
      }
    },
    setTime: setTime,
    set: function() {
      if (arguments.length === 2) {
        renderer.setUniform(arguments[0], arguments[1]);
      } else if (arguments.length === 1 && typeof arguments[0] === 'object') {
        const uniforms = arguments[0];
        for (let name in uniforms) {
          renderer.setUniform(name, uniforms[name])
        }
      } else {
        throw new Error("illegal call", {cause: arguments});
      }
      draw();
    },
    setCamera: function(opts: any) {
      switch (dimensions) {
      case 2: {
        if ('zoom' in opts) {
          camera.zoom = opts.zoom;
          renderer.setUniform('free_camera_zoom', camera.zoom);
        }
        if ('target' in opts) {
          camera.target = opts.target;
          renderer.setUniform('origin_2d', camera.target);
        }
        break;
      }
      case 3: {
        if ('rotation' in opts) {
          camera.rotation = opts.rotation;
          renderer.setUniform('free_camera_orbit', camera.rotation);
        }
        if ('zoom' in opts) {
          camera.zoom = opts.zoom;
          renderer.setUniform('free_camera_zoom', camera.zoom);
        }
        if ('target' in opts) {
          camera.target = opts.target;
          renderer.setUniform('free_camera_target', camera.target);
        }
        break;
      }
      }
      draw();
    }
  };
};
