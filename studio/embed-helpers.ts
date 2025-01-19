export default function convert(result: any) {
  const options: any = {source: result.shaderSource};
  if (result.dimension !== 3) {
    options.dimensions = result.dimension;
  }
  if (result.isAnimated) {
    options.animation = true;
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

  const uniforms: any = {};
  for (let {name, type, value} of result.uniforms) {
    uniforms[name] = value[type];
  }

  return [options, uniforms];
}
