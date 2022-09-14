export function clamp(value: number, min: number, max: number) {
  return Math.max(Math.min(value, max), min);
}

export function mod(a: number, b: number) {
  return ((a % b) + b) % b;
}

const TAU = 2 * Math.PI;
export {TAU};
