export type Seconds = number & {readonly __tag: unique symbol};

export enum RenderType {
  Normal = 0,
  Surfaceless = 1,
  Convergence = 2,
  Distance = 3,
};
