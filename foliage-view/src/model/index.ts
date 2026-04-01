import './foliage_model.js';

interface FoliageModelAPI {
  exampleProp(): string;
}

export const FoliageModel = (globalThis as Record<string, unknown>).FoliageModel as FoliageModelAPI;
