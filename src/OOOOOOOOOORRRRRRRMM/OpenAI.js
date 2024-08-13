import OpenAI from "openai";

const openai = new OpenAI();

export const createCompletionsImpl = (left) => (right) => (create) => () =>
  openai.chat.completions
    .create(create)
    .then((r) => right(r)())
    .catch((e) => left(e)());
