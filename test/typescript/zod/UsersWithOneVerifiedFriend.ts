
  const literalSchema = z.union([z.string(), z.number(), z.boolean(), z.null()]);
  type Literal = z.infer<typeof literalSchema>;
  type Json = Literal | { [key: string]: Json } | Json[];
  const json: z.ZodType<Json> = z.lazy(() =>
    z.union([literalSchema, z.array(json), z.record(json)])
  );
  import { z } from 'zod';

export const i = z.tuple([]);
export type Q = `SELECT DISTINCT u.* FROM users u JOIN friends f ON u.id = f.asker OR u.id = f.receiver;`;
export const q: Q = `SELECT DISTINCT u.* FROM users u JOIN friends f ON u.id = f.asker OR u.id = f.receiver;`;
export const o = z.array(z.object({
    id: z.number(),
    email: z.string().nullable(),
    first_name: z.string(),
    last_name: z.string(),
    username: z.string(),
    verified: z.boolean(),
    phone_number: z.string().nullable(),
    image_url: z.string().nullable(),
    date_joined: z.date(),
    settings: json.nullable()
}));
  export const run = (f: (s: string, v: unknown) => Promise<unknown>) => (input: z.infer<typeof i>) => f(q, input).then(x => o.parse(x));
  