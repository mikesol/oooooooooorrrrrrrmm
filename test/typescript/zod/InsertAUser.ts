
  const literalSchema = z.union([z.string(), z.number(), z.boolean(), z.null()]);
  type Literal = z.infer<typeof literalSchema>;
  type Json = Literal | { [key: string]: Json } | Json[];
  const json: z.ZodType<Json> = z.lazy(() =>
    z.union([literalSchema, z.array(json), z.record(json)])
  );
  import { z } from 'zod';

export const i = z.tuple([
    z.string(), // email
    z.string(), // first_name
    z.string(), // last_name
    z.string(), // username
    z.boolean(), // verified
    z.string().nullable(), // phone_number
    z.string().nullable() // image_url
]);
export type Q = `INSERT INTO users (email, first_name, last_name, username, verified, phone_number, image_url, date_joined, settings) VALUES ($1, $2, $3, $4, $5, $6, $7, NOW(), NULL);`;
export const q: Q = `INSERT INTO users (email, first_name, last_name, username, verified, phone_number, image_url, date_joined, settings) VALUES ($1, $2, $3, $4, $5, $6, $7, NOW(), NULL);`;
export const o = z.array(z.object({ 
    id: z.number(), // assuming id is auto-generated and is an integer
    verified: z.boolean(),
    email: z.string().nullable(),
    first_name: z.string(),
    last_name: z.string(),
    username: z.string(),
    phone_number: z.string().nullable(),
    image_url: z.string().nullable(),
    date_joined: z.date(),
    settings: json.nullable() 
}));
  export const run = (f: (s: string, v: unknown) => Promise<unknown>) => (input: z.infer<typeof i>) => f(q, input).then(x => o.parse(x));
  