
type TupleFn = <TCodecs extends readonly [...t.Mixed[]]>(
  codecs: TCodecs,
  name?: string,
) => t.TupleType<
  {
    -readonly [K in keyof TCodecs]: TCodecs[K];
  },
  {
    [K in keyof TCodecs]: TCodecs[K] extends t.Mixed
      ? t.TypeOf<TCodecs[K]>
      : unknown;
  },
  {
    [K in keyof TCodecs]: TCodecs[K] extends t.Mixed
      ? t.OutputOf<TCodecs[K]>
      : unknown;
  }
>;
const tuple: TupleFn = t.tuple as any;
import * as t from 'io-ts';

export const i = tuple([
  t.union([t.null, t.string]), // email
  t.string, // first_name
  t.string, // last_name
  t.string, // username
  t.boolean, // verified
  t.union([t.null, t.string]), // phone_number
  t.union([t.null, t.string])  // image_url
]);

export type Q = `INSERT INTO users (email, first_name, last_name, username, verified, phone_number, image_url, date_joined, settings) VALUES ($1, $2, $3, $4, $5, $6, $7, NOW(), NULL);`;
export const q: Q = `INSERT INTO users (email, first_name, last_name, username, verified, phone_number, image_url, date_joined, settings) VALUES ($1, $2, $3, $4, $5, $6, $7, NOW(), NULL);`;

export const o = t.type({
  id: t.number,
  // Note: date_joined is not included in the output as per the INSERT semantics
});

  export const run = (f: (s: string, v: any) => Promise<any>) => (input: t.TypeOf<typeof i>) => f(q, input).then(x => o.decode(x));
