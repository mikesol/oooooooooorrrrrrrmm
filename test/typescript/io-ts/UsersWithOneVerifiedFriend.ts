
function isInstanceOf<T>(ctor: new (...args: any[]) => T) {
  return new t.Type<T, T, unknown>(
    'InstanceOf',
    (u: unknown): u is T => u instanceof ctor,
    (u, c) => (u instanceof ctor ? t.success(u) : t.failure(u, c)),
    t.identity
  );
}

const date = isInstanceOf(Date);


const json = t.recursion('Json', () =>
  t.union([t.null, t.boolean, t.string, t.number, t.array(json), t.record(t.string, json)])
)

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

export const i = tuple([]);
export type Q = `SELECT DISTINCT u.* FROM users u JOIN friends f ON u.id = f.asker OR u.id = f.receiver;`;
export const q: Q = `SELECT DISTINCT u.* FROM users u JOIN friends f ON u.id = f.asker OR u.id = f.receiver;`;
export const o = t.array(
  t.type({
    id: t.number,
    email: t.union([t.null, t.string]),
    first_name: t.string,
    last_name: t.string,
    username: t.string,
    verified: t.boolean,
    phone_number: t.union([t.null, t.string]),
    image_url: t.union([t.null, t.string]),
    date_joined: date,
    settings: t.union([t.null, json])
  })
);
  export const run = (f: (s: string, v: any) => Promise<any>) => (input: t.TypeOf<typeof i>) => f(q, input).then(x => o.decode(x));
