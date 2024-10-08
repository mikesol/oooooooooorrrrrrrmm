# oooooooooorrrrrrrmm

An ORM.

## Prerequisites

1. A [chat completions API](#byocca).

2. Install `postgresql` via `apt` or `brew`.

3. Download [`pg_tmp`](https://github.com/eradman/ephemeralpg), then run `make && sudo make install` to install it.

## Installation

```bash
npm i -D oooooooooorrrrrrrmm
```

or

```bash
yarn add -D oooooooooorrrrrrrmm
```

or

```bash
pnpm i -D oooooooooorrrrrrrmm
```

## BYOCCA

`oooooooooorrrrrrrmm` is Bring Your Own Chat Completions API. The GitHub Action end-to-end test in this repo runs against `gpt-4o`.

You can let `oooooooooorrrrrrrmm` know about your chat completions api one of two ways:

### Environment variables

Use the following environment variables to set up your completions api:

| Variable | Description |
| --- | --- |
| `COMPLETIONS_URL` | The url of the completions API. |
| `COMPLETIONS_MODEL` | The model to use, ie `mistralai/Mixtral-8x7B-Instruct-v0.1` |
| `COMPLETIONS_TOKEN` | A bearer token, ie `sk-foo-bar` from Open AI. |

### JSON config

Create an `oooooooooorrrrrrrmm.config.json` with keys `"url"`, `"model"`, and optionally `"token"`.

You can also add an `additionalHeaders` object in the JSON to provide additional headers, for example if you're using [Claude messages](https://docs.anthropic.com/en/api/messages).

## Usage

The following commands are written in `pnpm` style. `yarn` works the same way. For `npm`, use `npx`.

### Create a migration

Assuming your migrations directory in the root of your project is `migrations`, you create your first migration by creating the file `migrations/0`. Here's an example of what it can contain.

```
Create a user table containing a primary id,
an optional email field, a required first name field, and a
required last name field. Also create username and please make this unique.
```

Nice! Now run:

```bash
pnpm oooooooooorrrrrrrmm migrate -m migrations -q queries
```

> The `-m` flag points to your migrations directory and the `-q` flag points to your queries directory. We'll omit them from future commands as they are the default.

The `migrate` will output the following migration to `migrations/__raw/0`:

```sql
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    email VARCHAR(255),
    first_name VARCHAR(100) NOT NULL,
    last_name VARCHAR(100) NOT NULL,
    username VARCHAR(50) UNIQUE NOT NULL
);
```

Create your new migrations in increasing order, counting up from 0.

Then just run the same command and the new migrations will be generated in the `__raw` folder.

Instead of writing a descriptive text, you can always hand-write SQL for a migration as well. In this case, _always_ add `--raw` as a single-line comment
to the top of your request.

### Bootstrap a db

You can apply these migrations sequentially to your db by running:

```bash
pnpm oooooooooorrrrrrrmm bootstrap -c 'postgresql://my.connection.string'
```

### Bootstrap a temporary db

You can apply these migrations sequentially to a temporary db by running:

```bash
pnpm oooooooooorrrrrrrmm bootstrap-tmp
```

A URL of the temporary DB will be printed to the command line. If you want to set up your temporary DB a particular way, you can pass an `--args` command followed by args to [`pg_tmp`](https://eradman.com/ephemeralpg/).

### Reverting migrations

If you haven't committed a migration to the db and/or to vc, the easiest way to revert it is to:

- the migration in your migrations directory
- a file with the same name in the `__raw` directory of your migrations directory
- a file with the same name in the `__meta` directory of your migrations directory
- recreate your [queries](#create-a-query) and [bindings](#purescript-bindings)

Sometimes, you'll get into a funky state where the db and your migrations fall out of sync. When this happens, the easiest thing to do is to run `pg_dump` on your db to generate a schema, delete your migrations, and add a `0` migration that starts with a single-line comment `--raw` followed by the schema. Then, regenerate your queries and bindings. Most stuff shouldn't budge.

### Create a query

Assuming your queries directory in the root of your project is `queries`, first create a free-form query with a kebab case name. For example, a file named `user-with-a-given-email-address` with the following content.

```
Select a user with a given email address.
```

Now run:

```bash
pnpm oooooooooorrrrrrrmm query
```

It will output the following query to `queries/__raw/user-with-a-given-email-address`:

```sql
SELECT * FROM users WHERE email = $1;
```

You can create as many queries as you want, and the script will generate queries for all of them.

### Queries during migrations

The `migrate` command will automatically any existing [queries](#create-a-query) to reflect the new schema. If a query is no longer relevant, you can delete it. 

Sometimes, a query may still be relevant but there are multiple ways it could be modified to apply to the new schema. In these cases, the migration script will fail.

You can provide this context by creating a file `context/<query-name>/<migration>`, for example `context/get-verified-users/42` for the `42` migration and the query `get-verified-users`. Then, rerun the migration command.

### Schema

You can generate the full schema for all your migrations by running:

```bash
pnpm oooooooooorrrrrrrmm schema -p schema.sql
```

The schema is the raw one output by postgres. To make it more human readable, add the `--legible` or `-l` flag to the command.

### Question

You can ask an arbitrary question about your schema by running:

```bash
pnpm oooooooooorrrrrrrmm schema -a "What are the tables in this app?"
```

### PureScript bindings

You can generate PureScript bindings for all your queries by running:

```bash
pnpm oooooooooorrrrrrrmm purescript
```

Bindings are only generated for new queries or queries that have changed since the last generation.

The bindings require the following PureScript packages to be installed:

- `aff`
- `foreign`
- `js-date`
- `maybe`
- `yoga-json`

### TypeScript bindings

You can generate TypeScript bindings for all your queries by running:

```bash
pnpm oooooooooorrrrrrrmm typescript
```

Bindings are only generated for new queries or queries that have changed since the last generation.

To use `zod` validation in the bindings, add `-v zod`. To use `io-ts` validation, add `-v io-ts`.

### Pre-commit hook

The pre-commit hook can be run by invoking:

```bash
pnpm oooooooooorrrrrrrmm pre-commit
```

It validates several things:

- all queries correspond to the most recent migrations
- all queries correspond to the most recent text descriptions
- all language bindings correspond to the most recent migrations and queries

If any of these are not the case, the hook will fail.

## Migrating from another ORM

The cleanest way to migrate from another ORM is to use `pg_dump` to get your current schema and create a `0` migration with the contents of the db dump, adding `--raw` as a single-line comment at the beginning.

Then, for each query in your app, you can paste the old ORM code as a reference and it will generate the correct query.
