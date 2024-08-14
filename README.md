# oooooooooorrrrrrrmm

An ORM.

## Prerequisites

1. Get an [OpenAI API key](https://help.openai.com/en/articles/4936850-where-do-i-find-my-openai-api-key). You'll need it below.

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
pnm i -D oooooooooorrrrrrrmm
```

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
OPENAI_API_KEY=<my-key> pnpm oooooooooorrrrrrrmm migrate -m migrations -q queries
```

It will output the following migration to `migrations/__raw/0`:

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

### Bootstrap a db

You can apply these migrations sequentially to your db by running:

```bash
OPENAI_API_KEY=<my-key> pnpm oooooooooorrrrrrrmm bootstrap -m migrations -c 'postgresql://my.connection.string'
```

### Bootstrap a db

You can apply these migrations sequentially to a temporary db by running:

```bash
OPENAI_API_KEY=<my-key> pnpm oooooooooorrrrrrrmm bootstrap-tmp -m migrations
```

A URL of the temporary DB will be printed to the command line. If you want to set up your temporary DB a particular way, you can pass an `--args` command followed by args to [`pg_tmp`](https://eradman.com/ephemeralpg/).

### Create a query

Assuming your queries directory in the root of your project is `queries`, first create a free-form query with a kebab case name. For example, a file named `user-with-a-given-email-address` with the following content.

```
Select a user with a given email address.
```

Now run:

```bash
OPENAI_API_KEY=<my-key> pnpm oooooooooorrrrrrrmm query -m migrations -q queries
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
OPENAI_API_KEY=<my-key> pnpm oooooooooorrrrrrrmm schema -m migrations -p schema.sql
```

The schema is the raw one output by postgres. To make it more human readable, add the `--legible` or `-l` flag to the command.

### Question

You can ask an arbitrary question about your schema by running:

```bash
OPENAI_API_KEY=<my-key> pnpm oooooooooorrrrrrrmm schema -m migrations -a "What are the tables in this app"`
```

### PureScript bindings

You can generate PureScript bindings for all your queries by running:

```bash
OPENAI_API_KEY=<my-key> pnpm oooooooooorrrrrrrmm purescript -m migrations -q queries
```

The bindings require the following PureScript packages to be installed:

- `aff`
- `foreign`
- `maybe`
- `yoga-json`

### TypeScript bindings

You can generate TypeScript bindings for all your queries by running:

```bash
OPENAI_API_KEY=<my-key> pnpm oooooooooorrrrrrrmm typescript -m migrations -q queries
```

To use `zod` validation in the bindings, add `-v zod`. To use `io-ts` validation, add `-v io-ts`.
