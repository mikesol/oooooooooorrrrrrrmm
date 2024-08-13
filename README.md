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

You can apply these migrations to your db sequentially by using, for example, a python script.

```python
import psycopg2
import os

# Set up the database connection
conn = psycopg2.connect(
    dbname="your_database_name",
    user="your_username",
    password="your_password",
    host="localhost"
)
cur = conn.cursor()

# Directory where the migrations are stored
migrations_dir = 'migrations/__raw'

# Get the list of migration files in sequential order
migration_files = sorted(os.listdir(migrations_dir), key=lambda x: int(x))

# Apply each migration in sequence
for migration_file in migration_files:
    with open(os.path.join(migrations_dir, migration_file), 'r') as file:
        sql = file.read()
        try:
            cur.execute(sql)
            conn.commit()
            print(f"Applied migration: {migration_file}")
        except Exception as e:
            conn.rollback()
            print(f"Failed to apply migration {migration_file}: {e}")
            break

# Close the connection
cur.close()
conn.close()
```

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

### PureScript bindings

You can generate PureScript bindings for all your queries by running:

```bash
OPENAI_API_KEY=<my-key> pnpm oooooooooorrrrrrrmm purescript -m migrations -q queries
```

### TypeScript bindings

You can generate TypeScript bindings for all your queries by running:

```bash
OPENAI_API_KEY=<my-key> pnpm oooooooooorrrrrrrmm typescript -m migrations -q queries
```
