import pkg from 'pg';
const { Client } = pkg;

export const parsePostgresUrlImpl = (left) => (right) => (url) => {
    const regex = /^postgresql:\/\/(?<user>[^@]+)@(?<host>[^:]+):(?<port>\d+)\/(?<database>.+)$/;
    const match = url.match(regex);
    
    if (match) {
        const { host, port, database, user } = match.groups;
        return right({
            host: host,
            port: parseInt(port, 10), // Convert port to an integer
            database: database,
            user: user
        });
    } else {
        return left;
    }
}

export const newClientImpl = (left) => (right) => (host) => (port) => (user) => (database) => async () => {
        const client = new Client({
            host,port,user,
            database
        });
    
    
        try {
            await client.connect();    
            right(client)();    
        }catch (error) {
            left(error)();
        }
    }

export const runSqlCommandImpl = (left) => (right) => (client) => (sql) => (values) => async () => {
   
    try {
        const res = await client.query(sql, values);
        right(res)();
    } catch (error) {
        left(error)();
    }
}

export const closeClientImpl = (left) => (right) => (client) => async () => {
        try {
            await client.end();
           right();
        } catch (error) { left(error)(); }
    }

    export const pgVarsAppend = (l) => (r) => l.concat(...r);
    export const pgVarsMempty =[];
    export const purePgVars = (a) => [a];