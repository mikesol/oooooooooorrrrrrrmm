type I = { "$1": string };

type Q = `SELECT * FROM users WHERE email = $1;`;

type O = { id: number, email?: string, first_name: string, last_name: string, username: string, verified: boolean, phone_number?: string, image_url?: string }[];