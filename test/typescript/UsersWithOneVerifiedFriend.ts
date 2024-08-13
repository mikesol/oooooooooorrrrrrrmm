type I = {};
type Q = `SELECT DISTINCT u.* FROM users u JOIN friends f ON u.id = f.asker OR u.id = f.receiver;`;
type O = { id: number, email?: string, first_name: string, last_name: string, username: string, verified: boolean, phone_number?: string, image_url?: string }[];