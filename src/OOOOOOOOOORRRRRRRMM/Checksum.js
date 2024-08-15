import crypto from "crypto";

export const checksum = (text) =>
  crypto.createHash("md5").update(text, "utf8").digest("hex");
