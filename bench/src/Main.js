import process from "process";

export const argv = () => process.argv.slice();
export const exit = (code) => () => process.exit(code);
