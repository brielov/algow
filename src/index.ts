import * as ast from "./ast";
import { infer } from "./checker";

const expr = ast.binOp("+", ast.num(10), ast.num(10));

console.log(expr);
console.log(infer(new Map(), expr));
