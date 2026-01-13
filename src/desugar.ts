/**
 * Desugaring: Surface AST → Core AST
 *
 * Implements the desugaring rules from SPEC.md Section 6.
 * Removes all syntactic sugar, producing a minimal Core AST.
 *
 * Note: Names in Core AST are still strings at this stage.
 * Unique IDs are assigned during name resolution (resolve.ts).
 */

import * as S from "./surface";
import type { NodeId, NodeIdGenerator } from "./surface";
import { createNodeIdGenerator } from "./surface";
import * as C from "./core";

// Temporary name generator for intermediate bindings
let _tempCounter = 0;
const resetTempCounter = (): void => {
  _tempCounter = 0;
};
const freshTemp = (): string => `$t${_tempCounter++}`;

// Desugarer state - holds node ID generator for synthetic nodes
type DesugarState = {
  readonly nodeIds: NodeIdGenerator;
};

const createDesugarState = (): DesugarState => ({
  nodeIds: createNodeIdGenerator(),
});

// Helper to generate new node IDs for synthetic nodes
const freshNodeId = (state: DesugarState): NodeId => state.nodeIds.next();

// Helper to create unresolved Name (with placeholder ID -1)
// ID will be assigned during name resolution
const unresolvedName = (text: string, span: S.Span): C.Name => ({ id: -1, text, span });

// =============================================================================
// Expression Desugaring (Section 6.1)
// =============================================================================

export const desugarExpr = (state: DesugarState, expr: S.SExpr): C.CExpr => {
  switch (expr.kind) {
    case "SVar":
      // D⟦ SVar x ⟧ = CVar x
      // Note: Name ID will be assigned during resolution
      // Direct mapping: preserve nodeId from Surface AST
      return C.cvar(expr.nodeId, unresolvedName(expr.name.text, expr.name.span), expr.span);

    case "SLit":
      // D⟦ SLit l ⟧ = CLit l
      // Direct mapping: preserve nodeId from Surface AST
      return C.clit(expr.nodeId, expr.value, expr.span);

    case "SApp":
      // D⟦ SApp e1 e2 ⟧ = CApp (D⟦e1⟧) (D⟦e2⟧)
      // Direct mapping: preserve nodeId from Surface AST
      return C.capp(
        expr.nodeId,
        desugarExpr(state, expr.func),
        desugarExpr(state, expr.arg),
        expr.span,
      );

    case "SAbs":
      // D⟦ SAbs [] e ⟧ = D⟦e⟧
      // D⟦ SAbs (x:xs) e ⟧ = CAbs x (D⟦ SAbs xs e ⟧)
      return desugarAbs(state, expr.nodeId, expr.params, expr.body, expr.span);

    case "SLet":
      // D⟦ SLet x e1 e2 ⟧ = CLet x (D⟦e1⟧) (D⟦e2⟧)
      // Direct mapping: preserve nodeId from Surface AST
      return C.clet(
        expr.nodeId,
        unresolvedName(expr.name.text, expr.name.span),
        desugarExpr(state, expr.value),
        desugarExpr(state, expr.body),
        expr.span,
      );

    case "SLetRec":
      // D⟦ SLetRec bs e ⟧ = CLetRec (map (λ(n,e) -> (n, D⟦e⟧)) bs) (D⟦e⟧)
      // Direct mapping: preserve nodeId from Surface AST
      return C.cletrec(
        expr.nodeId,
        expr.bindings.map((b) => ({
          name: unresolvedName(b.name.text, b.name.span),
          value: desugarExpr(state, b.value),
        })),
        desugarExpr(state, expr.body),
        expr.span,
      );

    case "SIf":
      // D⟦ SIf c t f ⟧ = CMatch (D⟦c⟧) [
      //   Case (CPLit (LBool true)) Nothing (D⟦t⟧),
      //   Case (CPLit (LBool false)) Nothing (D⟦f⟧)
      // ]
      // Synthetic transformation: SIf becomes CMatch, use fresh nodeId
      return C.cmatch(
        freshNodeId(state),
        desugarExpr(state, expr.cond),
        [
          {
            pattern: C.cplit(freshNodeId(state), { kind: "bool", value: true }, expr.cond.span),
            guard: null,
            body: desugarExpr(state, expr.thenBranch),
          },
          {
            pattern: C.cplit(freshNodeId(state), { kind: "bool", value: false }, expr.cond.span),
            guard: null,
            body: desugarExpr(state, expr.elseBranch),
          },
        ],
        expr.span,
      );

    case "SMatch":
      // D⟦ SMatch e cases ⟧ = CMatch (D⟦e⟧) (map D_case cases)
      // Direct mapping: preserve nodeId from Surface AST
      return C.cmatch(
        expr.nodeId,
        desugarExpr(state, expr.scrutinee),
        expr.cases.map((c) => desugarCase(state, c)),
        expr.span,
      );

    case "SCon":
      // D⟦ SCon c ⟧ = CCon c
      // Direct mapping: preserve nodeId from Surface AST
      return C.ccon(expr.nodeId, expr.name, expr.span);

    case "STuple":
      // D⟦ STuple es ⟧ = CTuple (map D es)
      // Direct mapping: preserve nodeId from Surface AST
      return C.ctuple(
        expr.nodeId,
        expr.elements.map((e) => desugarExpr(state, e)),
        expr.span,
      );

    case "SRecord":
      // D⟦ SRecord fs ⟧ = CRecord (map (λ(n,e) -> (n, D⟦e⟧)) fs)
      // Direct mapping: preserve nodeId from Surface AST
      return C.crecord(
        expr.nodeId,
        expr.fields.map((f) => ({ name: f.name, value: desugarExpr(state, f.value) })),
        expr.span,
      );

    case "SRecordUpdate":
      // D⟦ SRecordUpdate r fs ⟧ = CRecordUpdate (D⟦r⟧) (map (λ(n,e) -> (n, D⟦e⟧)) fs)
      // Direct mapping: preserve nodeId from Surface AST
      return C.crecordUpdate(
        expr.nodeId,
        desugarExpr(state, expr.record),
        expr.fields.map((f) => ({ name: f.name, value: desugarExpr(state, f.value) })),
        expr.span,
      );

    case "SField":
      // Check if this is a qualified module reference (Module.name)
      if (expr.record.kind === "SCon") {
        const qualifiedName = `${expr.record.name}.${expr.field}`;
        // If field starts with uppercase, it's a constructor (Module.Constructor)
        // Otherwise it's a function/variable (Module.func)
        if (expr.field[0] && expr.field[0] === expr.field[0].toUpperCase()) {
          // Direct mapping: preserve nodeId from Surface AST
          return C.ccon(expr.nodeId, qualifiedName, expr.span);
        }
        // Pass module span (from the constructor) and member span (from the field)
        // Direct mapping: preserve nodeId from Surface AST
        return C.cvar(
          expr.nodeId,
          unresolvedName(qualifiedName, expr.fieldSpan),
          expr.span,
          expr.record.span,
          expr.fieldSpan,
        );
      }
      // D⟦ SField e f ⟧ = CField (D⟦e⟧) f
      // Direct mapping: preserve nodeId from Surface AST
      return C.cfield(expr.nodeId, desugarExpr(state, expr.record), expr.field, expr.span);

    case "SList":
      // D⟦ SList [] ⟧ = CCon "Nil"
      // D⟦ SList (e:es) ⟧ = CApp (CApp (CCon "Cons") (D⟦e⟧)) (D⟦ SList es ⟧)
      return desugarList(state, expr.nodeId, expr.elements, expr.span);

    case "SPipe":
      // D⟦ SPipe e1 e2 ⟧ = CApp (D⟦e2⟧) (D⟦e1⟧)
      // Synthetic transformation: becomes CApp with swapped args, use fresh nodeId
      return C.capp(
        freshNodeId(state),
        desugarExpr(state, expr.right),
        desugarExpr(state, expr.left),
        expr.span,
      );

    case "SCons":
      // D⟦ SCons e1 e2 ⟧ = CApp (CApp (CCon "Cons") (D⟦e1⟧)) (D⟦e2⟧)
      // Synthetic transformation: becomes nested CApp with CCon, use fresh nodeIds
      return C.capp(
        freshNodeId(state),
        C.capp(
          freshNodeId(state),
          C.ccon(freshNodeId(state), "Cons", expr.span),
          desugarExpr(state, expr.head),
          expr.span,
        ),
        desugarExpr(state, expr.tail),
        expr.span,
      );

    case "SBinOp":
      // Special cases for && and || (short-circuit evaluation)
      if (expr.op === "&&") {
        // a && b → if a then b else false
        // Synthetic transformation: becomes CMatch, use fresh nodeIds
        return C.cmatch(
          freshNodeId(state),
          desugarExpr(state, expr.left),
          [
            {
              pattern: C.cplit(freshNodeId(state), { kind: "bool", value: true }, expr.left.span),
              guard: null,
              body: desugarExpr(state, expr.right),
            },
            {
              pattern: C.cplit(freshNodeId(state), { kind: "bool", value: false }, expr.left.span),
              guard: null,
              body: C.clit(freshNodeId(state), { kind: "bool", value: false }, expr.span),
            },
          ],
          expr.span,
        );
      }
      if (expr.op === "||") {
        // a || b → if a then true else b
        // Synthetic transformation: becomes CMatch, use fresh nodeIds
        return C.cmatch(
          freshNodeId(state),
          desugarExpr(state, expr.left),
          [
            {
              pattern: C.cplit(freshNodeId(state), { kind: "bool", value: true }, expr.left.span),
              guard: null,
              body: C.clit(freshNodeId(state), { kind: "bool", value: true }, expr.span),
            },
            {
              pattern: C.cplit(freshNodeId(state), { kind: "bool", value: false }, expr.left.span),
              guard: null,
              body: desugarExpr(state, expr.right),
            },
          ],
          expr.span,
        );
      }
      // Keep binary operators as direct operations (not function calls)
      // Direct mapping: preserve nodeId from Surface AST
      return C.cbinop(
        expr.nodeId,
        expr.op,
        desugarExpr(state, expr.left),
        desugarExpr(state, expr.right),
        expr.span,
      );

    case "SDo":
      // D⟦ SDo stmts ⟧ = D_do⟦stmts⟧
      return desugarDo(state, expr.stmts, expr.span);

    case "SAnnot":
      // D⟦ SAnnot e t ⟧ = D⟦e⟧
      // Type annotation is erased (used only during type checking)
      return desugarExpr(state, expr.expr);
  }
};

// =============================================================================
// Helper Functions
// =============================================================================

const desugarAbs = (
  state: DesugarState,
  nodeId: NodeId,
  params: readonly S.SParam[],
  body: S.SExpr,
  span: S.Span,
): C.CExpr => {
  if (params.length === 0) {
    return desugarExpr(state, body);
  }

  // Build nested lambdas from right to left
  let result = desugarExpr(state, body);
  for (let i = params.length - 1; i >= 0; i--) {
    const param = params[i]!;
    // For nested lambdas, use body's span; for outermost, use provided span
    const lambdaSpan = i === 0 ? span : body.span;
    // Use the original nodeId for outermost lambda, fresh for inner ones (synthetic)
    const lambdaNodeId = i === 0 ? nodeId : freshNodeId(state);
    result = C.cabs(
      lambdaNodeId,
      unresolvedName(param.name.text, param.name.span),
      result,
      lambdaSpan,
      param.name.span,
    );
  }
  return result;
};

const desugarList = (
  state: DesugarState,
  nodeId: NodeId,
  elements: readonly S.SExpr[],
  span: S.Span,
): C.CExpr => {
  // For empty list, use the original nodeId; otherwise all nodes are synthetic
  if (elements.length === 0) {
    return C.ccon(nodeId, "Nil", span);
  }
  // Build list from right to left - all synthetic nodes
  let result: C.CExpr = C.ccon(freshNodeId(state), "Nil", span);
  for (let i = elements.length - 1; i >= 0; i--) {
    const elem = elements[i]!;
    result = C.capp(
      freshNodeId(state),
      C.capp(
        freshNodeId(state),
        C.ccon(freshNodeId(state), "Cons", span),
        desugarExpr(state, elem),
        elem.span,
      ),
      result,
      span,
    );
  }
  return result;
};

const desugarCase = (state: DesugarState, c: S.SCase): C.CCase => ({
  pattern: desugarPattern(state, c.pattern),
  guard: c.guard ? desugarExpr(state, c.guard) : null,
  body: desugarExpr(state, c.body),
});

// =============================================================================
// Do-Notation Desugaring (Section 6.2)
// =============================================================================

const desugarDo = (state: DesugarState, stmts: readonly S.SDoStmt[], span: S.Span): C.CExpr => {
  if (stmts.length === 0) {
    // Empty do block - should have been caught by parser
    // Synthetic node
    return C.clit(freshNodeId(state), { kind: "int", value: 0 }, span);
  }

  const lastStmt = stmts[stmts.length - 1]!;

  // D_do⟦ [DoExpr e] ⟧ = D⟦e⟧
  if (stmts.length === 1 && lastStmt.kind === "DoExpr") {
    return desugarExpr(state, lastStmt.expr);
  }

  // Desugar from right to left
  let result =
    lastStmt.kind === "DoExpr"
      ? desugarExpr(state, lastStmt.expr)
      : C.clit(freshNodeId(state), { kind: "int", value: 0 }, span); // Shouldn't happen

  for (let i = stmts.length - 2; i >= 0; i--) {
    const stmt = stmts[i]!;
    const stmtSpan = stmt.expr.span;

    switch (stmt.kind) {
      case "DoBindPattern": {
        // D_do⟦ DoBindPattern (SPVar x) e : rest ⟧
        //   = CApp (CApp (CVar "flatMap") (CAbs x (D_do⟦rest⟧))) (D⟦e⟧)
        // All nodes in do-notation desugaring are synthetic
        if (stmt.pattern.kind === "SPVar") {
          result = C.capp(
            freshNodeId(state),
            C.capp(
              freshNodeId(state),
              C.cvar(freshNodeId(state), unresolvedName("flatMap", stmtSpan), stmtSpan),
              C.cabs(
                freshNodeId(state),
                unresolvedName(stmt.pattern.name.text, stmt.pattern.name.span),
                result,
                stmtSpan,
              ),
              stmtSpan,
            ),
            desugarExpr(state, stmt.expr),
            stmtSpan,
          );
        } else {
          // D_do⟦ DoBindPattern p e : rest ⟧
          //   = CApp (CApp (CVar "flatMap")
          //          (CAbs $tmp (CMatch (CVar $tmp) [Case (D_pat⟦p⟧) Nothing (D_do⟦rest⟧)])))
          //     (D⟦e⟧)
          const tmp = freshTemp();
          result = C.capp(
            freshNodeId(state),
            C.capp(
              freshNodeId(state),
              C.cvar(freshNodeId(state), unresolvedName("flatMap", stmtSpan), stmtSpan),
              C.cabs(
                freshNodeId(state),
                unresolvedName(tmp, stmtSpan),
                C.cmatch(
                  freshNodeId(state),
                  C.cvar(freshNodeId(state), unresolvedName(tmp, stmtSpan), stmtSpan),
                  [{ pattern: desugarPattern(state, stmt.pattern), guard: null, body: result }],
                  stmtSpan,
                ),
                stmtSpan,
              ),
              stmtSpan,
            ),
            desugarExpr(state, stmt.expr),
            stmtSpan,
          );
        }
        break;
      }

      case "DoLet": {
        // D_do⟦ DoLet (SPVar x) e : rest ⟧ = CLet x (D⟦e⟧) (D_do⟦rest⟧)
        // All nodes in do-notation desugaring are synthetic
        if (stmt.pattern.kind === "SPVar") {
          result = C.clet(
            freshNodeId(state),
            unresolvedName(stmt.pattern.name.text, stmt.pattern.name.span),
            desugarExpr(state, stmt.expr),
            result,
            stmtSpan,
          );
        } else {
          // D_do⟦ DoLet p e : rest ⟧ = CMatch (D⟦e⟧) [Case (D_pat⟦p⟧) Nothing (D_do⟦rest⟧)]
          result = C.cmatch(
            freshNodeId(state),
            desugarExpr(state, stmt.expr),
            [{ pattern: desugarPattern(state, stmt.pattern), guard: null, body: result }],
            stmtSpan,
          );
        }
        break;
      }

      case "DoExpr": {
        // D_do⟦ DoExpr e : rest ⟧ = CApp (CApp (CVar "flatMap") (CAbs _ (D_do⟦rest⟧))) (D⟦e⟧)
        // All nodes in do-notation desugaring are synthetic
        const tmp = freshTemp();
        result = C.capp(
          freshNodeId(state),
          C.capp(
            freshNodeId(state),
            C.cvar(freshNodeId(state), unresolvedName("flatMap", stmtSpan), stmtSpan),
            C.cabs(freshNodeId(state), unresolvedName(tmp, stmtSpan), result, stmtSpan),
            stmtSpan,
          ),
          desugarExpr(state, stmt.expr),
          stmtSpan,
        );
        break;
      }
    }
  }

  return result;
};

// =============================================================================
// Pattern Desugaring (Section 6.3)
// =============================================================================

/**
 * Collect all variable names bound by a pattern.
 * Used for shadowing module names in guard and body expressions.
 */
const collectPatternBindings = (pattern: S.SPattern): Set<string> => {
  const bindings = new Set<string>();

  const collect = (p: S.SPattern): void => {
    switch (p.kind) {
      case "SPWild":
      case "SPLit":
        break;
      case "SPVar":
        bindings.add(p.name.text);
        break;
      case "SPCon":
        for (const arg of p.args) collect(arg);
        break;
      case "SPTuple":
        for (const elem of p.elements) collect(elem);
        break;
      case "SPRecord":
        for (const f of p.fields) collect(f.pattern);
        break;
      case "SPAs":
        bindings.add(p.name.text);
        collect(p.pattern);
        break;
      case "SPOr":
        // For or-patterns, both branches bind the same variables
        collect(p.left);
        break;
      case "SPCons":
        collect(p.head);
        collect(p.tail);
        break;
      case "SPList":
        for (const elem of p.elements) collect(elem);
        break;
    }
  };

  collect(pattern);
  return bindings;
};

const desugarPattern = (state: DesugarState, pattern: S.SPattern): C.CPattern => {
  switch (pattern.kind) {
    case "SPWild":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cpwild(pattern.nodeId, pattern.span);

    case "SPVar":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cpvar(
        pattern.nodeId,
        unresolvedName(pattern.name.text, pattern.name.span),
        pattern.span,
      );

    case "SPLit":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cplit(pattern.nodeId, pattern.value, pattern.span);

    case "SPCon":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cpcon(
        pattern.nodeId,
        pattern.name,
        pattern.args.map((p) => desugarPattern(state, p)),
        pattern.span,
      );

    case "SPTuple":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cptuple(
        pattern.nodeId,
        pattern.elements.map((p) => desugarPattern(state, p)),
        pattern.span,
      );

    case "SPRecord":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cprecord(
        pattern.nodeId,
        pattern.fields.map((f) => ({ name: f.name, pattern: desugarPattern(state, f.pattern) })),
        pattern.span,
      );

    case "SPAs":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cpas(
        pattern.nodeId,
        unresolvedName(pattern.name.text, pattern.name.span),
        desugarPattern(state, pattern.pattern),
        pattern.span,
      );

    case "SPOr":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cpor(
        pattern.nodeId,
        desugarPattern(state, pattern.left),
        desugarPattern(state, pattern.right),
        pattern.span,
      );

    case "SPCons":
      // D_pat⟦ SPCons p1 p2 ⟧ = CPCon "Cons" [D_pat⟦p1⟧, D_pat⟦p2⟧]
      // Synthetic transformation: becomes CPCon, use fresh nodeId
      return C.cpcon(
        freshNodeId(state),
        "Cons",
        [desugarPattern(state, pattern.head), desugarPattern(state, pattern.tail)],
        pattern.span,
      );

    case "SPList":
      // D_pat⟦ SPList [] ⟧ = CPCon "Nil" []
      // D_pat⟦ SPList (p:ps) ⟧ = CPCon "Cons" [D_pat⟦p⟧, D_pat⟦ SPList ps ⟧]
      return desugarListPattern(state, pattern.nodeId, pattern.elements, pattern.span);
  }
};

const desugarListPattern = (
  state: DesugarState,
  nodeId: NodeId,
  elements: readonly S.SPattern[],
  span: S.Span,
): C.CPattern => {
  if (elements.length === 0) {
    // For empty list pattern, use the original nodeId
    return C.cpcon(nodeId, "Nil", [], span);
  }

  // All synthetic nodes for non-empty list
  let result: C.CPattern = C.cpcon(freshNodeId(state), "Nil", [], span);
  for (let i = elements.length - 1; i >= 0; i--) {
    result = C.cpcon(
      freshNodeId(state),
      "Cons",
      [desugarPattern(state, elements[i]!), result],
      span,
    );
  }
  return result;
};

/**
 * Desugar pattern with module context - qualifies imported constructor names.
 */
const desugarPatternInModuleWithImports = (
  state: DesugarState,
  pattern: S.SPattern,
  moduleName: string,
  moduleNames: Set<string>,
  importedNames: Map<string, string>,
): C.CPattern => {
  const recurse = (p: S.SPattern): C.CPattern =>
    desugarPatternInModuleWithImports(state, p, moduleName, moduleNames, importedNames);

  /**
   * Qualify a constructor name if it's defined in current module or imported.
   */
  const qualifyConstructor = (name: string): string => {
    // First check if defined in current module
    if (moduleNames.has(name)) {
      return `${moduleName}.${name}`;
    }
    // Then check if imported via `use`
    const sourceModule = importedNames.get(name);
    if (sourceModule) {
      return `${sourceModule}.${name}`;
    }
    // Otherwise leave unqualified
    return name;
  };

  switch (pattern.kind) {
    case "SPWild":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cpwild(pattern.nodeId, pattern.span);

    case "SPVar":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cpvar(
        pattern.nodeId,
        unresolvedName(pattern.name.text, pattern.name.span),
        pattern.span,
      );

    case "SPLit":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cplit(pattern.nodeId, pattern.value, pattern.span);

    case "SPCon":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cpcon(
        pattern.nodeId,
        qualifyConstructor(pattern.name),
        pattern.args.map(recurse),
        pattern.span,
      );

    case "SPTuple":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cptuple(pattern.nodeId, pattern.elements.map(recurse), pattern.span);

    case "SPRecord":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cprecord(
        pattern.nodeId,
        pattern.fields.map((f) => ({ name: f.name, pattern: recurse(f.pattern) })),
        pattern.span,
      );

    case "SPAs":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cpas(
        pattern.nodeId,
        unresolvedName(pattern.name.text, pattern.name.span),
        recurse(pattern.pattern),
        pattern.span,
      );

    case "SPOr":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cpor(pattern.nodeId, recurse(pattern.left), recurse(pattern.right), pattern.span);

    case "SPCons":
      // Cons is from prelude, not qualified
      // Synthetic transformation: becomes CPCon, use fresh nodeId
      return C.cpcon(
        freshNodeId(state),
        "Cons",
        [recurse(pattern.head), recurse(pattern.tail)],
        pattern.span,
      );

    case "SPList": {
      // Nil/Cons are from prelude, not qualified
      if (pattern.elements.length === 0) {
        // For empty list pattern, use the original nodeId
        return C.cpcon(pattern.nodeId, "Nil", [], pattern.span);
      }
      // All synthetic nodes for non-empty list
      let result: C.CPattern = C.cpcon(freshNodeId(state), "Nil", [], pattern.span);
      for (let i = pattern.elements.length - 1; i >= 0; i--) {
        result = C.cpcon(
          freshNodeId(state),
          "Cons",
          [recurse(pattern.elements[i]!), result],
          pattern.span,
        );
      }
      return result;
    }
  }
};

// =============================================================================
// Type Desugaring (Surface Type → Core Type)
// =============================================================================

const desugarType = (type: S.SType): C.CType => {
  switch (type.kind) {
    case "STVar":
      return C.ctvar(type.name);
    case "STCon":
      return C.ctcon(type.name);
    case "STApp":
      return C.ctapp(desugarType(type.func), desugarType(type.arg));
    case "STFun":
      return C.ctfun(desugarType(type.param), desugarType(type.result));
    case "STTuple":
      return C.cttuple(type.elements.map(desugarType));
    case "STRecord":
      return C.ctrecord(type.fields.map((f) => ({ name: f.name, type: desugarType(f.type) })));
  }
};

// =============================================================================
// Declaration Desugaring
// =============================================================================

const desugarDecl = (state: DesugarState, decl: S.SDecl): C.CDecl | C.CDecl[] | null => {
  switch (decl.kind) {
    case "SDeclType":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cdecltype(
        decl.nodeId,
        decl.name,
        decl.params,
        decl.constructors.map((c) =>
          C.ccondecl(c.nodeId, c.name, c.fields.map(desugarType), c.span),
        ),
        decl.span,
      );

    case "SDeclTypeAlias":
      // Type aliases are handled during type checking, not in Core AST
      // For now, skip them
      return null;

    case "SDeclLet":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cdecllet(
        decl.nodeId,
        unresolvedName(decl.name.text, decl.name.span),
        desugarExpr(state, decl.value),
        decl.span,
      );

    case "SDeclLetRec":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cdeclletrec(
        decl.nodeId,
        decl.bindings.map((b) => ({
          name: unresolvedName(b.name.text, b.name.span),
          value: desugarExpr(state, b.value),
        })),
        decl.span,
      );

    case "SDeclForeign":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cdeclforeign(
        decl.nodeId,
        unresolvedName(decl.name.text, decl.name.span),
        "", // module - to be filled during resolution
        decl.name.text, // jsName - same as name by default
        desugarType(decl.type),
        decl.isAsync,
        decl.span,
      );

    case "SDeclUse": {
      // use Foo (bar, baz) → creates aliases from qualified names
      // let bar = Foo.bar, let baz = Foo.baz
      // Constructors (uppercase) use CCon, functions (lowercase) use CVar
      if (decl.imports === "all") {
        // "all" imports require knowing module exports at this point
        // This will be handled by desugarDeclWithModuleInfo
        return null;
      }

      // Create let declarations for each imported name
      // All synthetic declarations
      const bindings: C.CDecl[] = [];
      for (const name of decl.imports) {
        const qualifiedName = `${decl.module}.${name}`;
        const isConstructor = name[0] === name[0]?.toUpperCase();
        bindings.push(
          C.cdecllet(
            freshNodeId(state),
            unresolvedName(name, decl.span),
            isConstructor
              ? C.ccon(freshNodeId(state), qualifiedName, decl.span)
              : C.cvar(freshNodeId(state), unresolvedName(qualifiedName, decl.span), decl.span),
            decl.span,
          ),
        );
      }
      return bindings;
    }

    case "SDeclModule": {
      // Operators module is special - don't qualify operator names
      if (decl.name === "Operators") {
        const innerDecls: C.CDecl[] = [];
        for (const d of decl.decls) {
          const result = desugarDecl(state, d);
          if (result === null) continue;
          if (Array.isArray(result)) {
            innerDecls.push(...result);
          } else {
            innerDecls.push(result);
          }
        }
        return innerDecls;
      }

      // Collect all names defined in this module for qualified reference resolution
      const moduleNames = collectModuleNames(decl.decls);
      // Flatten module declarations, setting module context for foreign declarations
      const innerDecls: C.CDecl[] = [];
      for (const d of decl.decls) {
        const result = desugarDeclInModule(state, d, decl.name, moduleNames);
        if (result === null) continue;
        if (Array.isArray(result)) {
          innerDecls.push(...result);
        } else {
          innerDecls.push(result);
        }
      }
      return innerDecls;
    }
  }
};

/**
 * Collect all names defined in a module for qualified reference resolution.
 * This includes function names AND constructor names from type declarations.
 */
const collectModuleNames = (decls: readonly S.SDecl[]): Set<string> => {
  const names = new Set<string>();
  for (const decl of decls) {
    switch (decl.kind) {
      case "SDeclType":
        // Collect constructor names from type declarations
        for (const con of decl.constructors) {
          names.add(con.name);
        }
        break;
      case "SDeclForeign":
        names.add(decl.name.text);
        break;
      case "SDeclLet":
        names.add(decl.name.text);
        break;
      case "SDeclLetRec":
        for (const b of decl.bindings) {
          names.add(b.name.text);
        }
        break;
    }
  }
  return names;
};

/**
 * Desugar expression with module context and imported names.
 * - If name is defined in current module → qualify with current module
 * - If name is imported via `use` → qualify with source module
 * - Otherwise → leave unqualified
 */
const desugarExprInModuleWithImports = (
  state: DesugarState,
  expr: S.SExpr,
  moduleName: string,
  moduleNames: Set<string>,
  importedNames: Map<string, string>,
): C.CExpr => {
  // Helper to recursively call with same context
  const recurse = (e: S.SExpr): C.CExpr =>
    desugarExprInModuleWithImports(state, e, moduleName, moduleNames, importedNames);

  switch (expr.kind) {
    case "SVar": {
      // First check if defined in current module
      // Direct mapping: preserve nodeId from Surface AST
      if (moduleNames.has(expr.name.text)) {
        return C.cvar(
          expr.nodeId,
          unresolvedName(`${moduleName}.${expr.name.text}`, expr.name.span),
          expr.span,
        );
      }
      // Then check if imported via `use`
      const sourceModule = importedNames.get(expr.name.text);
      if (sourceModule) {
        return C.cvar(
          expr.nodeId,
          unresolvedName(`${sourceModule}.${expr.name.text}`, expr.name.span),
          expr.span,
        );
      }
      // Otherwise leave unqualified
      return C.cvar(expr.nodeId, unresolvedName(expr.name.text, expr.name.span), expr.span);
    }

    case "SLit":
      // Direct mapping: preserve nodeId from Surface AST
      return C.clit(expr.nodeId, expr.value, expr.span);

    case "SApp":
      // Direct mapping: preserve nodeId from Surface AST
      return C.capp(expr.nodeId, recurse(expr.func), recurse(expr.arg), expr.span);

    case "SAbs":
      return desugarAbsInModuleWithImports(
        state,
        expr.nodeId,
        expr.params,
        expr.body,
        moduleName,
        moduleNames,
        importedNames,
        expr.span,
      );

    case "SLet": {
      // The bound name shadows module names in the body
      const shadowedModuleNames = new Set(moduleNames);
      const shadowedImportedNames = new Map(importedNames);
      shadowedModuleNames.delete(expr.name.text);
      shadowedImportedNames.delete(expr.name.text);
      // Direct mapping: preserve nodeId from Surface AST
      return C.clet(
        expr.nodeId,
        unresolvedName(expr.name.text, expr.name.span),
        recurse(expr.value),
        desugarExprInModuleWithImports(
          state,
          expr.body,
          moduleName,
          shadowedModuleNames,
          shadowedImportedNames,
        ),
        expr.span,
      );
    }

    case "SLetRec": {
      // All bound names shadow in all values and body
      const shadowedModuleNames = new Set(moduleNames);
      const shadowedImportedNames = new Map(importedNames);
      for (const b of expr.bindings) {
        shadowedModuleNames.delete(b.name.text);
        shadowedImportedNames.delete(b.name.text);
      }
      const recurseWithShadow = (e: S.SExpr): C.CExpr =>
        desugarExprInModuleWithImports(
          state,
          e,
          moduleName,
          shadowedModuleNames,
          shadowedImportedNames,
        );
      // Direct mapping: preserve nodeId from Surface AST
      return C.cletrec(
        expr.nodeId,
        expr.bindings.map((b) => ({
          name: unresolvedName(b.name.text, b.name.span),
          value: recurseWithShadow(b.value),
        })),
        recurseWithShadow(expr.body),
        expr.span,
      );
    }

    case "SIf":
      // Synthetic transformation: SIf becomes CMatch, use fresh nodeIds
      return C.cmatch(
        freshNodeId(state),
        recurse(expr.cond),
        [
          {
            pattern: C.cplit(freshNodeId(state), { kind: "bool", value: true }, expr.cond.span),
            guard: null,
            body: recurse(expr.thenBranch),
          },
          {
            pattern: C.cplit(freshNodeId(state), { kind: "bool", value: false }, expr.cond.span),
            guard: null,
            body: recurse(expr.elseBranch),
          },
        ],
        expr.span,
      );

    case "SMatch":
      // Direct mapping: preserve nodeId from Surface AST
      return C.cmatch(
        expr.nodeId,
        recurse(expr.scrutinee),
        expr.cases.map((c) => {
          // Pattern bindings shadow module names in guard and body
          const patternBindings = collectPatternBindings(c.pattern);
          const shadowedModuleNames = new Set(moduleNames);
          const shadowedImportedNames = new Map(importedNames);
          for (const name of patternBindings) {
            shadowedModuleNames.delete(name);
            shadowedImportedNames.delete(name);
          }
          const recurseWithShadow = (e: S.SExpr): C.CExpr =>
            desugarExprInModuleWithImports(
              state,
              e,
              moduleName,
              shadowedModuleNames,
              shadowedImportedNames,
            );
          return {
            pattern: desugarPatternInModuleWithImports(
              state,
              c.pattern,
              moduleName,
              moduleNames,
              importedNames,
            ),
            guard: c.guard ? recurseWithShadow(c.guard) : null,
            body: recurseWithShadow(c.body),
          };
        }),
        expr.span,
      );

    case "SCon": {
      // Qualify constructor if it's from current module or imported
      // Direct mapping: preserve nodeId from Surface AST
      if (moduleNames.has(expr.name)) {
        return C.ccon(expr.nodeId, `${moduleName}.${expr.name}`, expr.span);
      }
      const sourceModule = importedNames.get(expr.name);
      if (sourceModule) {
        return C.ccon(expr.nodeId, `${sourceModule}.${expr.name}`, expr.span);
      }
      return C.ccon(expr.nodeId, expr.name, expr.span);
    }

    case "STuple":
      // Direct mapping: preserve nodeId from Surface AST
      return C.ctuple(expr.nodeId, expr.elements.map(recurse), expr.span);

    case "SRecord":
      // Direct mapping: preserve nodeId from Surface AST
      return C.crecord(
        expr.nodeId,
        expr.fields.map((f) => ({ name: f.name, value: recurse(f.value) })),
        expr.span,
      );

    case "SRecordUpdate":
      // Direct mapping: preserve nodeId from Surface AST
      return C.crecordUpdate(
        expr.nodeId,
        recurse(expr.record),
        expr.fields.map((f) => ({ name: f.name, value: recurse(f.value) })),
        expr.span,
      );

    case "SField":
      // Check if this is a qualified module reference (Module.name)
      if (expr.record.kind === "SCon") {
        const qualifiedName = `${expr.record.name}.${expr.field}`;
        // If field starts with uppercase, it's a constructor (Module.Constructor)
        // Otherwise it's a function/variable (Module.func)
        if (expr.field[0] && expr.field[0] === expr.field[0].toUpperCase()) {
          // Direct mapping: preserve nodeId from Surface AST
          return C.ccon(expr.nodeId, qualifiedName, expr.span);
        }
        // Pass module span (from the constructor) and member span (from the field)
        // Direct mapping: preserve nodeId from Surface AST
        return C.cvar(
          expr.nodeId,
          unresolvedName(qualifiedName, expr.fieldSpan),
          expr.span,
          expr.record.span,
          expr.fieldSpan,
        );
      }
      // Direct mapping: preserve nodeId from Surface AST
      return C.cfield(expr.nodeId, recurse(expr.record), expr.field, expr.span);

    case "SList":
      return desugarListInModuleWithImports(
        state,
        expr.nodeId,
        expr.elements,
        moduleName,
        moduleNames,
        importedNames,
        expr.span,
      );

    case "SPipe":
      // Synthetic transformation: becomes CApp with swapped args, use fresh nodeId
      return C.capp(freshNodeId(state), recurse(expr.right), recurse(expr.left), expr.span);

    case "SCons":
      // Synthetic transformation: becomes nested CApp with CCon, use fresh nodeIds
      return C.capp(
        freshNodeId(state),
        C.capp(
          freshNodeId(state),
          C.ccon(freshNodeId(state), "Cons", expr.span),
          recurse(expr.head),
          expr.span,
        ),
        recurse(expr.tail),
        expr.span,
      );

    case "SBinOp":
      if (expr.op === "&&") {
        // Synthetic transformation: becomes CMatch, use fresh nodeIds
        return C.cmatch(
          freshNodeId(state),
          recurse(expr.left),
          [
            {
              pattern: C.cplit(freshNodeId(state), { kind: "bool", value: true }, expr.left.span),
              guard: null,
              body: recurse(expr.right),
            },
            {
              pattern: C.cplit(freshNodeId(state), { kind: "bool", value: false }, expr.left.span),
              guard: null,
              body: C.clit(freshNodeId(state), { kind: "bool", value: false }, expr.span),
            },
          ],
          expr.span,
        );
      }
      if (expr.op === "||") {
        // Synthetic transformation: becomes CMatch, use fresh nodeIds
        return C.cmatch(
          freshNodeId(state),
          recurse(expr.left),
          [
            {
              pattern: C.cplit(freshNodeId(state), { kind: "bool", value: true }, expr.left.span),
              guard: null,
              body: C.clit(freshNodeId(state), { kind: "bool", value: true }, expr.span),
            },
            {
              pattern: C.cplit(freshNodeId(state), { kind: "bool", value: false }, expr.left.span),
              guard: null,
              body: recurse(expr.right),
            },
          ],
          expr.span,
        );
      }
      // Keep binary operators as direct operations (not function calls)
      // Direct mapping: preserve nodeId from Surface AST
      return C.cbinop(expr.nodeId, expr.op, recurse(expr.left), recurse(expr.right), expr.span);

    case "SDo":
      return desugarDoInModuleWithImports(
        state,
        expr.stmts,
        moduleName,
        moduleNames,
        importedNames,
        expr.span,
      );

    case "SAnnot":
      return recurse(expr.expr);
  }
};

const desugarAbsInModuleWithImports = (
  state: DesugarState,
  nodeId: NodeId,
  params: readonly S.SParam[],
  body: S.SExpr,
  moduleName: string,
  moduleNames: Set<string>,
  importedNames: Map<string, string>,
  span: S.Span,
): C.CExpr => {
  if (params.length === 0) {
    return desugarExprInModuleWithImports(state, body, moduleName, moduleNames, importedNames);
  }

  // Parameters shadow module-level names, so remove them from moduleNames
  const shadowedModuleNames = new Set(moduleNames);
  const shadowedImportedNames = new Map(importedNames);
  for (const param of params) {
    shadowedModuleNames.delete(param.name.text);
    shadowedImportedNames.delete(param.name.text);
  }

  let result = desugarExprInModuleWithImports(
    state,
    body,
    moduleName,
    shadowedModuleNames,
    shadowedImportedNames,
  );
  for (let i = params.length - 1; i >= 0; i--) {
    const param = params[i]!;
    // For nested lambdas, use body's span; for outermost, use provided span
    const lambdaSpan = i === 0 ? span : body.span;
    // Use the original nodeId for outermost lambda, fresh for inner ones (synthetic)
    const lambdaNodeId = i === 0 ? nodeId : freshNodeId(state);
    result = C.cabs(
      lambdaNodeId,
      unresolvedName(param.name.text, param.name.span),
      result,
      lambdaSpan,
      param.name.span,
    );
  }
  return result;
};

const desugarListInModuleWithImports = (
  state: DesugarState,
  nodeId: NodeId,
  elements: readonly S.SExpr[],
  moduleName: string,
  moduleNames: Set<string>,
  importedNames: Map<string, string>,
  span: S.Span,
): C.CExpr => {
  // For empty list, use the original nodeId; otherwise all nodes are synthetic
  if (elements.length === 0) {
    return C.ccon(nodeId, "Nil", span);
  }
  // Build list from right to left - all synthetic nodes
  let result: C.CExpr = C.ccon(freshNodeId(state), "Nil", span);
  for (let i = elements.length - 1; i >= 0; i--) {
    const elem = elements[i]!;
    const elemDesugared = desugarExprInModuleWithImports(
      state,
      elem,
      moduleName,
      moduleNames,
      importedNames,
    );
    result = C.capp(
      freshNodeId(state),
      C.capp(
        freshNodeId(state),
        C.ccon(freshNodeId(state), "Cons", span),
        elemDesugared,
        elem.span,
      ),
      result,
      span,
    );
  }
  return result;
};

const desugarDoInModuleWithImports = (
  state: DesugarState,
  stmts: readonly S.SDoStmt[],
  moduleName: string,
  moduleNames: Set<string>,
  importedNames: Map<string, string>,
  span: S.Span,
): C.CExpr => {
  const recurse = (e: S.SExpr): C.CExpr =>
    desugarExprInModuleWithImports(state, e, moduleName, moduleNames, importedNames);

  if (stmts.length === 0) {
    // Synthetic node
    return C.clit(freshNodeId(state), { kind: "int", value: 0 }, span);
  }

  const lastStmt = stmts[stmts.length - 1]!;

  if (stmts.length === 1 && lastStmt.kind === "DoExpr") {
    return recurse(lastStmt.expr);
  }

  let result =
    lastStmt.kind === "DoExpr"
      ? recurse(lastStmt.expr)
      : C.clit(freshNodeId(state), { kind: "int", value: 0 }, span);

  for (let i = stmts.length - 2; i >= 0; i--) {
    const stmt = stmts[i]!;
    const stmtSpan = stmt.expr.span;

    switch (stmt.kind) {
      case "DoBindPattern": {
        // All nodes in do-notation desugaring are synthetic
        if (stmt.pattern.kind === "SPVar") {
          result = C.capp(
            freshNodeId(state),
            C.capp(
              freshNodeId(state),
              C.cvar(freshNodeId(state), unresolvedName("flatMap", stmtSpan), stmtSpan),
              C.cabs(
                freshNodeId(state),
                unresolvedName(stmt.pattern.name.text, stmt.pattern.name.span),
                result,
                stmtSpan,
              ),
              stmtSpan,
            ),
            recurse(stmt.expr),
            stmtSpan,
          );
        } else {
          const tmp = freshTemp();
          result = C.capp(
            freshNodeId(state),
            C.capp(
              freshNodeId(state),
              C.cvar(freshNodeId(state), unresolvedName("flatMap", stmtSpan), stmtSpan),
              C.cabs(
                freshNodeId(state),
                unresolvedName(tmp, stmtSpan),
                C.cmatch(
                  freshNodeId(state),
                  C.cvar(freshNodeId(state), unresolvedName(tmp, stmtSpan), stmtSpan),
                  [
                    {
                      pattern: desugarPatternInModuleWithImports(
                        state,
                        stmt.pattern,
                        moduleName,
                        moduleNames,
                        importedNames,
                      ),
                      guard: null,
                      body: result,
                    },
                  ],
                  stmtSpan,
                ),
                stmtSpan,
              ),
              stmtSpan,
            ),
            recurse(stmt.expr),
            stmtSpan,
          );
        }
        break;
      }

      case "DoLet": {
        // All nodes in do-notation desugaring are synthetic
        if (stmt.pattern.kind === "SPVar") {
          result = C.clet(
            freshNodeId(state),
            unresolvedName(stmt.pattern.name.text, stmt.pattern.name.span),
            recurse(stmt.expr),
            result,
            stmtSpan,
          );
        } else {
          result = C.cmatch(
            freshNodeId(state),
            recurse(stmt.expr),
            [
              {
                pattern: desugarPatternInModuleWithImports(
                  state,
                  stmt.pattern,
                  moduleName,
                  moduleNames,
                  importedNames,
                ),
                guard: null,
                body: result,
              },
            ],
            stmtSpan,
          );
        }
        break;
      }

      case "DoExpr": {
        // All nodes in do-notation desugaring are synthetic
        const tmp = freshTemp();
        result = C.capp(
          freshNodeId(state),
          C.capp(
            freshNodeId(state),
            C.cvar(freshNodeId(state), unresolvedName("flatMap", stmtSpan), stmtSpan),
            C.cabs(freshNodeId(state), unresolvedName(tmp, stmtSpan), result, stmtSpan),
            stmtSpan,
          ),
          recurse(stmt.expr),
          stmtSpan,
        );
        break;
      }
    }
  }

  return result;
};

/**
 * Desugar a declaration inside a module context.
 * This qualifies names and sets the module for foreign declarations.
 */
const desugarDeclInModule = (
  state: DesugarState,
  decl: S.SDecl,
  moduleName: string,
  moduleNames: Set<string>,
): C.CDecl | C.CDecl[] | null => {
  // Use empty imported names map for backward compatibility
  return desugarDeclInModuleWithImports(state, decl, moduleName, moduleNames, new Map());
};

/**
 * Desugar a declaration inside a module context with imported names.
 * This qualifies names and sets the module for foreign declarations.
 */
const desugarDeclInModuleWithImports = (
  state: DesugarState,
  decl: S.SDecl,
  moduleName: string,
  moduleNames: Set<string>,
  importedNames: Map<string, string>,
): C.CDecl | C.CDecl[] | null => {
  switch (decl.kind) {
    case "SDeclForeign": {
      // Qualify the name with module prefix (e.g., "length" in String -> "String.length")
      const qualifiedName = `${moduleName}.${decl.name.text}`;
      // Direct mapping: preserve nodeId from Surface AST
      return C.cdeclforeign(
        decl.nodeId,
        unresolvedName(qualifiedName, decl.name.span),
        moduleName,
        decl.name.text, // jsName is the unqualified name
        desugarType(decl.type),
        decl.isAsync,
        decl.span,
      );
    }

    case "SDeclLet": {
      // Qualify the name with module prefix
      const qualifiedName = `${moduleName}.${decl.name.text}`;
      // Direct mapping: preserve nodeId from Surface AST
      return C.cdecllet(
        decl.nodeId,
        unresolvedName(qualifiedName, decl.name.span),
        desugarExprInModuleWithImports(state, decl.value, moduleName, moduleNames, importedNames),
        decl.span,
      );
    }

    case "SDeclLetRec": {
      // Qualify names with module prefix
      // Direct mapping: preserve nodeId from Surface AST
      return C.cdeclletrec(
        decl.nodeId,
        decl.bindings.map((b) => ({
          name: unresolvedName(`${moduleName}.${b.name.text}`, b.name.span),
          value: desugarExprInModuleWithImports(
            state,
            b.value,
            moduleName,
            moduleNames,
            importedNames,
          ),
        })),
        decl.span,
      );
    }

    case "SDeclType": {
      // Qualify constructor names with module prefix
      // Direct mapping: preserve nodeId from Surface AST
      return C.cdecltype(
        decl.nodeId,
        decl.name,
        decl.params,
        decl.constructors.map((c) =>
          C.ccondecl(c.nodeId, `${moduleName}.${c.name}`, c.fields.map(desugarType), c.span),
        ),
        decl.span,
      );
    }

    default:
      // For other declarations (type aliases, etc.), use regular desugaring
      return desugarDecl(state, decl);
  }
};

// =============================================================================
// Module Export Collection
// =============================================================================

/**
 * Collect all module exports from the program.
 * Returns a map from module name to the set of names it exports.
 */
const collectModuleExports = (decls: readonly S.SDecl[]): Map<string, Set<string>> => {
  const exports = new Map<string, Set<string>>();

  for (const decl of decls) {
    if (decl.kind === "SDeclModule") {
      const names = collectModuleNames(decl.decls);
      exports.set(decl.name, names);
    }
  }

  return exports;
};

// =============================================================================
// Program Desugaring
// =============================================================================

export const desugarProgram = (program: S.SProgram): C.CProgram => {
  resetTempCounter();
  const state = createDesugarState();

  // First pass: collect all module exports
  const moduleExports = collectModuleExports(program.decls);

  const decls: C.CDecl[] = [];
  for (const d of program.decls) {
    const result = desugarDeclWithModuleInfo(state, d, moduleExports);
    if (result === null) continue;
    if (Array.isArray(result)) {
      decls.push(...result);
    } else {
      decls.push(result);
    }
  }

  const expr = program.expr ? desugarExpr(state, program.expr) : null;

  return { decls, expr };
};

/**
 * Desugar declaration with module export information.
 */
const desugarDeclWithModuleInfo = (
  state: DesugarState,
  decl: S.SDecl,
  moduleExports: Map<string, Set<string>>,
): C.CDecl | C.CDecl[] | null => {
  // Handle use declarations with "all" imports
  if (decl.kind === "SDeclUse") {
    if (decl.imports === "all") {
      // Import all exports from the module
      const exports = moduleExports.get(decl.module);
      if (!exports || exports.size === 0) {
        // Module not found or has no exports - skip
        return null;
      }

      // All synthetic declarations
      const bindings: C.CDecl[] = [];
      for (const name of exports) {
        const qualifiedName = `${decl.module}.${name}`;
        // Constructors (uppercase) use CCon, functions (lowercase) use CVar
        const isConstructor = name[0] === name[0]?.toUpperCase();
        bindings.push(
          C.cdecllet(
            freshNodeId(state),
            unresolvedName(name, decl.span),
            isConstructor
              ? C.ccon(freshNodeId(state), qualifiedName, decl.span)
              : C.cvar(freshNodeId(state), unresolvedName(qualifiedName, decl.span), decl.span),
            decl.span,
          ),
        );
      }
      return bindings;
    }
    // Specific imports handled by regular desugarDecl
    return desugarDecl(state, decl);
  }

  if (decl.kind !== "SDeclModule") {
    return desugarDecl(state, decl);
  }

  // Operators module is special - don't qualify operator names
  if (decl.name === "Operators") {
    const innerDecls: C.CDecl[] = [];
    for (const d of decl.decls) {
      const result = desugarDecl(state, d);
      if (result === null) continue;
      if (Array.isArray(result)) {
        innerDecls.push(...result);
      } else {
        innerDecls.push(result);
      }
    }
    return innerDecls;
  }

  // Build a map of imported names → source module from `use` statements
  const importedNames = new Map<string, string>(); // name → module
  for (const usedModule of decl.uses) {
    const exports = moduleExports.get(usedModule);
    if (exports) {
      // use Module (..) imports all exports
      for (const name of exports) {
        importedNames.set(name, usedModule);
      }
    }
  }

  // Collect names defined in this module
  const moduleNames = collectModuleNames(decl.decls);

  // Flatten module declarations
  const innerDecls: C.CDecl[] = [];
  for (const d of decl.decls) {
    const result = desugarDeclInModuleWithImports(state, d, decl.name, moduleNames, importedNames);
    if (result === null) continue;
    if (Array.isArray(result)) {
      innerDecls.push(...result);
    } else {
      innerDecls.push(result);
    }
  }
  return innerDecls;
};
