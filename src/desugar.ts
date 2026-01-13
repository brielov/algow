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
import * as C from "./core";

// Temporary name generator for intermediate bindings
let _tempCounter = 0;
const resetTempCounter = (): void => {
  _tempCounter = 0;
};
const freshTemp = (): string => `$t${_tempCounter++}`;

// =============================================================================
// Expression Desugaring (Section 6.1)
// =============================================================================

export const desugarExpr = (expr: S.SExpr): C.CExpr => {
  switch (expr.kind) {
    case "SVar":
      // D⟦ SVar x ⟧ = CVar x
      // Note: Name ID will be assigned during resolution
      return C.cvar({ id: -1, original: expr.name }, expr.span);

    case "SLit":
      // D⟦ SLit l ⟧ = CLit l
      return C.clit(expr.value, expr.span);

    case "SApp":
      // D⟦ SApp e1 e2 ⟧ = CApp (D⟦e1⟧) (D⟦e2⟧)
      return C.capp(desugarExpr(expr.func), desugarExpr(expr.arg), expr.span);

    case "SAbs":
      // D⟦ SAbs [] e ⟧ = D⟦e⟧
      // D⟦ SAbs (x:xs) e ⟧ = CAbs x (D⟦ SAbs xs e ⟧)
      return desugarAbs(expr.params, expr.body, expr.span);

    case "SLet":
      // D⟦ SLet x e1 e2 ⟧ = CLet x (D⟦e1⟧) (D⟦e2⟧)
      return C.clet(
        { id: -1, original: expr.name },
        desugarExpr(expr.value),
        desugarExpr(expr.body),
        expr.span,
        expr.nameSpan,
      );

    case "SLetRec":
      // D⟦ SLetRec bs e ⟧ = CLetRec (map (λ(n,e) -> (n, D⟦e⟧)) bs) (D⟦e⟧)
      return C.cletrec(
        expr.bindings.map((b) => ({
          name: { id: -1, original: b.name },
          value: desugarExpr(b.value),
          nameSpan: b.nameSpan,
        })),
        desugarExpr(expr.body),
        expr.span,
      );

    case "SIf":
      // D⟦ SIf c t f ⟧ = CMatch (D⟦c⟧) [
      //   Case (CPLit (LBool true)) Nothing (D⟦t⟧),
      //   Case (CPLit (LBool false)) Nothing (D⟦f⟧)
      // ]
      return C.cmatch(
        desugarExpr(expr.cond),
        [
          {
            pattern: C.cplit({ kind: "bool", value: true }),
            guard: null,
            body: desugarExpr(expr.thenBranch),
          },
          {
            pattern: C.cplit({ kind: "bool", value: false }),
            guard: null,
            body: desugarExpr(expr.elseBranch),
          },
        ],
        expr.span,
      );

    case "SMatch":
      // D⟦ SMatch e cases ⟧ = CMatch (D⟦e⟧) (map D_case cases)
      return C.cmatch(desugarExpr(expr.scrutinee), expr.cases.map(desugarCase), expr.span);

    case "SCon":
      // D⟦ SCon c ⟧ = CCon c
      return C.ccon(expr.name, expr.span);

    case "STuple":
      // D⟦ STuple es ⟧ = CTuple (map D es)
      return C.ctuple(expr.elements.map(desugarExpr), expr.span);

    case "SRecord":
      // D⟦ SRecord fs ⟧ = CRecord (map (λ(n,e) -> (n, D⟦e⟧)) fs)
      return C.crecord(
        expr.fields.map((f) => ({ name: f.name, value: desugarExpr(f.value) })),
        expr.span,
      );

    case "SRecordUpdate":
      // D⟦ SRecordUpdate r fs ⟧ = CRecordUpdate (D⟦r⟧) (map (λ(n,e) -> (n, D⟦e⟧)) fs)
      return C.crecordUpdate(
        desugarExpr(expr.record),
        expr.fields.map((f) => ({ name: f.name, value: desugarExpr(f.value) })),
        expr.span,
      );

    case "SField":
      // Check if this is a qualified module reference (Module.name)
      if (expr.record.kind === "SCon") {
        const qualifiedName = `${expr.record.name}.${expr.field}`;
        // If field starts with uppercase, it's a constructor (Module.Constructor)
        // Otherwise it's a function/variable (Module.func)
        if (expr.field[0] && expr.field[0] === expr.field[0].toUpperCase()) {
          return C.ccon(qualifiedName, expr.span);
        }
        // Pass module span (from the constructor) and member span (from the field)
        return C.cvar(
          { id: -1, original: qualifiedName },
          expr.span,
          expr.record.span,
          expr.fieldSpan,
        );
      }
      // D⟦ SField e f ⟧ = CField (D⟦e⟧) f
      return C.cfield(desugarExpr(expr.record), expr.field, expr.span);

    case "SList":
      // D⟦ SList [] ⟧ = CCon "Nil"
      // D⟦ SList (e:es) ⟧ = CApp (CApp (CCon "Cons") (D⟦e⟧)) (D⟦ SList es ⟧)
      return desugarList(expr.elements, expr.span);

    case "SPipe":
      // D⟦ SPipe e1 e2 ⟧ = CApp (D⟦e2⟧) (D⟦e1⟧)
      return C.capp(desugarExpr(expr.right), desugarExpr(expr.left), expr.span);

    case "SCons":
      // D⟦ SCons e1 e2 ⟧ = CApp (CApp (CCon "Cons") (D⟦e1⟧)) (D⟦e2⟧)
      return C.capp(
        C.capp(C.ccon("Cons"), desugarExpr(expr.head)),
        desugarExpr(expr.tail),
        expr.span,
      );

    case "SBinOp":
      // Special cases for && and || (short-circuit evaluation)
      if (expr.op === "&&") {
        // a && b → if a then b else false
        return C.cmatch(
          desugarExpr(expr.left),
          [
            {
              pattern: C.cplit({ kind: "bool", value: true }),
              guard: null,
              body: desugarExpr(expr.right),
            },
            {
              pattern: C.cplit({ kind: "bool", value: false }),
              guard: null,
              body: C.clit({ kind: "bool", value: false }),
            },
          ],
          expr.span,
        );
      }
      if (expr.op === "||") {
        // a || b → if a then true else b
        return C.cmatch(
          desugarExpr(expr.left),
          [
            {
              pattern: C.cplit({ kind: "bool", value: true }),
              guard: null,
              body: C.clit({ kind: "bool", value: true }),
            },
            {
              pattern: C.cplit({ kind: "bool", value: false }),
              guard: null,
              body: desugarExpr(expr.right),
            },
          ],
          expr.span,
        );
      }
      // Keep binary operators as direct operations (not function calls)
      return C.cbinop(expr.op, desugarExpr(expr.left), desugarExpr(expr.right), expr.span);

    case "SDo":
      // D⟦ SDo stmts ⟧ = D_do⟦stmts⟧
      return desugarDo(expr.stmts, expr.span);

    case "SAnnot":
      // D⟦ SAnnot e t ⟧ = D⟦e⟧
      // Type annotation is erased (used only during type checking)
      return desugarExpr(expr.expr);
  }
};

// =============================================================================
// Helper Functions
// =============================================================================

const desugarAbs = (params: readonly S.SParam[], body: S.SExpr, span?: S.Span): C.CExpr => {
  if (params.length === 0) {
    return desugarExpr(body);
  }

  // Build nested lambdas from right to left
  let result = desugarExpr(body);
  for (let i = params.length - 1; i >= 0; i--) {
    const param = params[i]!;
    result = C.cabs(
      { id: -1, original: param.name },
      result,
      i === 0 ? span : undefined,
      param.span,
    );
  }
  return result;
};

const desugarList = (elements: readonly S.SExpr[], span?: S.Span): C.CExpr => {
  let result: C.CExpr = C.ccon("Nil", span);
  for (let i = elements.length - 1; i >= 0; i--) {
    result = C.capp(C.capp(C.ccon("Cons"), desugarExpr(elements[i]!)), result);
  }
  return result;
};

const desugarCase = (c: S.SCase): C.CCase => ({
  pattern: desugarPattern(c.pattern),
  guard: c.guard ? desugarExpr(c.guard) : null,
  body: desugarExpr(c.body),
});

// =============================================================================
// Do-Notation Desugaring (Section 6.2)
// =============================================================================

const desugarDo = (stmts: readonly S.SDoStmt[], span?: S.Span): C.CExpr => {
  if (stmts.length === 0) {
    // Empty do block - should have been caught by parser
    return C.clit({ kind: "int", value: 0 }, span);
  }

  const lastStmt = stmts[stmts.length - 1]!;

  // D_do⟦ [DoExpr e] ⟧ = D⟦e⟧
  if (stmts.length === 1 && lastStmt.kind === "DoExpr") {
    return desugarExpr(lastStmt.expr);
  }

  // Desugar from right to left
  let result =
    lastStmt.kind === "DoExpr" ? desugarExpr(lastStmt.expr) : C.clit({ kind: "int", value: 0 }); // Shouldn't happen

  for (let i = stmts.length - 2; i >= 0; i--) {
    const stmt = stmts[i]!;

    switch (stmt.kind) {
      case "DoBindPattern": {
        // D_do⟦ DoBindPattern (SPVar x) e : rest ⟧
        //   = CApp (CApp (CVar "flatMap") (CAbs x (D_do⟦rest⟧))) (D⟦e⟧)
        if (stmt.pattern.kind === "SPVar") {
          result = C.capp(
            C.capp(
              C.cvar({ id: -1, original: "flatMap" }),
              C.cabs({ id: -1, original: stmt.pattern.name }, result),
            ),
            desugarExpr(stmt.expr),
          );
        } else {
          // D_do⟦ DoBindPattern p e : rest ⟧
          //   = CApp (CApp (CVar "flatMap")
          //          (CAbs $tmp (CMatch (CVar $tmp) [Case (D_pat⟦p⟧) Nothing (D_do⟦rest⟧)])))
          //     (D⟦e⟧)
          const tmp = freshTemp();
          result = C.capp(
            C.capp(
              C.cvar({ id: -1, original: "flatMap" }),
              C.cabs(
                { id: -1, original: tmp },
                C.cmatch(C.cvar({ id: -1, original: tmp }), [
                  { pattern: desugarPattern(stmt.pattern), guard: null, body: result },
                ]),
              ),
            ),
            desugarExpr(stmt.expr),
          );
        }
        break;
      }

      case "DoLet": {
        // D_do⟦ DoLet (SPVar x) e : rest ⟧ = CLet x (D⟦e⟧) (D_do⟦rest⟧)
        if (stmt.pattern.kind === "SPVar") {
          result = C.clet({ id: -1, original: stmt.pattern.name }, desugarExpr(stmt.expr), result);
        } else {
          // D_do⟦ DoLet p e : rest ⟧ = CMatch (D⟦e⟧) [Case (D_pat⟦p⟧) Nothing (D_do⟦rest⟧)]
          result = C.cmatch(desugarExpr(stmt.expr), [
            { pattern: desugarPattern(stmt.pattern), guard: null, body: result },
          ]);
        }
        break;
      }

      case "DoExpr": {
        // D_do⟦ DoExpr e : rest ⟧ = CApp (CApp (CVar "flatMap") (CAbs _ (D_do⟦rest⟧))) (D⟦e⟧)
        const tmp = freshTemp();
        result = C.capp(
          C.capp(
            C.cvar({ id: -1, original: "flatMap" }),
            C.cabs({ id: -1, original: tmp }, result),
          ),
          desugarExpr(stmt.expr),
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
        bindings.add(p.name);
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
        bindings.add(p.name);
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

const desugarPattern = (pattern: S.SPattern): C.CPattern => {
  switch (pattern.kind) {
    case "SPWild":
      return C.cpwild(pattern.span);

    case "SPVar":
      return C.cpvar({ id: -1, original: pattern.name }, pattern.span);

    case "SPLit":
      return C.cplit(pattern.value, pattern.span);

    case "SPCon":
      return C.cpcon(pattern.name, pattern.args.map(desugarPattern), pattern.span);

    case "SPTuple":
      return C.cptuple(pattern.elements.map(desugarPattern), pattern.span);

    case "SPRecord":
      return C.cprecord(
        pattern.fields.map((f) => ({ name: f.name, pattern: desugarPattern(f.pattern) })),
        pattern.span,
      );

    case "SPAs":
      return C.cpas(
        { id: -1, original: pattern.name },
        desugarPattern(pattern.pattern),
        pattern.span,
      );

    case "SPOr":
      return C.cpor(desugarPattern(pattern.left), desugarPattern(pattern.right), pattern.span);

    case "SPCons":
      // D_pat⟦ SPCons p1 p2 ⟧ = CPCon "Cons" [D_pat⟦p1⟧, D_pat⟦p2⟧]
      return C.cpcon(
        "Cons",
        [desugarPattern(pattern.head), desugarPattern(pattern.tail)],
        pattern.span,
      );

    case "SPList":
      // D_pat⟦ SPList [] ⟧ = CPCon "Nil" []
      // D_pat⟦ SPList (p:ps) ⟧ = CPCon "Cons" [D_pat⟦p⟧, D_pat⟦ SPList ps ⟧]
      return desugarListPattern(pattern.elements, pattern.span);
  }
};

const desugarListPattern = (elements: readonly S.SPattern[], span?: S.Span): C.CPattern => {
  if (elements.length === 0) {
    return C.cpcon("Nil", [], span);
  }

  let result: C.CPattern = C.cpcon("Nil", []);
  for (let i = elements.length - 1; i >= 0; i--) {
    result = C.cpcon("Cons", [desugarPattern(elements[i]!), result]);
  }
  return result;
};

/**
 * Desugar pattern with module context - qualifies imported constructor names.
 */
const desugarPatternInModuleWithImports = (
  pattern: S.SPattern,
  moduleName: string,
  moduleNames: Set<string>,
  importedNames: Map<string, string>,
): C.CPattern => {
  const recurse = (p: S.SPattern): C.CPattern =>
    desugarPatternInModuleWithImports(p, moduleName, moduleNames, importedNames);

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
      return C.cpwild(pattern.span);

    case "SPVar":
      return C.cpvar({ id: -1, original: pattern.name }, pattern.span);

    case "SPLit":
      return C.cplit(pattern.value, pattern.span);

    case "SPCon":
      return C.cpcon(qualifyConstructor(pattern.name), pattern.args.map(recurse), pattern.span);

    case "SPTuple":
      return C.cptuple(pattern.elements.map(recurse), pattern.span);

    case "SPRecord":
      return C.cprecord(
        pattern.fields.map((f) => ({ name: f.name, pattern: recurse(f.pattern) })),
        pattern.span,
      );

    case "SPAs":
      return C.cpas({ id: -1, original: pattern.name }, recurse(pattern.pattern), pattern.span);

    case "SPOr":
      return C.cpor(recurse(pattern.left), recurse(pattern.right), pattern.span);

    case "SPCons":
      // Cons is from prelude, not qualified
      return C.cpcon("Cons", [recurse(pattern.head), recurse(pattern.tail)], pattern.span);

    case "SPList": {
      // Nil/Cons are from prelude, not qualified
      if (pattern.elements.length === 0) {
        return C.cpcon("Nil", [], pattern.span);
      }
      let result: C.CPattern = C.cpcon("Nil", []);
      for (let i = pattern.elements.length - 1; i >= 0; i--) {
        result = C.cpcon("Cons", [recurse(pattern.elements[i]!), result]);
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

const desugarDecl = (decl: S.SDecl): C.CDecl | C.CDecl[] | null => {
  switch (decl.kind) {
    case "SDeclType":
      return C.cdecltype(
        decl.name,
        decl.params,
        decl.constructors.map((c) => ({
          name: c.name,
          fields: c.fields.map(desugarType),
          span: c.span,
        })),
        decl.span,
      );

    case "SDeclTypeAlias":
      // Type aliases are handled during type checking, not in Core AST
      // For now, skip them
      return null;

    case "SDeclLet":
      return C.cdecllet(
        { id: -1, original: decl.name },
        desugarExpr(decl.value),
        decl.span,
        decl.nameSpan,
      );

    case "SDeclLetRec":
      return C.cdeclletrec(
        decl.bindings.map((b) => ({
          name: { id: -1, original: b.name },
          value: desugarExpr(b.value),
          nameSpan: b.nameSpan,
        })),
        decl.span,
      );

    case "SDeclForeign":
      return C.cdeclforeign(
        { id: -1, original: decl.name },
        "", // module - to be filled during resolution
        decl.name, // jsName - same as name by default
        desugarType(decl.type),
        decl.isAsync,
        decl.span,
        decl.nameSpan,
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
      const bindings: C.CDecl[] = [];
      for (const name of decl.imports) {
        const qualifiedName = `${decl.module}.${name}`;
        const isConstructor = name[0] === name[0]?.toUpperCase();
        bindings.push(
          C.cdecllet(
            { id: -1, original: name },
            isConstructor
              ? C.ccon(qualifiedName, decl.span)
              : C.cvar({ id: -1, original: qualifiedName }, decl.span),
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
          const result = desugarDecl(d);
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
        const result = desugarDeclInModule(d, decl.name, moduleNames);
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
        names.add(decl.name);
        break;
      case "SDeclLet":
        names.add(decl.name);
        break;
      case "SDeclLetRec":
        for (const b of decl.bindings) {
          names.add(b.name);
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
  expr: S.SExpr,
  moduleName: string,
  moduleNames: Set<string>,
  importedNames: Map<string, string>,
): C.CExpr => {
  // Helper to recursively call with same context
  const recurse = (e: S.SExpr): C.CExpr =>
    desugarExprInModuleWithImports(e, moduleName, moduleNames, importedNames);

  switch (expr.kind) {
    case "SVar": {
      // First check if defined in current module
      if (moduleNames.has(expr.name)) {
        return C.cvar({ id: -1, original: `${moduleName}.${expr.name}` }, expr.span);
      }
      // Then check if imported via `use`
      const sourceModule = importedNames.get(expr.name);
      if (sourceModule) {
        return C.cvar({ id: -1, original: `${sourceModule}.${expr.name}` }, expr.span);
      }
      // Otherwise leave unqualified
      return C.cvar({ id: -1, original: expr.name }, expr.span);
    }

    case "SLit":
      return C.clit(expr.value, expr.span);

    case "SApp":
      return C.capp(recurse(expr.func), recurse(expr.arg), expr.span);

    case "SAbs":
      return desugarAbsInModuleWithImports(
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
      shadowedModuleNames.delete(expr.name);
      shadowedImportedNames.delete(expr.name);
      return C.clet(
        { id: -1, original: expr.name },
        recurse(expr.value),
        desugarExprInModuleWithImports(
          expr.body,
          moduleName,
          shadowedModuleNames,
          shadowedImportedNames,
        ),
        expr.span,
        expr.nameSpan,
      );
    }

    case "SLetRec": {
      // All bound names shadow in all values and body
      const shadowedModuleNames = new Set(moduleNames);
      const shadowedImportedNames = new Map(importedNames);
      for (const b of expr.bindings) {
        shadowedModuleNames.delete(b.name);
        shadowedImportedNames.delete(b.name);
      }
      const recurseWithShadow = (e: S.SExpr): C.CExpr =>
        desugarExprInModuleWithImports(e, moduleName, shadowedModuleNames, shadowedImportedNames);
      return C.cletrec(
        expr.bindings.map((b) => ({
          name: { id: -1, original: b.name },
          value: recurseWithShadow(b.value),
          nameSpan: b.nameSpan,
        })),
        recurseWithShadow(expr.body),
        expr.span,
      );
    }

    case "SIf":
      return C.cmatch(
        recurse(expr.cond),
        [
          {
            pattern: C.cplit({ kind: "bool", value: true }),
            guard: null,
            body: recurse(expr.thenBranch),
          },
          {
            pattern: C.cplit({ kind: "bool", value: false }),
            guard: null,
            body: recurse(expr.elseBranch),
          },
        ],
        expr.span,
      );

    case "SMatch":
      return C.cmatch(
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
              e,
              moduleName,
              shadowedModuleNames,
              shadowedImportedNames,
            );
          return {
            pattern: desugarPatternInModuleWithImports(
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
      if (moduleNames.has(expr.name)) {
        return C.ccon(`${moduleName}.${expr.name}`, expr.span);
      }
      const sourceModule = importedNames.get(expr.name);
      if (sourceModule) {
        return C.ccon(`${sourceModule}.${expr.name}`, expr.span);
      }
      return C.ccon(expr.name, expr.span);
    }

    case "STuple":
      return C.ctuple(expr.elements.map(recurse), expr.span);

    case "SRecord":
      return C.crecord(
        expr.fields.map((f) => ({ name: f.name, value: recurse(f.value) })),
        expr.span,
      );

    case "SRecordUpdate":
      return C.crecordUpdate(
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
          return C.ccon(qualifiedName, expr.span);
        }
        // Pass module span (from the constructor) and member span (from the field)
        return C.cvar(
          { id: -1, original: qualifiedName },
          expr.span,
          expr.record.span,
          expr.fieldSpan,
        );
      }
      return C.cfield(recurse(expr.record), expr.field, expr.span);

    case "SList":
      return desugarListInModuleWithImports(
        expr.elements,
        moduleName,
        moduleNames,
        importedNames,
        expr.span,
      );

    case "SPipe":
      return C.capp(recurse(expr.right), recurse(expr.left), expr.span);

    case "SCons":
      return C.capp(C.capp(C.ccon("Cons"), recurse(expr.head)), recurse(expr.tail), expr.span);

    case "SBinOp":
      if (expr.op === "&&") {
        return C.cmatch(
          recurse(expr.left),
          [
            {
              pattern: C.cplit({ kind: "bool", value: true }),
              guard: null,
              body: recurse(expr.right),
            },
            {
              pattern: C.cplit({ kind: "bool", value: false }),
              guard: null,
              body: C.clit({ kind: "bool", value: false }),
            },
          ],
          expr.span,
        );
      }
      if (expr.op === "||") {
        return C.cmatch(
          recurse(expr.left),
          [
            {
              pattern: C.cplit({ kind: "bool", value: true }),
              guard: null,
              body: C.clit({ kind: "bool", value: true }),
            },
            {
              pattern: C.cplit({ kind: "bool", value: false }),
              guard: null,
              body: recurse(expr.right),
            },
          ],
          expr.span,
        );
      }
      // Keep binary operators as direct operations (not function calls)
      return C.cbinop(expr.op, recurse(expr.left), recurse(expr.right), expr.span);

    case "SDo":
      return desugarDoInModuleWithImports(
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
  params: readonly S.SParam[],
  body: S.SExpr,
  moduleName: string,
  moduleNames: Set<string>,
  importedNames: Map<string, string>,
  span?: S.Span,
): C.CExpr => {
  if (params.length === 0) {
    return desugarExprInModuleWithImports(body, moduleName, moduleNames, importedNames);
  }

  // Parameters shadow module-level names, so remove them from moduleNames
  const shadowedModuleNames = new Set(moduleNames);
  const shadowedImportedNames = new Map(importedNames);
  for (const param of params) {
    shadowedModuleNames.delete(param.name);
    shadowedImportedNames.delete(param.name);
  }

  let result = desugarExprInModuleWithImports(
    body,
    moduleName,
    shadowedModuleNames,
    shadowedImportedNames,
  );
  for (let i = params.length - 1; i >= 0; i--) {
    const param = params[i]!;
    result = C.cabs(
      { id: -1, original: param.name },
      result,
      i === 0 ? span : undefined,
      param.span,
    );
  }
  return result;
};

const desugarListInModuleWithImports = (
  elements: readonly S.SExpr[],
  moduleName: string,
  moduleNames: Set<string>,
  importedNames: Map<string, string>,
  span?: S.Span,
): C.CExpr => {
  let result: C.CExpr = C.ccon("Nil", span);
  for (let i = elements.length - 1; i >= 0; i--) {
    result = C.capp(
      C.capp(
        C.ccon("Cons"),
        desugarExprInModuleWithImports(elements[i]!, moduleName, moduleNames, importedNames),
      ),
      result,
    );
  }
  return result;
};

const desugarDoInModuleWithImports = (
  stmts: readonly S.SDoStmt[],
  moduleName: string,
  moduleNames: Set<string>,
  importedNames: Map<string, string>,
  span?: S.Span,
): C.CExpr => {
  const recurse = (e: S.SExpr): C.CExpr =>
    desugarExprInModuleWithImports(e, moduleName, moduleNames, importedNames);

  if (stmts.length === 0) {
    return C.clit({ kind: "int", value: 0 }, span);
  }

  const lastStmt = stmts[stmts.length - 1]!;

  if (stmts.length === 1 && lastStmt.kind === "DoExpr") {
    return recurse(lastStmt.expr);
  }

  let result =
    lastStmt.kind === "DoExpr" ? recurse(lastStmt.expr) : C.clit({ kind: "int", value: 0 });

  for (let i = stmts.length - 2; i >= 0; i--) {
    const stmt = stmts[i]!;

    switch (stmt.kind) {
      case "DoBindPattern": {
        if (stmt.pattern.kind === "SPVar") {
          result = C.capp(
            C.capp(
              C.cvar({ id: -1, original: "flatMap" }),
              C.cabs({ id: -1, original: stmt.pattern.name }, result),
            ),
            recurse(stmt.expr),
          );
        } else {
          const tmp = freshTemp();
          result = C.capp(
            C.capp(
              C.cvar({ id: -1, original: "flatMap" }),
              C.cabs(
                { id: -1, original: tmp },
                C.cmatch(C.cvar({ id: -1, original: tmp }), [
                  {
                    pattern: desugarPatternInModuleWithImports(
                      stmt.pattern,
                      moduleName,
                      moduleNames,
                      importedNames,
                    ),
                    guard: null,
                    body: result,
                  },
                ]),
              ),
            ),
            recurse(stmt.expr),
          );
        }
        break;
      }

      case "DoLet": {
        if (stmt.pattern.kind === "SPVar") {
          result = C.clet({ id: -1, original: stmt.pattern.name }, recurse(stmt.expr), result);
        } else {
          result = C.cmatch(recurse(stmt.expr), [
            {
              pattern: desugarPatternInModuleWithImports(
                stmt.pattern,
                moduleName,
                moduleNames,
                importedNames,
              ),
              guard: null,
              body: result,
            },
          ]);
        }
        break;
      }

      case "DoExpr": {
        const tmp = freshTemp();
        result = C.capp(
          C.capp(
            C.cvar({ id: -1, original: "flatMap" }),
            C.cabs({ id: -1, original: tmp }, result),
          ),
          recurse(stmt.expr),
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
  decl: S.SDecl,
  moduleName: string,
  moduleNames: Set<string>,
): C.CDecl | C.CDecl[] | null => {
  // Use empty imported names map for backward compatibility
  return desugarDeclInModuleWithImports(decl, moduleName, moduleNames, new Map());
};

/**
 * Desugar a declaration inside a module context with imported names.
 * This qualifies names and sets the module for foreign declarations.
 */
const desugarDeclInModuleWithImports = (
  decl: S.SDecl,
  moduleName: string,
  moduleNames: Set<string>,
  importedNames: Map<string, string>,
): C.CDecl | C.CDecl[] | null => {
  switch (decl.kind) {
    case "SDeclForeign": {
      // Qualify the name with module prefix (e.g., "length" in String -> "String.length")
      const qualifiedName = `${moduleName}.${decl.name}`;
      return C.cdeclforeign(
        { id: -1, original: qualifiedName },
        moduleName,
        decl.name, // jsName is the unqualified name
        desugarType(decl.type),
        decl.isAsync,
        decl.span,
        decl.nameSpan,
      );
    }

    case "SDeclLet": {
      // Qualify the name with module prefix
      const qualifiedName = `${moduleName}.${decl.name}`;
      return C.cdecllet(
        { id: -1, original: qualifiedName },
        desugarExprInModuleWithImports(decl.value, moduleName, moduleNames, importedNames),
        decl.span,
        decl.nameSpan,
      );
    }

    case "SDeclLetRec": {
      // Qualify names with module prefix
      return C.cdeclletrec(
        decl.bindings.map((b) => ({
          name: { id: -1, original: `${moduleName}.${b.name}` },
          value: desugarExprInModuleWithImports(b.value, moduleName, moduleNames, importedNames),
          nameSpan: b.nameSpan,
        })),
        decl.span,
      );
    }

    case "SDeclType": {
      // Qualify constructor names with module prefix
      return C.cdecltype(
        decl.name,
        decl.params,
        decl.constructors.map((c) => ({
          name: `${moduleName}.${c.name}`,
          fields: c.fields.map(desugarType),
          span: c.span,
        })),
        decl.span,
      );
    }

    default:
      // For other declarations (type aliases, etc.), use regular desugaring
      return desugarDecl(decl);
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

  // First pass: collect all module exports
  const moduleExports = collectModuleExports(program.decls);

  const decls: C.CDecl[] = [];
  for (const d of program.decls) {
    const result = desugarDeclWithModuleInfo(d, moduleExports);
    if (result === null) continue;
    if (Array.isArray(result)) {
      decls.push(...result);
    } else {
      decls.push(result);
    }
  }

  const expr = program.expr ? desugarExpr(program.expr) : null;

  return { decls, expr };
};

/**
 * Desugar declaration with module export information.
 */
const desugarDeclWithModuleInfo = (
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

      const bindings: C.CDecl[] = [];
      for (const name of exports) {
        const qualifiedName = `${decl.module}.${name}`;
        // Constructors (uppercase) use CCon, functions (lowercase) use CVar
        const isConstructor = name[0] === name[0]?.toUpperCase();
        bindings.push(
          C.cdecllet(
            { id: -1, original: name },
            isConstructor
              ? C.ccon(qualifiedName, decl.span)
              : C.cvar({ id: -1, original: qualifiedName }, decl.span),
            decl.span,
          ),
        );
      }
      return bindings;
    }
    // Specific imports handled by regular desugarDecl
    return desugarDecl(decl);
  }

  if (decl.kind !== "SDeclModule") {
    return desugarDecl(decl);
  }

  // Operators module is special - don't qualify operator names
  if (decl.name === "Operators") {
    const innerDecls: C.CDecl[] = [];
    for (const d of decl.decls) {
      const result = desugarDecl(d);
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
    const result = desugarDeclInModuleWithImports(d, decl.name, moduleNames, importedNames);
    if (result === null) continue;
    if (Array.isArray(result)) {
      innerDecls.push(...result);
    } else {
      innerDecls.push(result);
    }
  }
  return innerDecls;
};
