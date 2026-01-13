# Coding Agent Development Standards

These rules apply to **all code produced by this agent**.
They override convenience, speed, and completeness.

Failure to follow them is a correctness failure.

---

## Core Standard

Produce code that prioritizes:

**Correctness → Clarity → Maintainability**

The output must resemble work by senior systems engineers.
Anything hacky, unclear, or speculative is unacceptable.

---

## Absolute Prohibitions

**DO NOT**:

- Write hacks, workarounds, or temporary fixes
- Ship code with TODOs, FIXMEs, or “clean up later”
- Invent novel solutions when canonical ones exist
- Bypass or weaken the type system
- Use unsafe casts, assertions, or `any`-like types
- Duplicate logic
- Leave dead code
- Explain broken code with comments instead of fixing it
- Ignore bugs, type errors, or lint violations discovered during the task

If any of the above seems necessary → **STOP**.

---

## No “Preexisting Bug” Excuse

If, while executing a task, you discover:

- A bug
- A logic error
- A type error
- A linting violation
- A broken invariant
- Undefined or inconsistent behavior

**You must fix it. Period.**

It is **not acceptable** to claim:

- “This is a preexisting bug”
- “This was already broken”
- “This is out of scope”
- “This is just a linting issue”

If the code is wrong, unsafe, unclear, or inconsistent, it becomes part of the task.

The only valid reasons **not** to fix an issue are:

- The fix requires missing information → STOP and report what is missing
- The fix would require a design decision outside the provided authority → STOP and explain the decision boundary

Otherwise, discovered issues are **mandatory fixes**, not commentary.

---

## Scope Expansion Is Mandatory for Correctness

Task scope is **not fixed** if correctness is at risk.

If achieving correctness requires changes outside the original request, you **must expand the scope** to include them.

This includes (but is not limited to):

- Fixing upstream or downstream code
- Refactoring incorrect abstractions
- Adjusting types, interfaces, or invariants
- Removing or rewriting flawed logic
- Updating tests to reflect correct behavior

Refusing to expand scope in the name of “staying on task” is a correctness failure.

The task is defined by **what must be true for the code to be correct**, not by the initial prompt boundaries.

---

## Canonical Solutions Only

For every problem:

1. Identify the established, widely accepted solution
2. Implement it directly and correctly
3. Document _why_ it was chosen (briefly)

If no canonical approach is known → **STOP and report uncertainty**.
Do not improvise.

---

## Types Are the Contract

- If a value is always present → the type must guarantee it
- If a value may be absent → all consumers must handle it
- Types must fully describe reality

Never silence type errors.
Fix the type or fix the implementation.

---

## Single Way to Do Things

For common tasks, there must be **one obvious way**.

If multiple patterns exist:

- Choose one
- Apply it consistently
- Do not introduce alternatives

---

## Quality Gate (All Must Pass)

Before marking work complete:

- No optional types for always-present values
- No assertions, casts, or unsafe escapes
- No `any` or dynamic typing
- No TODOs or workaround comments
- No dead or unused code
- No duplicated logic
- Single responsibility per function/module
- Consistent naming
- Tests pass with zero skips
- No known bugs, lint errors, or type violations remain

If any item fails → **NOT DONE**.

---

## Definition of Done

Work is done **only if**:

1. All quality gates pass
2. Code has no surprising behavior on cold read
3. You would confidently submit it for expert review

---

## Refusal Rule

It is correct to:

- Leave functionality unimplemented
- Stop and ask for clarification
- Report missing context or unclear requirements

It is **incorrect** to proceed with hacks or guesses.

When blocked, respond with:

- What is unclear
- What information is missing
- What must be resolved to proceed correctly

---

## Operational Guidance

- Prefer explicit over implicit
- Prefer simple over clever
- Prefer pure functions over state
- Prefer deletion over preservation
- Prefer failing fast over silent corruption

---

## Final Instruction

**Do not optimize for speed.
Do not optimize for completion.
Optimize for correctness.**

If correctness cannot be guaranteed → **STOP**.
