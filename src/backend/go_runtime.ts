/**
 * Go Runtime for Algow
 *
 * Defines the runtime representation of values and helper functions
 * needed by generated Go code.
 *
 * Value representations:
 * - All values are represented as interface{} (any)
 * - Primitives: int64, string, bool
 * - Functions: Func interface with Apply method
 * - Constructors: Con struct with Tag and Args
 * - Tuples: []any slice
 * - Records: map[string]any
 */

export const GO_RUNTIME = `package main

import (
	"fmt"
	"reflect"
)

// Value is the universal value type
type Value = any

// Func represents a callable function
type Func interface {
	Apply(arg Value) Value
}

// Con represents a constructor value
type Con struct {
	Tag  string
	Args []Value
}

// Apply implements partial application for constructors
func (c Con) Apply(arg Value) Value {
	newArgs := make([]Value, len(c.Args)+1)
	copy(newArgs, c.Args)
	newArgs[len(c.Args)] = arg
	return Con{Tag: c.Tag, Args: newArgs}
}

// Closure represents a function with captured environment
type Closure struct {
	Fn  func(env []Value, arg Value) Value
	Env []Value
}

// Apply calls the closure with its environment
func (c Closure) Apply(arg Value) Value {
	return c.Fn(c.Env, arg)
}

// PureFunc represents a function without captured variables
type PureFunc struct {
	Fn func(arg Value) Value
}

// Apply calls the pure function
func (f PureFunc) Apply(arg Value) Value {
	return f.Fn(arg)
}

// Apply applies a function to an argument
func Apply(fn Value, arg Value) Value {
	switch f := fn.(type) {
	case Func:
		return f.Apply(arg)
	case func(Value) Value:
		return f(arg)
	default:
		panic(fmt.Sprintf("Cannot apply non-function: %T", fn))
	}
}

// NewCon creates a new constructor value
func NewCon(tag string, args ...Value) Value {
	return Con{Tag: tag, Args: args}
}

// eqInternal performs deep structural equality (returns bool for internal use)
func eqInternal(a, b Value) bool {
	// Same reference
	if a == b {
		return true
	}

	// Handle nil
	if a == nil || b == nil {
		return a == nil && b == nil
	}

	// Constructor equality
	if ca, ok := a.(Con); ok {
		if cb, ok := b.(Con); ok {
			if ca.Tag != cb.Tag {
				return false
			}
			if len(ca.Args) != len(cb.Args) {
				return false
			}
			for i := range ca.Args {
				if !eqInternal(ca.Args[i], cb.Args[i]) {
					return false
				}
			}
			return true
		}
		return false
	}

	// Slice (tuple) equality
	if sa, ok := a.([]Value); ok {
		if sb, ok := b.([]Value); ok {
			if len(sa) != len(sb) {
				return false
			}
			for i := range sa {
				if !eqInternal(sa[i], sb[i]) {
					return false
				}
			}
			return true
		}
		return false
	}

	// Map (record) equality
	if ma, ok := a.(map[string]Value); ok {
		if mb, ok := b.(map[string]Value); ok {
			if len(ma) != len(mb) {
				return false
			}
			for k, v := range ma {
				if bv, ok := mb[k]; !ok || !eqInternal(v, bv) {
					return false
				}
			}
			return true
		}
		return false
	}

	// Use reflect for other types
	return reflect.DeepEqual(a, b)
}

// Eq performs deep structural equality (returns Value for use in generated code)
func Eq(a, b Value) Value {
	return eqInternal(a, b)
}

// Helper to convert bool to int for comparisons
func boolToInt(b bool) int64 {
	if b {
		return 1
	}
	return 0
}

`;
