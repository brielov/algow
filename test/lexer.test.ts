import { describe, expect, it } from "bun:test";
import { createLexer, nextToken, peek, peekAt, advance, slice, TokenKind } from "../src/lexer";

describe("Lexer", () => {
  describe("createLexer", () => {
    it("creates a lexer state from source", () => {
      const state = createLexer("hello");
      expect(state.source).toBe("hello");
      expect(state.pos).toBe(0);
      expect(state.atLineStart).toBe(true);
    });

    it("handles empty source", () => {
      const state = createLexer("");
      expect(state.source).toBe("");
      expect(state.pos).toBe(0);
    });
  });

  describe("peek and peekAt", () => {
    it("peeks current character", () => {
      const state = createLexer("abc");
      expect(peek(state)).toBe(0x61); // 'a'
    });

    it("returns -1 for EOF", () => {
      const state = createLexer("");
      expect(peek(state)).toBe(-1);
    });

    it("peekAt returns character at offset", () => {
      const state = createLexer("abc");
      expect(peekAt(state, 0)).toBe(0x61); // 'a'
      expect(peekAt(state, 1)).toBe(0x62); // 'b'
      expect(peekAt(state, 2)).toBe(0x63); // 'c'
      expect(peekAt(state, 3)).toBe(-1); // EOF
    });
  });

  describe("advance", () => {
    it("advances the position", () => {
      const state = createLexer("abc");
      expect(state.pos).toBe(0);
      advance(state);
      expect(state.pos).toBe(1);
      advance(state);
      expect(state.pos).toBe(2);
    });
  });

  describe("slice", () => {
    it("extracts text from source", () => {
      const state = createLexer("hello world");
      expect(slice(state, 0, 5)).toBe("hello");
      expect(slice(state, 6, 11)).toBe("world");
    });
  });

  describe("number literals", () => {
    it("lexes integers", () => {
      const state = createLexer("42");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
      expect(slice(state, tok[1], tok[2])).toBe("42");
    });

    it("lexes multiple integers", () => {
      const state = createLexer("42 123 0");
      let tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
      expect(slice(state, tok[1], tok[2])).toBe("42");

      tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
      expect(slice(state, tok[1], tok[2])).toBe("123");

      tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
      expect(slice(state, tok[1], tok[2])).toBe("0");
    });

    it("lexes floating point numbers", () => {
      const state = createLexer("3.14");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Float);
      expect(slice(state, tok[1], tok[2])).toBe("3.14");
    });

    it("lexes numbers with multiple decimal places", () => {
      const state = createLexer("123.456789");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Float);
      expect(slice(state, tok[1], tok[2])).toBe("123.456789");
    });

    it("lexes integer followed by dot as separate tokens", () => {
      const state = createLexer("42.x");
      let tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
      expect(slice(state, tok[1], tok[2])).toBe("42");

      tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Dot);

      tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Lower);
    });
  });

  describe("string literals", () => {
    it("lexes simple strings", () => {
      const state = createLexer('"hello"');
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.String);
      expect(slice(state, tok[1], tok[2])).toBe('"hello"');
    });

    it("lexes empty strings", () => {
      const state = createLexer('""');
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.String);
      expect(slice(state, tok[1], tok[2])).toBe('""');
    });

    it("lexes strings with escape sequences", () => {
      const state = createLexer('"hello\\nworld"');
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.String);
      expect(slice(state, tok[1], tok[2])).toBe('"hello\\nworld"');
    });

    it("lexes strings with escaped quotes", () => {
      const state = createLexer('"say \\"hi\\""');
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.String);
      expect(slice(state, tok[1], tok[2])).toBe('"say \\"hi\\""');
    });

    it("lexes strings with escaped backslash", () => {
      const state = createLexer('"path\\\\to\\\\file"');
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.String);
    });

    it("returns error for unterminated string", () => {
      const state = createLexer('"hello');
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Error);
    });

    it("lexes multiple strings", () => {
      const state = createLexer('"hello" "world"');
      let tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.String);
      expect(slice(state, tok[1], tok[2])).toBe('"hello"');

      tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.String);
      expect(slice(state, tok[1], tok[2])).toBe('"world"');
    });
  });

  describe("character literals", () => {
    it("lexes simple character", () => {
      const state = createLexer("'a'");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Char);
      expect(slice(state, tok[1], tok[2])).toBe("'a'");
    });

    it("lexes digit character", () => {
      const state = createLexer("'0'");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Char);
      expect(slice(state, tok[1], tok[2])).toBe("'0'");
    });

    it("lexes space character", () => {
      const state = createLexer("' '");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Char);
      expect(slice(state, tok[1], tok[2])).toBe("' '");
    });

    it("lexes escaped newline", () => {
      const state = createLexer("'\\n'");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Char);
      expect(slice(state, tok[1], tok[2])).toBe("'\\n'");
    });

    it("lexes escaped tab", () => {
      const state = createLexer("'\\t'");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Char);
      expect(slice(state, tok[1], tok[2])).toBe("'\\t'");
    });

    it("lexes escaped single quote", () => {
      const state = createLexer("'\\''");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Char);
      expect(slice(state, tok[1], tok[2])).toBe("'\\''");
    });

    it("lexes escaped backslash", () => {
      const state = createLexer("'\\\\'");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Char);
      expect(slice(state, tok[1], tok[2])).toBe("'\\\\'");
    });

    it("returns error for unterminated char", () => {
      const state = createLexer("'a");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Error);
    });

    it("returns error for empty char literal", () => {
      const state = createLexer("''");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Error);
    });

    it("returns error for multi-character literal", () => {
      const state = createLexer("'ab'");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Error);
    });

    it("lexes multiple char literals", () => {
      const state = createLexer("'a' 'b' 'c'");
      let tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Char);
      expect(slice(state, tok[1], tok[2])).toBe("'a'");

      tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Char);
      expect(slice(state, tok[1], tok[2])).toBe("'b'");

      tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Char);
      expect(slice(state, tok[1], tok[2])).toBe("'c'");
    });
  });

  describe("identifiers", () => {
    it("lexes lowercase identifiers", () => {
      const state = createLexer("foo");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Lower);
      expect(slice(state, tok[1], tok[2])).toBe("foo");
    });

    it("lexes identifiers with underscores", () => {
      const state = createLexer("foo_bar");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Lower);
      expect(slice(state, tok[1], tok[2])).toBe("foo_bar");
    });

    it("lexes identifiers starting with underscore", () => {
      const state = createLexer("_foo");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Lower);
      expect(slice(state, tok[1], tok[2])).toBe("_foo");
    });

    it("lexes identifiers with digits", () => {
      const state = createLexer("foo123");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Lower);
      expect(slice(state, tok[1], tok[2])).toBe("foo123");
    });

    it("lexes uppercase identifiers (constructors)", () => {
      const state = createLexer("Foo");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Upper);
      expect(slice(state, tok[1], tok[2])).toBe("Foo");
    });

    it("lexes uppercase identifiers with mixed case", () => {
      const state = createLexer("FooBar123");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Upper);
      expect(slice(state, tok[1], tok[2])).toBe("FooBar123");
    });
  });

  describe("keywords", () => {
    it("lexes 'let' keyword", () => {
      const state = createLexer("let");
      expect(nextToken(state)[0]).toBe(TokenKind.Let);
    });

    it("lexes 'rec' keyword", () => {
      const state = createLexer("rec");
      expect(nextToken(state)[0]).toBe(TokenKind.Rec);
    });

    it("lexes 'in' keyword", () => {
      const state = createLexer("in");
      expect(nextToken(state)[0]).toBe(TokenKind.In);
    });

    it("lexes 'if' keyword", () => {
      const state = createLexer("if");
      expect(nextToken(state)[0]).toBe(TokenKind.If);
    });

    it("lexes 'then' keyword", () => {
      const state = createLexer("then");
      expect(nextToken(state)[0]).toBe(TokenKind.Then);
    });

    it("lexes 'else' keyword", () => {
      const state = createLexer("else");
      expect(nextToken(state)[0]).toBe(TokenKind.Else);
    });

    it("lexes 'match' keyword", () => {
      const state = createLexer("match");
      expect(nextToken(state)[0]).toBe(TokenKind.Match);
    });

    it("lexes 'end' keyword", () => {
      const state = createLexer("end");
      expect(nextToken(state)[0]).toBe(TokenKind.End);
    });

    it("lexes 'type' keyword", () => {
      const state = createLexer("type");
      expect(nextToken(state)[0]).toBe(TokenKind.Type);
    });

    it("lexes 'when' keyword", () => {
      const state = createLexer("when");
      expect(nextToken(state)[0]).toBe(TokenKind.When);
    });

    it("lexes 'true' keyword", () => {
      const state = createLexer("true");
      expect(nextToken(state)[0]).toBe(TokenKind.True);
    });

    it("lexes 'false' keyword", () => {
      const state = createLexer("false");
      expect(nextToken(state)[0]).toBe(TokenKind.False);
    });

    it("distinguishes keywords from identifiers", () => {
      const state = createLexer("let letter");
      expect(nextToken(state)[0]).toBe(TokenKind.Let);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
    });

    it("treats keyword-like prefixes as identifiers", () => {
      const state = createLexer("lettuce");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Lower);
      expect(slice(state, tok[1], tok[2])).toBe("lettuce");
    });

    it("lexes 'foreign' keyword", () => {
      const state = createLexer("foreign");
      expect(nextToken(state)[0]).toBe(TokenKind.Foreign);
    });

    it("distinguishes 'foreign' from similar identifiers", () => {
      const state = createLexer("foreign foreigner");
      expect(nextToken(state)[0]).toBe(TokenKind.Foreign);
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Lower);
      expect(slice(state, tok[1], tok[2])).toBe("foreigner");
    });
  });

  describe("operators", () => {
    it("lexes arithmetic operators", () => {
      const state = createLexer("+ - * /");
      expect(nextToken(state)[0]).toBe(TokenKind.Plus);
      expect(nextToken(state)[0]).toBe(TokenKind.Minus);
      expect(nextToken(state)[0]).toBe(TokenKind.Star);
      expect(nextToken(state)[0]).toBe(TokenKind.Slash);
    });

    it("lexes comparison operators", () => {
      const state = createLexer("< <= > >=");
      expect(nextToken(state)[0]).toBe(TokenKind.Lt);
      expect(nextToken(state)[0]).toBe(TokenKind.Le);
      expect(nextToken(state)[0]).toBe(TokenKind.Gt);
      expect(nextToken(state)[0]).toBe(TokenKind.Ge);
    });

    it("lexes equality operators", () => {
      const state = createLexer("== !=");
      expect(nextToken(state)[0]).toBe(TokenKind.EqEq);
      expect(nextToken(state)[0]).toBe(TokenKind.Ne);
    });

    it("lexes pipe operator", () => {
      const state = createLexer("|>");
      expect(nextToken(state)[0]).toBe(TokenKind.Pipe);
    });

    it("lexes arrow operator", () => {
      const state = createLexer("->");
      expect(nextToken(state)[0]).toBe(TokenKind.Arrow);
    });

    it("lexes single equals", () => {
      const state = createLexer("=");
      expect(nextToken(state)[0]).toBe(TokenKind.Eq);
    });

    it("lexes bar", () => {
      const state = createLexer("|");
      expect(nextToken(state)[0]).toBe(TokenKind.Bar);
    });

    it("lexes logical and operator", () => {
      const state = createLexer("&&");
      expect(nextToken(state)[0]).toBe(TokenKind.And);
    });

    it("lexes two pluses as two tokens", () => {
      const state = createLexer("++");
      expect(nextToken(state)[0]).toBe(TokenKind.Plus);
      expect(nextToken(state)[0]).toBe(TokenKind.Plus);
    });

    it("handles sequences of pluses", () => {
      const state = createLexer("+ ++ +");
      expect(nextToken(state)[0]).toBe(TokenKind.Plus);
      expect(nextToken(state)[0]).toBe(TokenKind.Plus);
      expect(nextToken(state)[0]).toBe(TokenKind.Plus);
      expect(nextToken(state)[0]).toBe(TokenKind.Plus);
    });

    it("lexes logical or operator", () => {
      const state = createLexer("||");
      expect(nextToken(state)[0]).toBe(TokenKind.Or);
    });

    it("lexes standalone ampersand as error", () => {
      const state = createLexer("&");
      expect(nextToken(state)[0]).toBe(TokenKind.Error);
    });

    it("distinguishes | from ||", () => {
      const state = createLexer("| || |");
      expect(nextToken(state)[0]).toBe(TokenKind.Bar);
      expect(nextToken(state)[0]).toBe(TokenKind.Or);
      expect(nextToken(state)[0]).toBe(TokenKind.Bar);
    });

    it("lexes punctuation", () => {
      const state = createLexer(", . _");
      expect(nextToken(state)[0]).toBe(TokenKind.Comma);
      expect(nextToken(state)[0]).toBe(TokenKind.Dot);
      expect(nextToken(state)[0]).toBe(TokenKind.Underscore);
    });

    it("lexes standalone bang as error", () => {
      const state = createLexer("!");
      expect(nextToken(state)[0]).toBe(TokenKind.Error);
    });
  });

  describe("delimiters", () => {
    it("lexes parentheses", () => {
      const state = createLexer("()");
      expect(nextToken(state)[0]).toBe(TokenKind.LParen);
      expect(nextToken(state)[0]).toBe(TokenKind.RParen);
    });

    it("lexes braces", () => {
      const state = createLexer("{}");
      expect(nextToken(state)[0]).toBe(TokenKind.LBrace);
      expect(nextToken(state)[0]).toBe(TokenKind.RBrace);
    });

    it("lexes brackets", () => {
      const state = createLexer("[]");
      expect(nextToken(state)[0]).toBe(TokenKind.LBracket);
      expect(nextToken(state)[0]).toBe(TokenKind.RBracket);
    });
  });

  describe("whitespace handling", () => {
    it("skips spaces", () => {
      const state = createLexer("   42");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
    });

    it("skips tabs", () => {
      const state = createLexer("\t\t42");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
    });

    it("handles newlines and sets atLineStart", () => {
      const state = createLexer("42\n43");
      nextToken(state);
      expect(state.atLineStart).toBe(false);
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
      expect(slice(state, tok[1], tok[2])).toBe("43");
    });

    it("handles carriage returns", () => {
      const state = createLexer("42\r43");
      nextToken(state);
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
      expect(slice(state, tok[1], tok[2])).toBe("43");
    });

    it("handles mixed whitespace", () => {
      const state = createLexer("  \t\n\r  42");
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
    });
  });

  describe("comments", () => {
    it("skips line comments", () => {
      const state = createLexer("42 -- this is a comment\n43");
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
      expect(slice(state, tok[1], tok[2])).toBe("43");
    });

    it("handles line comment at end of file", () => {
      const state = createLexer("42 -- comment");
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      expect(nextToken(state)[0]).toBe(TokenKind.Eof);
    });

    it("handles empty line comment", () => {
      const state = createLexer("42 --\n43");
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
    });

    it("skips block comments", () => {
      const state = createLexer("42 {- comment -} 43");
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
      expect(slice(state, tok[1], tok[2])).toBe("43");
    });

    it("handles nested block comments", () => {
      const state = createLexer("42 {- outer {- inner -} outer -} 43");
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
      expect(slice(state, tok[1], tok[2])).toBe("43");
    });

    it("handles multiline block comments", () => {
      const state = createLexer("42 {-\nmultiline\ncomment\n-} 43");
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
    });

    it("handles deeply nested block comments", () => {
      const state = createLexer("42 {- a {- b {- c -} b -} a -} 43");
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Int);
      expect(slice(state, tok[1], tok[2])).toBe("43");
    });

    it("handles unclosed block comment at EOF", () => {
      const state = createLexer("42 {- unclosed");
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      expect(nextToken(state)[0]).toBe(TokenKind.Eof);
    });
  });

  describe("EOF handling", () => {
    it("returns EOF for empty input", () => {
      const state = createLexer("");
      expect(nextToken(state)[0]).toBe(TokenKind.Eof);
    });

    it("returns EOF after all tokens", () => {
      const state = createLexer("42");
      nextToken(state);
      expect(nextToken(state)[0]).toBe(TokenKind.Eof);
    });

    it("returns EOF repeatedly", () => {
      const state = createLexer("");
      expect(nextToken(state)[0]).toBe(TokenKind.Eof);
      expect(nextToken(state)[0]).toBe(TokenKind.Eof);
      expect(nextToken(state)[0]).toBe(TokenKind.Eof);
    });

    it("EOF token has same start and end", () => {
      const state = createLexer("42");
      nextToken(state);
      const eof = nextToken(state);
      expect(eof[1]).toBe(eof[2]);
    });
  });

  describe("complex token sequences", () => {
    it("lexes a simple expression", () => {
      const state = createLexer("x + y * 2");
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.Plus);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.Star);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
    });

    it("lexes a let expression", () => {
      const state = createLexer("let x = 42 in x + 1");
      expect(nextToken(state)[0]).toBe(TokenKind.Let);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.Eq);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      expect(nextToken(state)[0]).toBe(TokenKind.In);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.Plus);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
    });

    it("lexes a lambda expression", () => {
      const state = createLexer("x -> x + 1");
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.Arrow);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.Plus);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
    });

    it("lexes a match expression", () => {
      const state = createLexer("match x when Just y -> y when Nothing -> 0 end");
      expect(nextToken(state)[0]).toBe(TokenKind.Match);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.When);
      expect(nextToken(state)[0]).toBe(TokenKind.Upper);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.Arrow);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.When);
      expect(nextToken(state)[0]).toBe(TokenKind.Upper);
      expect(nextToken(state)[0]).toBe(TokenKind.Arrow);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      expect(nextToken(state)[0]).toBe(TokenKind.End);
    });

    it("lexes a type declaration", () => {
      const state = createLexer("type Maybe a = Nothing | Just a");
      expect(nextToken(state)[0]).toBe(TokenKind.Type);
      expect(nextToken(state)[0]).toBe(TokenKind.Upper);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.Eq);
      expect(nextToken(state)[0]).toBe(TokenKind.Upper);
      expect(nextToken(state)[0]).toBe(TokenKind.Bar);
      expect(nextToken(state)[0]).toBe(TokenKind.Upper);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
    });

    it("lexes record syntax", () => {
      const state = createLexer("{ x = 1, y = 2 }");
      expect(nextToken(state)[0]).toBe(TokenKind.LBrace);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.Eq);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      expect(nextToken(state)[0]).toBe(TokenKind.Comma);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.Eq);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      expect(nextToken(state)[0]).toBe(TokenKind.RBrace);
    });

    it("lexes tuple syntax", () => {
      const state = createLexer("(1, 2, 3)");
      expect(nextToken(state)[0]).toBe(TokenKind.LParen);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      expect(nextToken(state)[0]).toBe(TokenKind.Comma);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      expect(nextToken(state)[0]).toBe(TokenKind.Comma);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      expect(nextToken(state)[0]).toBe(TokenKind.RParen);
    });

    it("lexes field access", () => {
      const state = createLexer("record.field");
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.Dot);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
    });

    it("lexes pipe operator chain", () => {
      const state = createLexer("x |> f |> g");
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.Pipe);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
      expect(nextToken(state)[0]).toBe(TokenKind.Pipe);
      expect(nextToken(state)[0]).toBe(TokenKind.Lower);
    });
  });

  describe("edge cases", () => {
    it("handles operators without spaces", () => {
      const state = createLexer("1+2*3");
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      expect(nextToken(state)[0]).toBe(TokenKind.Plus);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
      expect(nextToken(state)[0]).toBe(TokenKind.Star);
      expect(nextToken(state)[0]).toBe(TokenKind.Int);
    });

    it("handles consecutive operators", () => {
      const state = createLexer("<= >=");
      expect(nextToken(state)[0]).toBe(TokenKind.Le);
      expect(nextToken(state)[0]).toBe(TokenKind.Ge);
    });

    it("distinguishes similar operators", () => {
      const state = createLexer("< <= = == -> |>");
      expect(nextToken(state)[0]).toBe(TokenKind.Lt);
      expect(nextToken(state)[0]).toBe(TokenKind.Le);
      expect(nextToken(state)[0]).toBe(TokenKind.Eq);
      expect(nextToken(state)[0]).toBe(TokenKind.EqEq);
      expect(nextToken(state)[0]).toBe(TokenKind.Arrow);
      expect(nextToken(state)[0]).toBe(TokenKind.Pipe);
    });

    it("handles underscore as wildcard vs identifier prefix", () => {
      const state = createLexer("_ _x");
      expect(nextToken(state)[0]).toBe(TokenKind.Underscore);
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Lower);
      expect(slice(state, tok[1], tok[2])).toBe("_x");
    });

    it("handles numbers at boundaries", () => {
      const state = createLexer("0 999999999");
      let tok = nextToken(state);
      expect(slice(state, tok[1], tok[2])).toBe("0");
      tok = nextToken(state);
      expect(slice(state, tok[1], tok[2])).toBe("999999999");
    });

    it("handles very long identifiers", () => {
      const longIdent = "a".repeat(100);
      const state = createLexer(longIdent);
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.Lower);
      expect(slice(state, tok[1], tok[2])).toBe(longIdent);
    });

    it("handles unicode in strings", () => {
      const state = createLexer('"hello 世界"');
      const tok = nextToken(state);
      expect(tok[0]).toBe(TokenKind.String);
      expect(slice(state, tok[1], tok[2])).toBe('"hello 世界"');
    });
  });

  describe("token position tracking", () => {
    it("tracks positions correctly for single tokens", () => {
      const state = createLexer("foo");
      const tok = nextToken(state);
      expect(tok[1]).toBe(0); // start
      expect(tok[2]).toBe(3); // end
    });

    it("tracks positions with leading whitespace", () => {
      const state = createLexer("   foo");
      const tok = nextToken(state);
      expect(tok[1]).toBe(3); // start after whitespace
      expect(tok[2]).toBe(6); // end
    });

    it("tracks positions for multiple tokens", () => {
      const state = createLexer("a + b");
      let tok = nextToken(state);
      expect(tok[1]).toBe(0);
      expect(tok[2]).toBe(1);

      tok = nextToken(state);
      expect(tok[1]).toBe(2);
      expect(tok[2]).toBe(3);

      tok = nextToken(state);
      expect(tok[1]).toBe(4);
      expect(tok[2]).toBe(5);
    });

    it("tracks positions for multi-character operators", () => {
      const state = createLexer("==");
      const tok = nextToken(state);
      expect(tok[1]).toBe(0);
      expect(tok[2]).toBe(2);
    });
  });
});
