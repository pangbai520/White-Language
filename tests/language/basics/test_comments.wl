// Test: COMMENT_STRIPPING
// File: tests/language/basics/test_comments.wl
// Focus: Ensuring comments (single/multi/URL) do not interfere with execution.
import "builtin"


/*
  White Language: Professional Test Case
  This block should be entirely ignored by the parser.
*/

// https://www.white-lang.org

func main() -> Int {
    builtin.print("PASS: Lexer comment stripping");
    return 0;
}
