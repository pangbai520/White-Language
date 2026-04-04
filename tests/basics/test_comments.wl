// Test: COMMENT_STRIPPING
// File: tests/basics/test_comments.wl
// Focus: Ensuring comments (single/multi/URL) do not interfere with execution.
import "builtin"

/*
  White Language: Professional Test Case
  This block should be entirely ignored by the parser.
*/

// https://xn--vfv958a.com

func main() -> Int {
    builtin.print("PASS: Lexer comment stripping");
    return 0;
}