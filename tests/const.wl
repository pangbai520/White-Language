import "builtin"
const LANG_NAME->String = "White Language v0.";
const VERSION->Int = 1;

func main() -> Int {
    builtin.print(LANG_NAME + VERSION);
    // LANG_NAME="Python"; // will report an error
    VERSION++; // will report an error
    return 0;
}