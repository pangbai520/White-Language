// Test: STRING_SLICE_REQUIRES_REF
// File: tests/diagnostics/failures/test_slice_missing_ref.wl
// Focus: Enforcement of the 'ref' keyword requirement.
// Expected Error: " InvalidSyntax: Expected 'ref' before slice '[:]'. "

func main() -> Int {
    let value -> String = "WhiteLang";
    let invalid -> String = value[:];
    return 0;
}
