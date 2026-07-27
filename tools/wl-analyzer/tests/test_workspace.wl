// Test: ANALYZER_WORKSPACE
// File: tools/wl-analyzer/tests/test_workspace.wl
// Focus: analyzer document lifecycle and version updates.
import "builtin"
import "../internal/workspace/_pkg.wl" as workspace

func main() -> Int {
    let state -> workspace.Workspace = workspace.Workspace();
    state.open("memory.wl", 1, "first");
    let document -> workspace.Document = state.find("memory.wl");
    if (document is null || document.version != 1 || document.text != "first") {
        builtin.print("FAIL: analyzer document open");
        return 1;
    }

    state.open("memory.wl", 2, "second");
    document = state.find("memory.wl");
    if (document is null || document.version != 2 || document.text != "second") {
        builtin.print("FAIL: analyzer document change");
        return 1;
    }

    if (!state.close("memory.wl") || state.find("memory.wl") is !null) {
        builtin.print("FAIL: analyzer document close");
        return 1;
    }

    builtin.print("PASS: analyzer workspace");
    return 0;
}
