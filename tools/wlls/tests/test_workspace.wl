// Test: WLLS_WORKSPACE
// File: tools/wlls/tests/test_workspace.wl
// Focus: wlls document lifecycle and version updates.
import "builtin"
import "../internal/workspace/_pkg.wl" as workspace

func main() -> Int {
    let state -> workspace.Workspace = workspace.Workspace();
    state.open("memory.wl", 1, "first");
    let document -> workspace.Document = state.find("memory.wl");
    if (document is null || document.version != 1 || document.text != "first") {
        builtin.print("FAIL: wlls document open");
        return 1;
    }

    state.open("memory.wl", 2, "second");
    document = state.find("memory.wl");
    if (document is null || document.version != 2 || document.text != "second") {
        builtin.print("FAIL: wlls document change");
        return 1;
    }

    if (!state.close("memory.wl") || state.find("memory.wl") is !null) {
        builtin.print("FAIL: wlls document close");
        return 1;
    }

    builtin.print("PASS: wlls workspace");
    return 0;
}
